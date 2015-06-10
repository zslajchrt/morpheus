package org.morpheus

import scala.collection.BitSet

/**
 *
 * Created by zslajchrt on 19/05/15.
 */

case class AltRating(rating: Double, fragmentBits: BitSet)

class Alternatives[M] private (private [morpheus] val rootNode: MorphModelNode, private [morpheus] val ratedAlts: Map[List[FragmentNode], AltRating]) {

  private [morpheus] def filter(f: (List[FragmentNode], Double) => Boolean): Alternatives[M] = {
    val newRatedAlts = ratedAlts.filter(e => f(e._1, e._2.rating))
    new Alternatives[M](rootNode, newRatedAlts)
  }

  def rate(ratingFn: (List[FragmentNode], Double) => Double): Alternatives[M] = {
    val altIter = new IdentAltIterator(rootNode.toAltNode)
    var newRatedAlts = Map.empty[List[FragmentNode], AltRating]
    while (altIter.hasNext) {
      val alt: List[FragmentNode] = altIter.next()
      ratedAlts.get(alt) match {
        case None =>
        case Some(altRating) =>
          newRatedAlts += (alt -> altRating.copy(rating = ratingFn(alt, altRating.rating)))
      }
    }

    new Alternatives[M](rootNode, newRatedAlts)
  }

  private def bitoper(fragments: Set[Int], mask: Boolean): Alternatives[M] = {
    val newRatedAlts = for (ratedAlt <- ratedAlts) yield {
      val fragIntersect: Set[Int] = ratedAlt._1.map(_.id).toSet.intersect(fragments)
      val newMask = if (mask)
        ratedAlt._2.fragmentBits.`|`(fragIntersect)
      else
        ratedAlt._2.fragmentBits.--(fragIntersect)

      ratedAlt._1 -> ratedAlt._2.copy(fragmentBits = newMask)
    }

    new Alternatives[M](rootNode, newRatedAlts)

  }

  def mask(maskedFragments: Set[Int]): Alternatives[M] = {
    bitoper(maskedFragments, true)
  }

  def unmask(unmaskedFragments: Set[Int]): Alternatives[M] = {
    bitoper(unmaskedFragments, false)
  }

  def promote(promotedAlts: Set[List[Int]]): Alternatives[M] = {

    def reorderModel(altNode: AltNode[FragmentNode]): MorphModelNode = altNode match {
      case ch@ChoiceAltNode(children) =>
        val (_, _, currentChild) = ch.findSubCounter(ch.counter.value)
        val reorderedChildren = Alternatives.moveChildToHead(children.map(reorderModel), children.indexOf(currentChild))
        DisjNode(reorderedChildren)
      case SeqAltNode(children) => ConjNode(children.map(reorderModel))
      case LeafAltNode(leaf) => leaf
      case NoneAltNode => UnitNode
    }

    // Search for the first alternative matching any from promotedAlts

    val altIter = new IdentAltIterator(rootNode.toAltNode)
    var newAlternatives: Option[Alternatives[M]] = None
    while (!newAlternatives.isDefined && altIter.hasNext) {
      val alt: List[Int] = altIter.current().map(_.id)
      if (promotedAlts.contains(alt)) {
        // The alt found. Reorder the model so that the iterator produces this alt as the first one.
        val newModelRoot: MorphModelNode = reorderModel(altIter.rootAltNode)
        newAlternatives = Some(new Alternatives[M](newModelRoot, ratedAlts))
      } else {
        altIter.next()
      }
    }

    newAlternatives match {
      case None => this
      case Some(newAlts) => newAlts
    }

  }

  lazy val toMaskedList: List[(List[FragmentNode], Double)] = {
    val maxFragBits = ratedAlts.foldLeft(0)((maxBits, altRating) => if (altRating._2.fragmentBits.size > maxBits)
      altRating._2.fragmentBits.size
    else
      maxBits)

    // We have to use IdentAltIterator to preserve the order of the alternatives
    val altIter = new IdentAltIterator(rootNode.toAltNode)
    val alts = altIter.toList
    for (alt <- alts; r <- ratedAlts.get(alt) if r.fragmentBits.size == maxFragBits) yield (alt, r.rating)

  }

  lazy val toList: List[(List[FragmentNode], Double)] = {
    // We have to use IdentAltIterator to preserve the order of the alternatives
    val altIter = new IdentAltIterator(rootNode.toAltNode)
    val alts = altIter.toList
    for (alt <- alts; r <- ratedAlts.get(alt)) yield (alt, r.rating)
  }

}

object Alternatives {

  private [morpheus] def apply[M](root: MorphModelNode): Alternatives[M] = {
    val altIter = new IdentAltIterator(root.toAltNode)
    val defaultRatedAlts: Map[List[FragmentNode], AltRating] = altIter.toList.map(alt => (alt, AltRating(0d, BitSet.empty))).toMap
    new Alternatives[M](root, defaultRatedAlts)
  }

  def moveChildToHead(children: List[MorphModelNode], index: Int): List[MorphModelNode] = {
    val splitChildren = children.splitAt(index)
    splitChildren._2.head :: (splitChildren._1 ::: splitChildren._2.tail)
  }

}