package org.morpheus

import scala.collection.BitSet

/**
 *
 * Created by zslajchrt on 19/05/15.
 */

case class AltRating(rating: Double)

class Alternatives[M] private (private [morpheus] val rootNode: MorphModelNode,
                               private [morpheus] val ratedAlts: Map[List[FragmentNode], AltRating],
                               private [morpheus] val fragmentMask: BitSet) {

  private [morpheus] def filter(f: (List[FragmentNode], Double) => Boolean): Alternatives[M] = {
    val newRatedAlts = ratedAlts.filter(e => f(e._1, e._2.rating))
    new Alternatives[M](rootNode, newRatedAlts, fragmentMask)
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

    new Alternatives[M](rootNode, newRatedAlts, fragmentMask)
  }

  def maskAll(): Alternatives[M] = {
    new Alternatives[M](rootNode, ratedAlts, BitSet.empty | rootNode.fragments.map(_.id).toSet)
  }

  def unmaskAll(): Alternatives[M] = {
    new Alternatives[M](rootNode, ratedAlts, BitSet.empty)
  }

  def mask(mask: Set[Int]): Alternatives[M] = {
    new Alternatives[M](rootNode, ratedAlts, fragmentMask.`|`(mask))
  }

  def unmask(mask: Set[Int]): Alternatives[M] = {
    new Alternatives[M](rootNode, ratedAlts, fragmentMask.--(mask))
  }

  def promote(promotedAlts: Set[List[Int]]): Alternatives[M] = {

    def reorderModel(altNode: AltNode[FragmentNode]): MorphModelNode = altNode match {
      case ch@ChoiceAltNode(children) =>
        //val (_, currentChild) = ch.findSubCounter(ch.ChoiceCounter.value)
        val currentChild = ch.currentChild
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
        newAlternatives = Some(new Alternatives[M](newModelRoot, ratedAlts, fragmentMask))
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
    val altIter = new IdentAltIterator(rootNode.toAltNode)
    val alts = altIter.toList
    for (alt <- alts;
         r <- ratedAlts.get(alt);
         altBits = BitSet.empty ++ alt.map(_.id)
         if altBits.&(fragmentMask) == altBits) yield (alt, r.rating)
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
    val defaultRatedAlts: Map[List[FragmentNode], AltRating] = altIter.toList.map(alt => (alt, AltRating(0d))).toMap
    new Alternatives[M](root, defaultRatedAlts, BitSet.empty).maskAll()
  }

  def moveChildToHead(children: List[MorphModelNode], index: Int): List[MorphModelNode] = {
    val splitChildren = children.splitAt(index)
    splitChildren._2.head :: (splitChildren._1 ::: splitChildren._2.tail)
  }

}