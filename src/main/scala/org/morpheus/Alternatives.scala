package org.morpheus

/**
 *
 * Created by zslajchrt on 19/05/15.
 */
class Alternatives[M] private (rootNode: MorphModelNode, ratedAlts: Map[List[FragmentNode], Double]) {

  private [morpheus] def filter(f: (List[FragmentNode], Double) => Boolean): Alternatives[M] = {
    val newRatedAlts = ratedAlts.filter(e => f(e._1, e._2))
    new Alternatives[M](rootNode, newRatedAlts)
  }

  def rate(ratingFn: (List[FragmentNode], Double) => Double): Alternatives[M] = {
    val altIter = new IdentAltIterator(rootNode.toAltNode)
    var newRatedAlts = Map.empty[List[FragmentNode], Double]
    while (altIter.hasNext) {
      val alt: List[FragmentNode] = altIter.next()
      ratedAlts.get(alt) match {
        case None =>
        case Some(altRating) =>
          newRatedAlts += (alt -> ratingFn(alt, altRating))
      }
    }

    new Alternatives[M](rootNode, newRatedAlts)
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

  lazy val toList: List[(List[FragmentNode], Double)] = {
    val altIter = new IdentAltIterator(rootNode.toAltNode)
    val alts = altIter.toList
    for (alt <- alts; r <- ratedAlts.get(alt)) yield (alt, r)
  }

}

object Alternatives {

  private [morpheus] def apply[M](root: MorphModelNode): Alternatives[M] = {
    val altIter = new IdentAltIterator(root.toAltNode)
    val defaultRatedAlts: Map[List[FragmentNode], Double] = altIter.toList.map(alt => (alt, 0d)).toMap
    new Alternatives[M](root, defaultRatedAlts)
  }

  def moveChildToHead(children: List[MorphModelNode], index: Int): List[MorphModelNode] = {
    val splitChildren = children.splitAt(index)
    splitChildren._2.head :: (splitChildren._1 ::: splitChildren._2.tail)
  }

}