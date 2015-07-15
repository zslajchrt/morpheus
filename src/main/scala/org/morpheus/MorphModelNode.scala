package org.morpheus

import java.util.concurrent.atomic.AtomicInteger

/**
 * Created by zslajchrt on 29/04/15.
 */
sealed trait MorphModelNode {

  def ++(other: MorphModelNode): MorphModelNode = {

    val idCounter = new AtomicInteger()

    def renumberFragments(node: MorphModelNode): MorphModelNode = node match {
      case DisjNode(children) => DisjNode(children.map(renumberFragments(_)))
      case ConjNode(children) => ConjNode(children.map(renumberFragments(_)))
      case FragmentNode(id, _) => FragmentNode(idCounter.getAndIncrement)
      case UnitNode => UnitNode
    }

    renumberFragments(ConjNode(List(this, other))).flatten
  }

  val fragments: List[FragmentNode]

  private def flattenConj(cn: ConjNode): List[MorphModelNode] = {
    cn.children.flatMap({
      case cn2@ConjNode(_) => flattenConj(cn2)
      case other => List(other.flatten)
    })
  }

  private def flattenDisj(cn: DisjNode): List[MorphModelNode] = {
    cn.children.flatMap({
      case cn2@DisjNode(_) => flattenDisj(cn2)
      case other => List(other.flatten)
    })
  }

  def flatten: MorphModelNode = this match {
    case cn@ConjNode(_) => ConjNode(flattenConj(cn))
    case dn@DisjNode(children) => DisjNode(flattenDisj(dn))
    case other => other
  }

  def toAltNode: AltNode[FragmentNode]

  def createAntagonistsMatrix(nodeEquivalence: ((FragmentNode, FragmentNode) => Boolean) = _ == _): Set[(FragmentNode, FragmentNode)] = {
    val equivPairs = for (frag1 <- fragments;
                          frag2 <- fragments if nodeEquivalence(frag1, frag2)) yield (frag1, frag2)
    val equivMap: Map[FragmentNode, List[FragmentNode]] =
      (for (frag <- fragments) yield (frag, equivPairs.filter(_._1 == frag).map(_._2))).toMap

    val altIter = new IdentAltIterator(toAltNode)
    val alts = altIter.toList

    // Create a set of 'friends'. Two fragments are friends if they appear in the same alternative.
    val friends = (for (alt <- alts;
                        i <- 0 until alt.size;
                        j <- 0 until alt.size;
                        altFrag1 <- equivMap(alt(i));
                        altFrag2 <- equivMap(alt(j))
    ) yield (altFrag1, altFrag2)).toSet

    // the antagonist set is complementary to the friends
    (for (frag1 <- fragments;
          frag2 <- fragments if !friends.contains((frag1, frag2))
    ) yield (frag1, frag2)).toSet
  }

}

//case class ConjNode(children: List[MorphModelNode]) extends MorphModelNode
case class ConjNode(children: List[MorphModelNode]) extends MorphModelNode {
  def toAltNode: AltNode[FragmentNode] = {
    SeqAltNode[FragmentNode](children.map(_.toAltNode))
  }

  override val fragments: List[FragmentNode] = children.flatMap(_.fragments)
}

case class DisjNode(children: List[MorphModelNode]) extends MorphModelNode {
  def toAltNode: AltNode[FragmentNode] = {
    ChoiceAltNode[FragmentNode](children.map(_.toAltNode))
  }

  override val fragments: List[FragmentNode] = children.flatMap(_.fragments)
}

case class FragmentNode(id: Int, placeholder: Boolean = false) extends MorphModelNode {

  def toAltNode: AltNode[FragmentNode] = {
    LeafAltNode[FragmentNode](this)
  }

  override val fragments: List[FragmentNode] = List(this)


}

case object UnitNode extends MorphModelNode {
  def isUnit(tpName: String) = tpName == "scala.Unit"

  def toAltNode: AltNode[FragmentNode] = NoneAltNode

  override val fragments: List[FragmentNode] = Nil
}
