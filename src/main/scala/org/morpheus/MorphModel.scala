package org.morpheus

import shapeless.HList

import scala.reflect.runtime.universe._

/**
 * Created by zslajchrt on 29/04/15.
 */
@deprecated
trait MorphingTools[M] {

  def activator(activator: PartialFunction[Frag[_, _], Boolean]) = new FragmentSelector[M](activator)

  def left = new LeftAltsMorphingStrategy[M]()

  def right = new RightAltsMorphingStrategy[M]()

  def ?[F: WeakTypeTag](activator: Frag[_, _] => Boolean): PartialFunction[Frag[_, _], Boolean] = FragmentSelector.?[F](activator)

}

abstract class MorphModelBase[M](val rootNode: MorphModelNode) extends MorphingTools[M] {
  val fragmentDescriptorsList: List[Frag[_, _]]
  val lubComponents: Array[Class[_]]
  /**
   * Maps the secondary fragments to the primary ones
   */
  val sec2prim: Map[Int, Int]

  private lazy val frag2Desc: Map[Int, Frag[_, _]] = {
    sec2prim.map(e => {
      val secFragId = e._1
      val primFragId = e._2
      val fragDesc: Frag[_, _] = fragmentDescriptorsList.find(_.index == primFragId).get
      (secFragId, fragDesc)
    }).toMap
//    fragmentDescriptorsList.map(fd => {
//      (fd.index, fd)
//    }).toMap
  }

  def altIterator(): AltIterator[FragmentNode, List[FragmentNode]] =
    new AltIterator[FragmentNode, List[FragmentNode]](rootNode.toAltNode) {
      override protected def mapAlt(alt: List[FragmentNode]): List[FragmentNode] = alt
    }

  def fragmentDescriptor(fragment: FragmentNode): Option[Frag[_, _]] = {
    frag2Desc.get(fragment.id)
  }

  def toString(alternative: List[FragmentNode]): String = {
    alternative.map(fragmentDescriptor(_)).mkString(",")
  }

  override def toString: String = {
    fragmentDescriptorsList.mkString("{", ",", "}")
  }

  lazy val alternatives: Alternatives[M] = Alternatives.apply[M](rootNode, new FragmentsHelper[Frag[_, _], RuntimeClass](frag2Desc, f => f.fragmentClass))

}

abstract class MorphModel[M](rn: MorphModelNode) extends MorphModelBase[M](rn) with MorphingTools[M] {
  outer =>
  type LUB
  type Model = M
  type ConformLevel <: ConformanceLevelMarker
  type MutableLUB = LUB with MutableMorphMirror[M] { type ConfLev = ConformLevel }
  type ImmutableLUB = LUB with MorphMirror[M] { type ConfLev = ConformLevel }
  type Kernel = MorphKernel[Model] { type LUB = outer.LUB; type ConformLevel = TotalConformance }

  val fragmentDescriptors: HList

}

