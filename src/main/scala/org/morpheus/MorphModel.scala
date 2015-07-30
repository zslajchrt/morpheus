package org.morpheus

import shapeless.HList

import scala.reflect.runtime.universe._

/**
 * Created by zslajchrt on 29/04/15.
 */
trait MorphingTools[M] {

  def activator(activator: PartialFunction[Frag[_, _], Boolean]) = new FragmentSelector[M](activator)

  def left = new LeftAltsMorphingStrategy[M]()

  def right = new RightAltsMorphingStrategy[M]()

  def ?[F: WeakTypeTag](activator: Frag[_, _] => Boolean): PartialFunction[Frag[_, _], Boolean] = FragmentSelector.?[F](activator)

}

abstract class MorphModelBase[M](val rootNode: MorphModelNode) extends MorphingTools[M] {
  val fragmentDescriptorsList: List[Frag[_, _]]
  val lubComponents: Array[Class[_]]

  private lazy val frag2Desc: Map[Int, Frag[_, _]] = {
    fragmentDescriptorsList.map(fd => {
      (fd.index, fd)
    }).toMap
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

  val alternatives: Alternatives[M] = Alternatives.apply[M](rootNode)

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

