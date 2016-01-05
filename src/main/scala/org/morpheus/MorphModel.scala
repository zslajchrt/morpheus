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

  def fragmentBitMask(fragmentId: Int): Set[Int] = {
    sec2prim.filter(p => p._2 == fragmentId).keySet
  }

  def altIterator(): AltIterator[FragmentNode, List[FragmentNode]] =
    new AltIterator[FragmentNode, List[FragmentNode]](rootNode.toAltNode) {
      override protected def mapAlt(alt: List[FragmentNode]): List[FragmentNode] = alt
    }

  def fragmentDescriptor(fragmentId: Int): Option[Frag[_, _]] = {
    frag2Desc.get(fragmentId)
  }

  def fragmentDescriptor(fragment: FragmentNode): Option[Frag[_, _]] = {
    frag2Desc.get(fragment.id)
  }

  def fragmentDescriptor[F: WeakTypeTag]: Option[Frag[F, _]] = {
    val fragTpe: WeakTypeTag[F] = implicitly[WeakTypeTag[F]]
    fragmentDescriptorsList.find(fd => fragTpe.tpe =:= fd.fragTag.tpe).asInstanceOf[Option[Frag[F, _]]]
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
  type MutableLUB = LUB with MutableMorphMirror[M] { type ConfLev = ConformLevel; type LUB = outer.LUB }
  type ImmutableLUB = LUB with MorphMirror[M] { type ConfLev = ConformLevel; type LUB = outer.LUB }
  type Kernel = MorphKernel[Model] { type LUB = outer.LUB; type ConformLevel = TotalConformance }
  type Recognizer = Kernel // Just an alias for Kernel

  val fragmentDescriptors: HList

  trait Strategy extends MorphingStrategy[Model] {

    override def chooseAlternatives(instance: MorphKernel[MorphModel.this.Model])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[MorphModel.this.Model] = {
      val fixedInst = instance.asInstanceOf[Recognizer]
      chooseAlts(fixedInst)(morphProxy.asInstanceOf[Option[fixedInst.ImmutableLUB]])
    }

    def chooseAlts(instance: Recognizer)(morphProxy: Option[instance.ImmutableLUB]): Alternatives[instance.Model]
  }

}

