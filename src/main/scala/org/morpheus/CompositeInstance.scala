package org.morpheus

import shapeless.HList

import scala.reflect.runtime.universe._

/**
 * Created by zslajchrt on 29/04/15.
 */
abstract class CompositeProtoInstance[M](val rootNode: CompositeModelNode) {

  val parent: Option[CompositeProtoInstance[_]]
  val defaultStrategy: MorpherStrategy[M]
  val altComposer: AlternativeComposer[M]

  def fragmentList: List[FragmentHolder[_]]

  val model: CompositeProtoModel[M]

  def fragmentHolder(fragment: FragmentNode): Option[FragmentHolder[_]] = {
    fragmentList.find(fh => fragment.id == fh.fragment.index)
  }

  def fragmentHolder(fragment: Frag[_, _]): Option[FragmentHolder[_]] = {
    val isAbstractFragment = !fragment.fragmentAnnotation.isDefined && !fragment.wrapperAnnotation.isDefined
    fragmentList.find(fh => if (isAbstractFragment) {
      fh.fragment.fragTag.tpe <:< fragment.fragTag.tpe
    } else {
      fragment.fragTag.tpe =:= fh.fragment.fragTag.tpe
    })
  }

  def fragmentHolder[F: WeakTypeTag]: Option[FragmentHolder[F]] = {
    val fragTpe: WeakTypeTag[F] = implicitly[WeakTypeTag[F]]
    fragmentList.find(fh => fragTpe.tpe =:= fh.fragment.fragTag.tpe) match {
      case None => parent match {
        case None => None
        case Some(par) => par.fragmentHolder(fragTpe)
      }
      case Some(holder) => Some(holder.asInstanceOf[FragmentHolder[F]])
    }
  }

  def altIterator(): AltIterator[FragmentNode, List[(FragmentNode, FragmentHolder[_])]] =
    new AltIterator[FragmentNode, List[(FragmentNode, FragmentHolder[_])]](rootNode.toAltNode) {
      override protected def mapAlt(alt: List[FragmentNode]): List[(FragmentNode, FragmentHolder[_])] = {
        alt.map(fn => (fn, fragmentHolder(fn).get))
      }
    }

}

abstract class CompositeInstance[M](val root: CompositeModelNode) extends CompositeProtoInstance[M](root) with MorphingTools[M] {

  outer =>

  //def this(ci: CompositeInstanceRef[M]) = this(ci.instance.rootNode)

  type LUB
  type Model = M
  type ConformLevel <: ConformanceLevelMarker
  type MutableLUB = LUB with MutableCompositeMirror[M, LUB] { type ConfLev = ConformLevel }
  type ImutableLUB = LUB with CompositeMirror[M, LUB] { type ConfLev = ConformLevel }

  val fragmentDescriptors: HList
  val fragments: HList
  val proxies: AnyRef
  val model: CompositeModel[M]

  val lubComponents: Array[Class[_]]

  lazy val ! = make
  lazy val ~ = make_~

  def morph(implicit strategy: MorpherStrategy[M]): ImutableLUB = {
    Morpher.morph[M](this, Some(strategy))(None)
  }

  def make: ImutableLUB = {
    Morpher.morph[M](this, None)(None)
  }

  def morph_~(implicit strategy: MorpherStrategy[M]): MutableLUB = mutableProxy

  def make_~ : MutableLUB = mutableProxy_(None)

  def mutableProxy(implicit strategy: MorpherStrategy[M]): MutableLUB = mutableProxy_(Some(strategy))

  private def mutableProxy_(customStrategy: Option[MorpherStrategy[M]]): MutableLUB = {
    val initialStrategy = customStrategy match {
      case None => this.defaultStrategy
      case Some(custStrat) => custStrat
    }

    val mp = new MutableCompositeContext[M, LUB, ConformLevel, ImutableLUB, MutableLUB](lubComponents, initialStrategy) {

      //override def morph(proxy: this.ci.MutableLUB, strategy: org.morpheus.MorpherStrategy[M]): this.ci.ImutableLUB
      override def morph(proxy: MutableLUB, actualStrategy: MorpherStrategy[M]): ImutableLUB = {
        Morpher.morph[M](outer, actualStrategy)(Some(proxy))
      }
    }

    mp.proxy
  }

  @deprecated
  def lazyProxy = {
    new LazyRef[MutableLUB] {
      def create(implicit strategy: MorpherStrategy[M]) = {
        val mp = mutableProxy
        apply(mp)
        mp match {
          case mut: MutableFragment with Mutator[M] =>
            mut.addListener(new MutableFragmentListener {
              override def onEvent(eventName: String, eventValue: Any, eventSource: Any) = {
                mut.remorph()
                Nil
              }
            })
          case _ =>
        }
        mp
      }
    }
  }

}

/**
 * Marks a composite instance that it may contain hidden fragments
 */
trait WithHiddenFragments

/**
 * Marks a composite instance that it contains no hidden fragments
 */
trait WithoutHiddenFragments
