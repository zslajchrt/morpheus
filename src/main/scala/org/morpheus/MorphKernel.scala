package org.morpheus

import shapeless.HList

import scala.reflect.runtime.universe._

/**
 * Created by zslajchrt on 29/04/15.
 */
abstract class MorphKernelBase[M](val rootNode: MorphModelNode) {

  val parent: Option[MorphKernelBase[_]]
  val defaultStrategy: MorphingStrategy[M]
  val altComposer: AlternativeComposer[M]

  def fragmentList: List[FragmentHolder[_]]

  val model: MorphModelBase[M]

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

abstract class MorphKernel[M](val root: MorphModelNode) extends MorphKernelBase[M](root) with MorphingTools[M] {

  outer =>

  //def this(ci: MorphKernelRef[M]) = this(ci.instance.rootNode)

  type LUB
  type Model = M
  type ConformLevel <: ConformanceLevelMarker
  type MutableLUB = LUB with MutableMorphMirror[M] {type ConfLev = ConformLevel; type LUB = outer.LUB}
  type ImmutableLUB = LUB with MorphMirror[M] {type ConfLev = ConformLevel; type LUB = outer.LUB}

  val fragmentDescriptors: HList
  val fragments: HList
  val proxies: AnyRef
  val model: MorphModel[M]

  val lubComponents: Array[Class[_]]

  lazy val ! = make
  lazy val ~ = make_~

  def morph(implicit strategy: MorphingStrategy[M]): ImmutableLUB = {
    Morpher.morph[M](this, Some(strategy))(None)
  }

  def make: ImmutableLUB = {
    Morpher.morph[M](this, None)(None)
  }

  def morph_~(implicit strategy: MorphingStrategy[M]): MutableLUB = mutableProxy

  def make_~ : MutableLUB = mutableProxy_(None)

  def mutableProxy(implicit strategy: MorphingStrategy[M]): MutableLUB = mutableProxy_(Some(strategy))

  private def mutableProxy_(customStrategy: Option[MorphingStrategy[M]]): MutableLUB = {
    val initialStrategy = customStrategy match {
      case None => this.defaultStrategy
      case Some(custStrat) => custStrat
    }

    val mp = new MutableMorphContext[M, LUB, ConformLevel](this, lubComponents, initialStrategy) {

      //override def morph(proxy: MutableLUB, actualStrategy: MorphingStrategy[M]): owningKernel.ImmutableLUB = {
      override def morph(proxyOpt: Option[owningKernel.MutableLUB], actualStrategy: MorphingStrategy[M]): owningKernel.ImmutableLUB = {
        Morpher.morph[M](owningKernel, actualStrategy)(proxyOpt)
      }
    }

    mp.proxy.asInstanceOf[MutableLUB]
  }

  @deprecated
  def lazyProxy = {
    new LazyRef[MutableLUB] {
      def create(implicit strategy: MorphingStrategy[M]) = {
        val mp = mutableProxy
        apply(mp)
        mp match {
          case mut: MutableFragment =>
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

  class MorphIterator extends Iterator[ImmutableLUB] {

    private val alts: List[List[FragmentNode]] = model.altIterator().toList
    private[this] var i = 0

    override def hasNext: Boolean = i < alts.size

    override def next(): ImmutableLUB = {
      val promotedAlt: List[Int] = alts(i).map(_.id)
      i += 1

      morph(new MorphingStrategy[M] {
        override def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M] = {
          model.alternatives.promote(Set(promotedAlt))
        }
      })

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

