package org.morpheus

import shapeless.Poly1

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

/**
 * Created by zslajchrt on 29/04/15.
 */
object ConfigImplicits extends Poly1 {

  implicit def caseUnivExplicit[T, Cfg](implicit factoryFn: (Frag[T, Cfg]) => T): Case.Aux[Frag[T, Cfg], FragmentHolder[T]] = at[Frag[T, Cfg]](frag => {
    new FragmentHolder[T] {
      val fragment = frag

      def proxy: T = factoryFn(fragment)

      override def toString: String = s"${frag.fragTag.tpe}"
    }
  })

}

object FactoryProviderImplicits {

  implicit def implicitFragmentFactory[T]: (Frag[T, Unit]) => T = macro FactoryProviderImplicitsImpl.implicitFragmentFactory[T]

}

/**
 * This provider takes fragment instances from the source composite instance.
 */
class CopyProviderImplicits[M](val src: MorphKernelBase[M], val placeholderFactMap: Map[Int, (Frag[_, _] => _)], modelRoot: MorphModelNode) {

  implicit def implicitFragmentFactory[T, Cfg]: (Frag[T, Cfg]) => T = {
    (frag) => {
      if (modelRoot.fragments(frag.index).placeholder) {
        placeholderFactMap.get(frag.index) match {
          case None => sys.error(s"Fragment factory for placeholder ${frag.fragTag.tpe} not found")
          case Some(placeholderFact) => placeholderFact(frag).asInstanceOf[T]
        }
      } else {

        def findHolder(inst: MorphKernelBase[_]): Option[FragmentHolder[_]] = {
          inst.fragmentHolder(frag) match {
            case Some(holder) =>
              Some(holder)
            case None => inst.parent match {
              case None => None
              case Some(par) => findHolder(par)
            }
          }
        }

        findHolder(src) match {
          case Some(holder) =>
            holder.proxy.asInstanceOf[T]
          case None => sys.error(s"Cannot find fragment holder in the source composite instance for fragment ${frag.fragTag.tpe}")
        }

      }
    }
  }
}

//class ForkProviderImplicits[M]() {
//
//  implicit def implicitFragmentFactory[T, Cfg]: (Frag[T, Cfg]) => T = {
//    (frag) => {
//      sys.error("This is a dummy fragment factory supposed to be replaced by ForkAlternativeComposer")
//    }
//  }
//}

//class ForkMorphKernelBase[M](src1: MorphKernelBase[M], src2: MorphKernelBase[M]) extends MorphKernelBase[M](src1.rootNode) {
//  override val parent: Option[MorphKernelBase[_]] = active.parent
//
//  override def fragmentList: List[FragmentHolder[_]] = active.fragmentList
//
//  override val altComposer: AlternativeComposer[M] = active.altComposer
//  override val model: MorphModelBase[M] = active.model
//  override val defaultStrategy: MorphingStrategy[M] = active.defaultStrategy
//
//  def active: MorphKernelBase[M] = src1
//}

class FactoryProviderImplicitsImpl(val c: whitebox.Context) {

  import c.universe._

  def implicitFragmentFactory[T: WeakTypeTag]: Tree = {
    val tp = implicitly[WeakTypeTag[T]].tpe
    if (Morpheus.isAbstractFragment(c)(tp)) {
      c.abort(c.enclosingPosition, s"Fragment $tp is abstract. Its instance must be provided via an implicit value.")
    }
    q"org.morpheus.Morpheus.frag[$tp]"
  }

}

object SingletonProviderImplicits extends Poly1 {

  implicit def implicitFragmentFactory[T]: (Frag[T, Unit]) => T = macro SingletonProviderImplicitsImpl.implicitFragmentFactory[T]

}

class SingletonProviderImplicitsImpl(val c: whitebox.Context) {

  import c.universe._

  def implicitFragmentFactory[T: WeakTypeTag]: Tree = {
    val tp = implicitly[WeakTypeTag[T]].tpe
    if (Morpheus.isAbstractFragment(c)(tp)) {
      c.abort(c.enclosingPosition, s"Fragment $tp is abstract. Its instance must be provided via an implicit value.")
    }
    q"org.morpheus.Morpheus.single[$tp]"
  }

}

object InstanceProviderImplicits extends Poly1 {

  implicit def implicitFragmentFactory[T, Cfg]: (Frag[T, Cfg]) => T = macro InstanceProviderImplicitsImpl.implicitFragmentFactory[T, Cfg]

}

class InstanceProviderImplicitsImpl(val c: whitebox.Context) {

  import c.universe._

  def implicitFragmentFactory[T: WeakTypeTag, Cfg: WeakTypeTag]: Tree = {
    val tp = implicitly[WeakTypeTag[T]]
    val tpCfg = implicitly[WeakTypeTag[Cfg]]
    q"org.morpheus.Morpheus.existing[$tp, $tpCfg]"
  }

}
