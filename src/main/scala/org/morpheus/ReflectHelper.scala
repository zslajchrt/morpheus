package org.morpheus

import scala.reflect.runtime.universe._
import scala.reflect.api.Universe

/**
 * Created by zslajchrt on 29/04/15.
 */
object ReflectHelper {
  val mirror = reflect.runtime.universe.runtimeMirror(getClass.getClassLoader)

  type fragmentClassAnnot = fragmentClass

  def newFragment[F, C](frag: Frag[F, C], cfgOpt: Option[C]): F = {

    val fragTp: Type = frag.fragTag.tpe
    val cfg: Option[(Any, Type)] = cfgOpt match {
      case None => None
      case Some(c) => Some((c, frag.cfgTag.tpe))
    }

    val fragmentTrait: Class[_] = mirror.runtimeClass(fragTp.typeSymbol.asClass) //Class.forName(fragTp.typeSymbol.fullName)
    if (!fragmentTrait.isInterface) {
      // Entity instantiation
      fragmentTrait.newInstance().asInstanceOf[F]

      //      val entityEnhancer = new EntityEnhancer(fragmentTrait)
      //      entityEnhancer.create().asInstanceOf[F]

    } else {

      val fragmentClass: RuntimeClass = mirror.runtimeClass(mirror.staticClass(s"${fragTp.typeSymbol.fullName}$$fragment"))
      val fragmentClassType: ClassSymbol = mirror.classSymbol(fragmentClass)

      //val fragmentClass: Class[_] = clazz // Class.forName(s"${fragTp.typeSymbol.fullName}$$fragment")
      //val fragClassAnnot: fragmentClassAnnot = fragmentClass.getAnnotation(classOf[fragmentClassAnnot])

      // Fragment instantiation
      val ext = new FragmentClassAnalyzer {
        override val univ: Universe = mirror.universe
      }

      val isWrapper = frag.wrapperAnnotation.isDefined
      val isDimWrapper = frag.dimAnnotation.isDefined

      def newFragmentWithConfig(config: Any, cfgCls: Class[_]) = {
        createFragmentProxy(Array(cfgCls), Array(config.asInstanceOf[AnyRef]))
      }

      def newFragmentWithoutConfig() = {
        createFragmentProxy(Array.empty, Array.empty)
      }

      def createFragmentProxy(fragArgTypes: Array[Class[_]], fragArgs: Array[AnyRef]) = {
        val initContext: FragmentInitContext = new FragmentInitContext(fragmentTrait)
        if (isWrapper) {
          FragmentEnhancer.createFragmentWrapperProxy(initContext, fragmentClass, fragArgTypes, fragArgs, isDimWrapper)
        } else {
          FragmentEnhancer.createFragmentProxy(initContext, fragmentClass, fragArgTypes, fragArgs)
        }
      }

      val fragInst = cfg match {
        case None => newFragmentWithoutConfig()
        case Some((cfgInst, cfgTpe)) =>
          val cfgCls: Class[_] = mirror.runtimeClass(cfgTpe.typeSymbol.asClass)
          newFragmentWithConfig(cfgInst, cfgCls)
      }

      fragInst.asInstanceOf[F]
    }
  }

  def classFromTag[T](tg: WeakTypeTag[T]): RuntimeClass = {
    mirror.runtimeClass(tg.tpe.typeSymbol.asClass)
  }

  def getAnnotation[A: WeakTypeTag](tpe: Type) = {
    val annotTag = implicitly[WeakTypeTag[A]]
    tpe.typeSymbol.annotations.find(ann => {
      ann.tree.tpe =:= annotTag.tpe
    })
  }

  def invokeMethod(inst: AnyRef, method: MethodSymbol, args: Any*): AnyRef = {
    val filterFragmentMirror: InstanceMirror = ReflectHelper.mirror.reflect(inst)
    val methodMirror: MethodMirror = filterFragmentMirror.reflectMethod(method)
    methodMirror.apply(args: _*).asInstanceOf[AnyRef]
  }

}
