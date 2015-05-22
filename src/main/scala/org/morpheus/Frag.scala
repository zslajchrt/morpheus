package org.morpheus

/**
 * Created by zslajchrt on 29/04/15.
 */

import scala.reflect.runtime.universe._

case class Frag[T, C](index: Int, fragTag: WeakTypeTag[T], cfgTag: WeakTypeTag[C], depsMappings: Option[AltMappings]) {
  val fragmentClass: RuntimeClass = ReflectHelper.classFromTag(fragTag)

  val configClass: RuntimeClass = ReflectHelper.classFromTag(cfgTag)

  val fragmentClassAnnotation = fragmentClass.getAnnotation(classOf[fragmentClass])

  val fragmentAnnotation = ReflectHelper.getAnnotation[fragment](fragTag.tpe)

  val wrapperAnnotation = ReflectHelper.getAnnotation[wrapper](fragTag.tpe)

  val dimAnnotation = ReflectHelper.getAnnotation[dimension](fragTag.tpe)

}
