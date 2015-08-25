package org.morpheus

/**
 * Created by zslajchrt on 29/04/15.
 */

import scala.reflect.runtime.universe._

case class Frag[T, C](index: Int, fragTag: WeakTypeTag[T], cfgTag: WeakTypeTag[C], depsMappingsSerializedOpt: Option[String], fragGroup: Option[WeakTypeTag[_]], dimGroup: Option[WeakTypeTag[_]]) {
  val fragmentClass: RuntimeClass = ReflectHelper.classFromTag(fragTag)

  val configClass: RuntimeClass = ReflectHelper.classFromTag(cfgTag)

  val fragmentClassAnnotation = fragmentClass.getAnnotation(classOf[fragmentClass])

  val fragmentAnnotation = ReflectHelper.getAnnotation[fragment](fragTag.tpe)

  val wrapperAnnotation = ReflectHelper.getAnnotation[wrapper](fragTag.tpe)

  val dimAnnotation = ReflectHelper.getAnnotation[dimension](fragTag.tpe)

  lazy val depsMappings: Option[AltMappings] = depsMappingsSerializedOpt match {
    case None => None
    case Some(depsMappingsSerialized) => Some(AltMappings(depsMappingsSerialized))
  }

  def sameWrapperGroup(other: Frag[_, _]): Boolean = if (other == this) {
    true
  } else {
    if (wrapperAnnotation.isDefined) {
      val dimTest: Option[Boolean] = for (dg1 <- dimGroup; dg2 <- other.dimGroup) yield dg1.tpe.erasure =:= dg2.tpe.erasure
      val fragTest: Option[Boolean] = for (fg1 <- fragGroup; fg2 <- other.fragGroup) yield fg1.tpe.erasure =:= fg2.tpe.erasure
      val dimTestRes = dimTest match {
        case None => false
        case Some(r) => r
      }
      val fragTestRes = fragTest match {
        case None => false
        case Some(r) => r
      }
      dimTestRes || fragTestRes
    } else {
      false
    }
  }
}
