package org.morpheus

import org.morpheus.Morpheus.or

/**
 * Created by zslajchrt on 29/04/15.
 */
trait FragmentClassAnalyzer {
  val univ: scala.reflect.api.Universe

  import univ._

  def extractConfigType(fragmentClazz: ClassSymbol): Option[Type] = {

    // We must take the self type from the fragment trait, since the fragment class may not be fully defined yet,
    // in contrast to the fragment trait. The fragment trait is stored at the 2nd position in the base classes list.

    if (fragmentClazz.baseClasses.size <= 1) {
      sys.error(s"Invalid fragment class $fragmentClazz with base classes ${fragmentClazz.baseClasses}")
    }

    fragmentClazz.toType.member(TermName("<init>")) match {
      case NoSymbol => None
      case constr: Symbol =>

        val constrParams = constr.info.paramLists.flatMap(identity)
        val configParam = constrParams match {
          case Nil => None
          case p :: Nil => Some(p.info.resultType)
          case _ => sys.error(s"Illegal constructor in fragment $fragmentClazz")
        }

        configParam
    }

  }
}
