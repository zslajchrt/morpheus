package org.morpheus

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Created by zslajchrt on 02/08/15.
 */
object FragmentValidator {

  def success[F]: Success[F] = macro success_impl[F]

  def failure[F](reason: String): Failure[F] = macro failure_impl[F]

  def success_impl[F: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    q"""
      org.morpheus.FragmentValidator.Success(org.morpheus.Morpheus.fragmentInReferredKernel[${implicitly[WeakTypeTag[F]]}](this))
    """
  }

  def failure_impl[F: c.WeakTypeTag](c: whitebox.Context)(reason: c.Tree): c.Tree = {
    import c.universe._
    q"""
      org.morpheus.FragmentValidator.Failure(org.morpheus.Morpheus.fragmentInReferredKernel[${implicitly[WeakTypeTag[F]]}](this), $reason)
    """
  }

  sealed trait ValidationResult[F] {
    val fragment: Option[Frag[F, _]]
    def succeeded: Boolean = this.isInstanceOf[Success[F]]
    def failed: Boolean = !succeeded
  }
  case class Success[F](fragment: Option[Frag[F, _]]) extends ValidationResult[F]
  case class Failure[F](fragment: Option[Frag[F, _]], reason: String) extends ValidationResult[F]

}


