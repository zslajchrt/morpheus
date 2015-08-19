package org.morpheus

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Created by zslajchrt on 02/08/15.
 */
object FragmentValidator {

//  val valResults: Iterable[ValidationResult[Currency]] = for (currencyMorph <- currency)
//    yield currencyMorph.validateCurrency
//
//  failedFragments = Some((for (vr <- valResults if vr.failed; f <- vr.fragment) yield f.index).toSet)

  def validationResult[F](opt: Option[_], failureMsg: String): ValidationResult[F] = macro validationResult_impl[F]

  def success[F]: Success[F] = macro success_impl[F]

  def failure[F](failureMsg: String): Failure[F] = macro failure_impl[F]

  def validationResult_impl[F: c.WeakTypeTag](c: whitebox.Context)(opt: c.Expr[Option[_]], failureMsg: c.Expr[String]): c.Tree = {
    import c.universe._

    val fragTpe = implicitly[WeakTypeTag[F]]
    q"""
        {
          import org.morpheus.FragmentValidator._
          $opt match {
            case None => failure[$fragTpe]($failureMsg)
            case Some(_) => success[$fragTpe]
          }
        }
    """
  }

  def success_impl[F: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._
    q"""
      org.morpheus.FragmentValidator.Success(org.morpheus.Morpheus.fragmentInReferredKernel[${implicitly[WeakTypeTag[F]]}](this))
    """
  }

  def failure_impl[F: c.WeakTypeTag](c: whitebox.Context)(failureMsg: c.Tree): c.Tree = {
    import c.universe._
    q"""
      org.morpheus.FragmentValidator.Failure(org.morpheus.Morpheus.fragmentInReferredKernel[${implicitly[WeakTypeTag[F]]}](this), $failureMsg)
    """
  }

  sealed trait ValidationResult[F] {
    val fragment: Option[Frag[F, _]]
    def succeeded: Boolean = this.isInstanceOf[Success[F]]
    def failed: Boolean = !succeeded
  }
  case class Success[F](fragment: Option[Frag[F, _]]) extends ValidationResult[F]
  case class Failure[F](fragment: Option[Frag[F, _]], reason: String) extends ValidationResult[F]

  object ValidationResult {
    def extractInvalidFragments(validationResults: Iterable[ValidationResult[_]]): Set[Int] = {
      (for (vr <- validationResults if vr.failed; f <- vr.fragment) yield f.index).toSet
    }
    def extractValidFragments(validationResults: Iterable[ValidationResult[_]]): Set[Int] = {
      (for (vr <- validationResults if vr.succeeded; f <- vr.fragment) yield f.index).toSet
    }
    def extractAllFragments(validationResults: Iterable[ValidationResult[_]]): Set[Int] = {
      (for (vr <- validationResults; f <- vr.fragment) yield f.index).toSet
    }
  }
}


