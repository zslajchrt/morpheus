package org.morpheus.test

import scala.language.experimental.macros

import java.util.regex.Pattern

import scala.reflect.macros.{ whitebox, TypecheckException }

/**
 * A utility which ensures that a code fragment does not typecheck.
 *
 * Credit: Stefan Zeiger (@StefanZeiger)
 */
object illTyped {
  def apply(code: String): Unit = macro applyImplNoExp
  def apply(code: String, expected: String): Unit = macro applyImpl

  def applyImplNoExp(c: whitebox.Context)(code: c.Expr[String]) = applyImpl(c)(code, null)

  def applyImpl(c: whitebox.Context)(code: c.Expr[String], expected: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._

    val Expr(Literal(Constant(codeStr: String))) = code
    val (expPat, expMsg) = expected match {
      case null => (null, "Expected some error.")
      case Expr(Literal(Constant(s: String))) =>
        (Pattern.compile(s, Pattern.CASE_INSENSITIVE | Pattern.DOTALL), "Expected error matching: "+s)
    }

    try {
      val dummy = TermName(c.freshName())
      c.typecheck(c.parse(s"{ val $dummy = { $codeStr } ; () }"))
      c.abort(c.enclosingPosition, "Type-checking succeeded unexpectedly.\n"+expMsg)
    } catch {
      case e: TypecheckException =>
        val msg = e.getMessage
        if((expected ne null) && !expPat.matcher(msg).matches)
          c.abort(c.enclosingPosition, s"Type-checking failed in an unexpected way.\n$expMsg\nActual error: $msg")
    }

    reify(())
  }
}
