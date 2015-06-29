package org.morpheus.dci

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import org.morpheus.MorphMirror

/**
 * Created by zslajchrt on 22/06/15.
 */
object DCI {

  def role[R, D, C](data: D): R = macro role_impl[R, D, C]

  def role_impl[R: c.WeakTypeTag, D: c.WeakTypeTag, C: c.WeakTypeTag](c: whitebox.Context)(data: c.Expr[D]): c.Expr[R] = {
    import c.universe._

    val roleTpe = implicitly[WeakTypeTag[R]]
    val dataTpe = implicitly[WeakTypeTag[D]]
    val contextTpe = implicitly[WeakTypeTag[C]]

    val res = q"""
      {
        import org.morpheus.Morpheus._
        import org.morpheus._
        implicit val dataFrag = external[$dataTpe]($data)
        implicit val selfFrag = external[$contextTpe](this)
        singleton[$dataTpe with $roleTpe with $contextTpe].!
      }
    """

    c.Expr[R](res)

  }

}