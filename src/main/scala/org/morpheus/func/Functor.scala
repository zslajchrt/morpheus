package org.morpheus.func

import org.morpheus.{&, &!}

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * Created by zslajchrt on 22/06/15.
 */
trait CovFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: &![A] => &![B]): F[B]
}

trait ConFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: &[B] => &[A]): F[B]
}

object Functor {

  def augment[B](aRef: &![_]): Any = macro augment_impl[B]

  //def shrink[B](bRef: &[_]): Any = macro shrink_impl[B]

  def augment_impl[B: c.WeakTypeTag](c: whitebox.Context)(aRef: c.Expr[&![_]]): c.Expr[Any] = {
    import c.universe._

    val aTpe = aRef.actualType.typeArgs.head
    val bTpe = implicitly[WeakTypeTag[B]]

    //c.info(c.enclosingPosition, s"aTpe: $aTpe, bTpe: $bTpe", true)

    val res = q"""
      {
        import org.morpheus.Morpheus._
        import org.morpheus._

        val x12: &![$$[$bTpe] with $aTpe] = *($aRef)
        val bRef: &![$bTpe with $aTpe] = *(x12, single[$bTpe])
        bRef
      }
    """

    c.Expr[Any](res)

  }

//  def shrink_impl[B: c.WeakTypeTag](c: whitebox.Context)(aRef: c.Expr[&![_]]): c.Expr[Any] = {
//    import c.universe._
//
//    val aTpe = aRef.actualType.typeArgs.head
//    val bTpe = implicitly[WeakTypeTag[B]]
//
//    //c.info(c.enclosingPosition, s"aTpe: $aTpe, bTpe: $bTpe", true)
//
//    val res = q"""
//      {
//        import org.morpheus.Morpheus._
//        import org.morpheus._
//        val x12: &![$$[$bTpe] with $aTpe] = *($aRef)
//        val bRef: &![$bTpe with $aTpe] = *(x12, single[$bTpe])
//        bRef
//      }
//    """
//
//    c.Expr[Any](res)
//
//  }
}