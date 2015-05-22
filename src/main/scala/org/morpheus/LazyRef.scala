package org.morpheus

/**
 * Created by zslajchrt on 29/04/15.
 */
@deprecated
class LazyRef[T] {
  private[this] var value: Option[T] = None

  def apply(v: T): T = {
    value = Some(v)
    v
  }

  def apply[R](fn: T => R, defVal: R): R = value match {
    case Some(v) => fn(v)
    case None => defVal
  }
}
