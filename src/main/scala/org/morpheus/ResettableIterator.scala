package org.morpheus

/**
 *
 * Created by zslajchrt on 13/03/15.
 */
trait ResettableIterator[T] extends Iterator[T] {
  def reset(): Unit
}
