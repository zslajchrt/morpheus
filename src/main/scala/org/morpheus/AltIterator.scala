package org.morpheus

/**
 *
 * Created by zslajchrt on 13/03/15.
 */
abstract class AltIterator[T, R](val rootAltNode: AltNode[T]) extends ResettableIterator[R] {

  private val coupledCounters = new CoupledCounters(rootAltNode.collectCounters)

  protected def mapAlt(alt: List[T]): R

  def current(): R = mapAlt(rootAltNode.alternative)

  private var hasMore = true

  override def hasNext: Boolean = hasMore

  override def next(): R = {
    try {
      current()
    }
    finally {
      hasMore = coupledCounters.inc()
    }
  }

  override def reset(): Unit = {
    coupledCounters.reset()
    hasMore = true
  }
}

class IdentAltIterator[T](root: AltNode[T]) extends AltIterator[T, List[T]](root) {
  override protected def mapAlt(alt: List[T]): List[T] = alt
}
