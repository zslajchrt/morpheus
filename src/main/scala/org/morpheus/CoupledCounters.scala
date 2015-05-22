package org.morpheus

/**
 * Created by zslajchrt on 11/03/15.
 */

case class CoupledCounters(counters: List[Counter]) {

  private var cnt = 0

  def inc(): Boolean = {
    cnt += 1

    val res = counters.foldLeft(1)((div, counter) => {
      val a = cnt / div
      val b = if (a < counter.length) a else a % counter.length
      counter.set(b)
      div * counter.length
    })

    if (res == cnt) {
      cnt = 0
      false
    } else {
      true
    }
  }

  def reset(): Unit = {
    counters.foreach(_.set(0))
  }

}

class Counter(val length: Int) {

  private var cnt = 0

  def value = cnt

  def set(v: Int): Unit = {
    cnt = v
  }

}
