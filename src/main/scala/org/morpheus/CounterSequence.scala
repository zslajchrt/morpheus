package org.morpheus

/**
 * Created by zslajchrt on 11/03/15.
 */

case class CounterSequence(counters: List[CounterState]) {

  /**
   * The total length of the coupled counters is the product of all partial counters.
   */
  lazy val length = counters.foldLeft(1)((r, c) => r * c.length)

  /**
   * The master counter with range (0..length-1)
   */
  private var masterCnt = 0

  def inc(): Boolean = {
    masterCnt += 1

    // Update each partial counter by its modulo calculated from the master counter
    var partCntBase = 1
    for (counter <- counters) {
      val partCntIndex = masterCnt / partCntBase
      val partCntModulo = partCntIndex % counter.length
      // Update the partial counter
      counter.set(partCntModulo)
      // update the base for the next partial counter
      partCntBase *= counter.length
    }

    if (masterCnt == length) {
      masterCnt = 0
      false
    } else {
      true
    }
  }

  def reset(): Unit = {
    counters.foreach(_.set(0))
  }

}

class CounterChoice(counters: List[CounterSequence]) extends CounterState(counters.map(_.length).sum) {

  private [this] var curSubCntIndex = 0

  def currentChildCountersIndex = curSubCntIndex

  def findSubCounter(cntVal: Int): Int = {
    var seen = 0
    var i = 0
    counters.indexWhere(sc => {
      seen += sc.length
      seen > cntVal
    })
  }

  override def set(v: Int): Unit = {

    if (v != value) {
      val curSubCnt = counters(curSubCntIndex)
      curSubCnt.inc()
      curSubCntIndex = findSubCounter(v)
    }

    super.set(v)
  }

  lazy val asCoupledCounters = CounterSequence(List(this))
}

class CounterState(val length: Int) {

  private var cnt = 0

  def value = cnt

  def set(v: Int): Unit = {
    cnt = v
  }

}
