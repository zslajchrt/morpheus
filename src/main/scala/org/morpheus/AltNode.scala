package org.morpheus

/**
 *
 * Created by zslajchrt on 11/03/15.
 */

sealed trait AltNode[+T] {

  val children: List[AltNode[T]]

  def alternative: List[T]

  def counters: CoupledCounters

}

case class SeqAltNode[+T](children: List[AltNode[T]]) extends AltNode[T] {

  def alternative: List[T] = children.flatMap(_.alternative)

  val counters: CoupledCounters = CoupledCounters(children.flatMap(_.counters.counters))

}

case class ChoiceAltNode[+T](children: List[AltNode[T]]) extends AltNode[T] {

  private val subCounters: List[(CoupledCounters, AltNode[T])] = children.map(ch => (ch.counters, ch))
  private val length: Int = subCounters.map(_._1.length).sum

  def findSubCounter(cntVal: Int): (CoupledCounters, AltNode[T]) = {
    var seen = 0
    subCounters.find(sc => {
      seen += sc._1.length
      seen > cntVal
    }).get
  }

  val counter = new Counter(length) {

    private [this] var currentSubCounters = subCounters.head

    override def set(v: Int): Unit = {

      if (v != value) {
        currentSubCounters._1.inc()
        currentSubCounters = findSubCounter(v)
      }

      super.set(v)
    }
  }

  def alternative: List[T] = if (children.isEmpty) Nil else findSubCounter(counter.value)._2.alternative

  def counters: CoupledCounters = CoupledCounters(List(counter))

}

case class LeafAltNode[+T](t: T) extends AltNode[T] {

  val children: List[AltNode[T]] = Nil

  def next(): Boolean = false

  def alternative: List[T] = List(t)

  def counters: CoupledCounters = CoupledCounters(List(new Counter(1)))

}

case object NoneAltNode extends AltNode[Nothing] {

  val children: List[AltNode[Nothing]] = Nil

  def next(): Boolean = false

  def alternative: List[Nothing] = Nil

  def counters: CoupledCounters = CoupledCounters(Nil)

}
