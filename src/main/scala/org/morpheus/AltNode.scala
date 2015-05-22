package org.morpheus

/**
 *
 * Created by zslajchrt on 11/03/15.
 */

sealed trait AltNode[+T] {

  val alts: List[AltNode[T]]

  def alternative: List[T]

  def collectCounters: List[Counter]

  /**
   * This can be used to show NoneAltNode in the alternatives.
   * @return
   */
  def toMetaAltNode: AltNode[AltNode[T]]
}

case class SeqAltNode[+T](alts: List[AltNode[T]]) extends AltNode[T] {

  def alternative: List[T] = alts.flatMap(_.alternative)

  def collectCounters: List[Counter] = alts.flatMap(_.collectCounters)

  def toMetaAltNode: AltNode[AltNode[T]] = SeqAltNode(alts.map(_.toMetaAltNode))
}

case class ChoiceAltNode[+T](alts: List[AltNode[T]]) extends AltNode[T] {

  val subCounters: List[(CoupledCounters, Int, AltNode[T])] = alts.map(alt => {
    val subCnts = alt.collectCounters match {
      case Nil => List(new Counter(1))
      case c => c
    }
    val pwr = subCnts.foldLeft(1)((r, v) => r * v.length)
    (new CoupledCounters(subCnts), pwr, alt)
  })

  def findSubCounter(cntVal: Int): (CoupledCounters, Int, AltNode[T]) = {
    var seen = 0
    subCounters.find(sc => {
      seen += sc._2
      seen > cntVal
    }).get
  }

  val counter = new Counter(subCounters.map(_._2).sum) {

    private [this] var currentSubCounterTpl = subCounters.head

    override def set(v: Int): Unit = {

      if (v != value) {
        currentSubCounterTpl._1.inc()
        currentSubCounterTpl = findSubCounter(v)
      }

      super.set(v)
    }
  }

  def alternative: List[T] = if (alts.isEmpty) Nil else findSubCounter(counter.value)._3.alternative

  def collectCounters: List[Counter] = List(counter)

  def toMetaAltNode: AltNode[AltNode[T]] = ChoiceAltNode(alts.map(_.toMetaAltNode))
}

case class LeafAltNode[+T](t: T) extends AltNode[T] {

  val alts: List[AltNode[T]] = Nil

  def next(): Boolean = false

  def alternative: List[T] = List(t)

  def collectCounters: List[Counter] = Nil

  def toMetaAltNode: AltNode[AltNode[T]] = LeafAltNode(this)
}

case object NoneAltNode extends AltNode[Nothing] {

  val alts: List[AltNode[Nothing]] = Nil

  def next(): Boolean = false

  def alternative: List[Nothing] = Nil

  def collectCounters: List[Counter] = Nil

  def toMetaAltNode: AltNode[AltNode[Nothing]] = LeafAltNode(NoneAltNode)
}
