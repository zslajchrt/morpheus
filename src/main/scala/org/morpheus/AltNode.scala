package org.morpheus

/**
 *
 * Created by zslajchrt on 11/03/15.
 */

sealed trait AltNode[+T] {

  val children: List[AltNode[T]]

  def alternative: List[T]

  def counters: CounterSequence

}

case class SeqAltNode[+T](children: List[AltNode[T]]) extends AltNode[T] {

  def alternative: List[T] = children.flatMap(_.alternative)

  val counters: CounterSequence = CounterSequence(children.flatMap(_.counters.counters))

}

case class ChoiceAltNode[+T](children: List[AltNode[T]]) extends AltNode[T] {

  val choiceCounter = new CounterChoice(children.map(_.counters))

  def currentChild: AltNode[T] = children(choiceCounter.currentChildCountersIndex)

  def alternative: List[T] = if (children.isEmpty) Nil else children(choiceCounter.currentChildCountersIndex).alternative

  lazy val counters: CounterSequence = choiceCounter.asCoupledCounters

}

case class LeafAltNode[+T](t: T) extends AltNode[T] {

  val children: List[AltNode[T]] = Nil

  def next(): Boolean = false

  def alternative: List[T] = List(t)

  val counters: CounterSequence = CounterSequence(List(new CounterState(1)))

}

case object NoneAltNode extends AltNode[Nothing] {

  val children: List[AltNode[Nothing]] = Nil

  def next(): Boolean = false

  def alternative: List[Nothing] = Nil

  val counters: CounterSequence = CounterSequence(Nil)

}
