package org.morpheus

import org.junit.Assert._
import org.junit.Test

/**
 *
 * Created by zslajchrt on 11/03/15.
 */
class CounterSequenceTest {

  @Test
  def testNoCoupledCounters(): Unit = {
    val counters = List.empty[CounterState]
    val coupled = new CounterSequence(counters)

    var result = List.empty[List[Int]]
    do {
      result ::= counters.map(_.value)
    } while (coupled.inc())

    assertEquals(List(Nil), result)
  }

  @Test
  def testSingleCounterLength1(): Unit = {
    val counters = List(new CounterState(1))
    val coupled = new CounterSequence(counters)

    val expected = List(
      List(0)
    )

    var result = List.empty[List[Int]]
    do {
      result ::= counters.map(_.value)
    } while (coupled.inc())

    assertEquals(expected, result)

  }

  @Test
  def testSingleCounter(): Unit = {
    val counters = List(new CounterState(2))
    val coupled = new CounterSequence(counters)

    val expected = List(
      List(0),
      List(1)
    ).reverse

    var result = List.empty[List[Int]]
    do {
      result ::= counters.map(_.value)
    } while (coupled.inc())

    assertEquals(expected, result)

  }

  @Test
  def testTwoCoupledCounters(): Unit = {
    val counters = List(new CounterState(2), new CounterState(3), new CounterState(4))
    val coupled = new CounterSequence(counters)

    val expected = List(
        List(0, 0, 0),
        List(1, 0, 0),
        List(0, 1, 0),
        List(1, 1, 0),
        List(0, 2, 0),
        List(1, 2, 0),
        List(0, 0, 1),
        List(1, 0, 1),
        List(0, 1, 1),
        List(1, 1, 1),
        List(0, 2, 1),
        List(1, 2, 1),
        List(0, 0, 2),
        List(1, 0, 2),
        List(0, 1, 2),
        List(1, 1, 2),
        List(0, 2, 2),
        List(1, 2, 2),
        List(0, 0, 3),
        List(1, 0, 3),
        List(0, 1, 3),
        List(1, 1, 3),
        List(0, 2, 3),
        List(1, 2, 3)
    ).reverse

    var result = List.empty[List[Int]]
    do {
      result ::= counters.map(_.value)
    } while (coupled.inc())

    assertEquals(expected, result)

  }

  @Test
  def testFindSubCounter(): Unit = {
    var ch = new CounterChoice(List(CounterSequence(List(new CounterState(1))), CounterSequence(List(new CounterState(1)))))
    assertEquals(0, ch.findSubCounter(0))
    assertEquals(1, ch.findSubCounter(1))

    ch = new CounterChoice(List(CounterSequence(List(new CounterState(2))), CounterSequence(List(new CounterState(3)))))
    assertEquals(0, ch.findSubCounter(0))
    assertEquals(0, ch.findSubCounter(1))
    assertEquals(1, ch.findSubCounter(2))
    assertEquals(1, ch.findSubCounter(3))
    assertEquals(1, ch.findSubCounter(4))
  }


}
