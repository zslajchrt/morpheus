package org.morpheus

import org.junit.Assert._
import org.junit.Test

/**
 *
 * Created by zslajchrt on 11/03/15.
 */
class CoupledCountersTest {

  @Test
  def testNoCoupledCounters(): Unit = {
    val counters = List.empty[Counter]
    val coupled = new CoupledCounters(counters)

    var result = List.empty[List[Int]]
    do {
      result ::= counters.map(_.value)
    } while (coupled.inc())

    assertEquals(List(Nil), result)
  }

  @Test
  def testSingleCounterLength1(): Unit = {
    val counters = List(new Counter(1))
    val coupled = new CoupledCounters(counters)

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
    val counters = List(new Counter(2))
    val coupled = new CoupledCounters(counters)

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
    val counters = List(new Counter(2), new Counter(3), new Counter(4))
    val coupled = new CoupledCounters(counters)

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

}
