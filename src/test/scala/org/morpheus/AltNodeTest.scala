package org.morpheus

import org.junit.Assert._
import org.junit.Test

/**
 *
 * Created by zslajchrt on 11/03/15.
 */
class AltNodeTest {

  @Test
  def testFindSubCounter(): Unit = {
    var ch = ChoiceAltNode(List(LeafAltNode(0), LeafAltNode(1)))
    assertEquals(2, ch.counter.length)
    val (_, 1, LeafAltNode(0)) = ch.findSubCounter(0)
    val (_, 1, LeafAltNode(1)) = ch.findSubCounter(1)

    ch = ChoiceAltNode(List(SeqAltNode(List(LeafAltNode(0), LeafAltNode(1))), LeafAltNode(2)))
    assertEquals(2, ch.counter.length)
    val (_, 1, SeqAltNode(List(LeafAltNode(0), LeafAltNode(1)))) = ch.findSubCounter(0)
    val (_, 1, LeafAltNode(2)) = ch.findSubCounter(1)

    ch = ChoiceAltNode(List(ChoiceAltNode(List(LeafAltNode(0), LeafAltNode(1))), ChoiceAltNode(List(LeafAltNode(2), LeafAltNode(3)))))
    assertEquals(4, ch.counter.length)
    val (_, 2, ChoiceAltNode(List(LeafAltNode(0), LeafAltNode(1)))) = ch.findSubCounter(0)
    val (_, 2, ChoiceAltNode(List(LeafAltNode(0), LeafAltNode(1)))) = ch.findSubCounter(1)
    val (_, 2, ChoiceAltNode(List(LeafAltNode(2), LeafAltNode(3)))) = ch.findSubCounter(2)
    val (_, 2, ChoiceAltNode(List(LeafAltNode(2), LeafAltNode(3)))) = ch.findSubCounter(3)

    ch = ChoiceAltNode(List(LeafAltNode(0), ChoiceAltNode(List(LeafAltNode(1), LeafAltNode(2)))))
    assertEquals(3, ch.counter.length)
    val (_, 1, LeafAltNode(0)) = ch.findSubCounter(0)
    val (_, 2, ChoiceAltNode(List(LeafAltNode(1), LeafAltNode(2)))) = ch.findSubCounter(1)
    val (_, 2, ChoiceAltNode(List(LeafAltNode(1), LeafAltNode(2)))) = ch.findSubCounter(2)
  }


  @Test
  def testTwoIndependentChoices(): Unit = {

    val tree = SeqAltNode(List(ChoiceAltNode(List(LeafAltNode(0), LeafAltNode(1))), ChoiceAltNode(List(LeafAltNode(2), LeafAltNode(3), LeafAltNode(4)))))
    val coupledCounters = new CoupledCounters(tree.collectCounters)

    val expected = List(
      List(0, 2),
      List(1, 2),
      List(0, 3),
      List(1, 3),
      List(0, 4),
      List(1, 4)
    ).reverse

    var result = List.empty[List[Int]]
    do {
      result ::= tree.alternative
    } while (coupledCounters.inc())

    assertEquals(expected, result)
  }

  @Test
  def testTwoDependentChoices(): Unit = {

    val tree = ChoiceAltNode(List(LeafAltNode(0), ChoiceAltNode(List(LeafAltNode(1), LeafAltNode(2)))))
    val coupledCounters = new CoupledCounters(tree.collectCounters)

    val expected = List(
      List(0),
      List(1),
      List(2)
    ).reverse

    var result = List.empty[List[Int]]
    do {
      result ::= tree.alternative
    } while (coupledCounters.inc())

    assertEquals(expected, result)
  }

  @Test
  def testIndependentDependentChoices(): Unit = {
    val tree = SeqAltNode(List(ChoiceAltNode(List(LeafAltNode(0), LeafAltNode(1))), ChoiceAltNode(List(LeafAltNode(2), SeqAltNode(List(LeafAltNode(3), ChoiceAltNode(List(LeafAltNode(4), LeafAltNode(5)))))))))
    val coupledCounters = new CoupledCounters(tree.collectCounters)

    val expected = List(
      List(0, 2),
      List(1, 2),
      List(0, 3, 4),
      List(1, 3, 4),
      List(0, 3, 5),
      List(1, 3, 5)
    ).reverse

    var result = List.empty[List[Int]]
    do {
      result ::= tree.alternative
    } while (coupledCounters.inc())

    assertEquals(expected, result)
  }
}
