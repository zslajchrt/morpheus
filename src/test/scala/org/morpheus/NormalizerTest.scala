package org.morpheus

import org.morpheus._
import org.junit.Assert._
import org.junit.Test

/**
 *
 * Created by zslajchrt on 09/04/15.
 */
class NormalizerTest {

  @Test
  def testChooseAlternativesZeroes(): Unit = {
    val rootNode = DisjNode(List(
      FragmentNode(0),
      FragmentNode(1),
      FragmentNode(2)))
    val alts = Alternatives.apply[Any](rootNode)

    val normalized = Normalizer(FixedStrategy(alts)).chooseAlternatives(null)(None).toList

    val expected = List(
      (List(FragmentNode(0)), 0d),
      (List(FragmentNode(1)), 0d),
      (List(FragmentNode(2)), 0d))

    assertEquals(expected, normalized)
  }

  @Test
  def testChooseAlternativesDefault(): Unit = {
    val rootNode = DisjNode(List(
      FragmentNode(0),
      FragmentNode(1),
      FragmentNode(2)))
    val rating = Map(List(FragmentNode(0)) -> 0d, List(FragmentNode(1)) -> 1d, List(FragmentNode(2)) -> 2d)
    val alts = Alternatives.apply[Any](rootNode).rate((alt, _) => rating(alt))

    val normalized = Normalizer(FixedStrategy(alts)).chooseAlternatives(null)(None).toList

    val expected = List(
      (List(FragmentNode(0)), 0d),
      (List(FragmentNode(1)), 0.5d),
      (List(FragmentNode(2)), 1d))

    assertEquals(expected, normalized)
  }

  @Test
  def testChooseAlternatives2(): Unit = {
    val rootNode = DisjNode(List(
      FragmentNode(0),
      FragmentNode(1),
      FragmentNode(2)))
    val rating = Map(List(FragmentNode(0)) -> -1d, List(FragmentNode(1)) -> 0d, List(FragmentNode(2)) -> 1d)
    val alts = Alternatives.apply[Any](rootNode).rate((alt, _) => rating(alt))

    val normalized = Normalizer(FixedStrategy(alts), 0, 2).chooseAlternatives(null)(None).toList

    val expected = List(
      (List(FragmentNode(0)), 0d),
      (List(FragmentNode(1)), 1d),
      (List(FragmentNode(2)), 2d))

    assertEquals(expected, normalized)
  }

  @Test
  def testChooseAlternatives3(): Unit = {
    val rootNode = DisjNode(List(
      FragmentNode(0),
      FragmentNode(1),
      FragmentNode(2)))
    val rating = Map(List(FragmentNode(0)) -> 0d, List(FragmentNode(1)) -> 1d, List(FragmentNode(2)) -> 2d)
    val alts = Alternatives.apply[Any](rootNode).rate((alt, _) => rating(alt))

    val normalized = Normalizer(FixedStrategy(alts), -1, 1).chooseAlternatives(null)(None).toList

    val expected = List(
      (List(FragmentNode(0)), -1d),
      (List(FragmentNode(1)), 0d),
      (List(FragmentNode(2)), 1d))

    assertEquals(expected, normalized)
  }
}
