package org.morpheus

import org.junit.Assert._
import org.junit.Test

import scala.collection.BitSet

/**
 *
 * Created by zslajchrt on 19/05/15.
 */
class AlternativesTest {

  val root =
    ConjNode(List(DisjNode(List(FragmentNode(0), FragmentNode(1))), DisjNode(List(FragmentNode(2), DisjNode(List(FragmentNode(3), FragmentNode(4)))))))

  @Test
  def testToList(): Unit = {

    val alternatives = Alternatives.apply[Any](root)

    val expected = List(
      (List(FragmentNode(0, false), FragmentNode(2, false)), 0.0),
      (List(FragmentNode(1, false), FragmentNode(2, false)), 0.0),
      (List(FragmentNode(0, false), FragmentNode(3, false)), 0.0),
      (List(FragmentNode(1, false), FragmentNode(3, false)), 0.0),
      (List(FragmentNode(0, false), FragmentNode(4, false)), 0.0),
      (List(FragmentNode(1, false), FragmentNode(4, false)), 0.0))

    assertEquals(expected, alternatives.toList)

  }

  @Test
  def testRate(): Unit = {
    val alts1 = Alternatives.apply[Any](root)
    val alts2 = alts1.rate((alt, oldRating) => oldRating + 1)

    val expected = List(
      (List(FragmentNode(0, false), FragmentNode(2, false)), 1.0),
      (List(FragmentNode(1, false), FragmentNode(2, false)), 1.0),
      (List(FragmentNode(0, false), FragmentNode(3, false)), 1.0),
      (List(FragmentNode(1, false), FragmentNode(3, false)), 1.0),
      (List(FragmentNode(0, false), FragmentNode(4, false)), 1.0),
      (List(FragmentNode(1, false), FragmentNode(4, false)), 1.0))

    assertEquals(expected, alts2.toList)
  }


  @Test
  def testFilter(): Unit = {
    val alts1 = Alternatives.apply[Any](root)
    val alts2 = alts1.filter((alt, rating) => !alt.contains(FragmentNode(4, false)))

    val expected = List(
      (List(FragmentNode(0, false), FragmentNode(2, false)), 0.0),
      (List(FragmentNode(1, false), FragmentNode(2, false)), 0.0),
      (List(FragmentNode(0, false), FragmentNode(3, false)), 0.0),
      (List(FragmentNode(1, false), FragmentNode(3, false)), 0.0))

    assertEquals(expected, alts2.toList)
  }

  @Test
  def testPromote(): Unit = {
    val alts1 = Alternatives.apply[Any](root)
    val alts2 = alts1.promote(Set(List(1, 4)))

    val expected = List(
      (List(FragmentNode(1, false), FragmentNode(4, false)), 0.0),
      (List(FragmentNode(0, false), FragmentNode(4, false)), 0.0),
      (List(FragmentNode(1, false), FragmentNode(3, false)), 0.0),
      (List(FragmentNode(0, false), FragmentNode(3, false)), 0.0),
      (List(FragmentNode(1, false), FragmentNode(2, false)), 0.0),
      (List(FragmentNode(0, false), FragmentNode(2, false)), 0.0))

    assertEquals(expected, alts2.toList)
  }


  @Test
  def testMaskUnmask(): Unit = {
    var fragBitsFn: (Alternatives[Any]) => Map[List[Int], BitSet] = (a: Alternatives[Any]) => a.ratedAlts.map(entry => entry._1.map(_.id) -> entry._2.fragmentBits)

    // masking

    val alts1 = Alternatives.apply[Any](root)
    var maskedAlts = alts1.toMaskedList
    assertEquals(6, maskedAlts.size)

    val alts2 = alts1.mask(Set(1))
    var fragBits = fragBitsFn(alts2)
    assertEquals(fragBits(List(1, 2)), BitSet(1))
    assertEquals(fragBits(List(1, 4)), BitSet(1))
    assertEquals(fragBits(List(0, 4)), BitSet())
    assertEquals(fragBits(List(0, 2)), BitSet())
    assertEquals(fragBits(List(1, 3)), BitSet(1))
    assertEquals(fragBits(List(0, 3)), BitSet())

    maskedAlts = alts2.toMaskedList
    assertEquals(3, maskedAlts.size)

    val alts3 = alts2.mask(Set(2))
    fragBits = fragBitsFn(alts3)
    assertEquals(fragBits(List(1, 2)), BitSet(1, 2))
    assertEquals(fragBits(List(1, 4)), BitSet(1))
    assertEquals(fragBits(List(0, 4)), BitSet())
    assertEquals(fragBits(List(0, 2)), BitSet(2))
    assertEquals(fragBits(List(1, 3)), BitSet(1))
    assertEquals(fragBits(List(0, 3)), BitSet())

    maskedAlts = alts3.toMaskedList
    assertEquals(1, maskedAlts.size)

    // unmasking

    val alts4 = alts3.unmask(Set(1))
    fragBits = fragBitsFn(alts4)
    assertEquals(fragBits(List(1, 2)), BitSet(2))
    assertEquals(fragBits(List(1, 4)), BitSet())
    assertEquals(fragBits(List(0, 4)), BitSet())
    assertEquals(fragBits(List(0, 2)), BitSet(2))
    assertEquals(fragBits(List(1, 3)), BitSet())
    assertEquals(fragBits(List(0, 3)), BitSet())

    maskedAlts = alts4.toMaskedList
    assertEquals(2, maskedAlts.size)

    val alts5 = alts4.unmask(Set(2))
    fragBits = fragBitsFn(alts5)
    assertEquals(fragBits(List(1, 2)), BitSet())
    assertEquals(fragBits(List(1, 4)), BitSet())
    assertEquals(fragBits(List(0, 4)), BitSet())
    assertEquals(fragBits(List(0, 2)), BitSet())
    assertEquals(fragBits(List(1, 3)), BitSet())
    assertEquals(fragBits(List(0, 3)), BitSet())

    maskedAlts = alts5.toMaskedList
    assertEquals(6, maskedAlts.size)

  }

}
