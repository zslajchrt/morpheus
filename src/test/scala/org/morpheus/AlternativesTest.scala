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
  def testMask(): Unit = {

    // masking

    var alts = Alternatives.apply[Any](root)
    var maskedAlts = alts.toMaskedList

    assertEquals(List((List(FragmentNode(0,false), FragmentNode(2,false)),0.0),
      (List(FragmentNode(1,false), FragmentNode(2,false)),0.0),
      (List(FragmentNode(0,false), FragmentNode(3,false)),0.0),
      (List(FragmentNode(1,false), FragmentNode(3,false)),0.0),
      (List(FragmentNode(0,false), FragmentNode(4,false)),0.0),
      (List(FragmentNode(1,false), FragmentNode(4,false)),0.0)), maskedAlts)

    alts = alts.mask(Set(1))
    maskedAlts = alts.toMaskedList

    assertEquals(List((List(FragmentNode(1,false), FragmentNode(2,false)),0.0),
      (List(FragmentNode(1,false), FragmentNode(3,false)),0.0),
      (List(FragmentNode(1,false), FragmentNode(4,false)),0.0)), maskedAlts)


    alts = alts.mask(Set(2))
    maskedAlts = alts.toMaskedList

    assertEquals(List((List(FragmentNode(1,false), FragmentNode(2,false)),0.0)), maskedAlts)

    // unmasking

    alts = alts.unmask(Set(1))
    maskedAlts = alts.toMaskedList

    assertEquals(List((List(FragmentNode(0,false), FragmentNode(2,false)),0.0),
      (List(FragmentNode(1,false), FragmentNode(2,false)),0.0)), maskedAlts)

    alts = alts.unmask(Set(2))
    maskedAlts = alts.toMaskedList

    assertEquals(List((List(FragmentNode(0,false), FragmentNode(2,false)),0.0),
      (List(FragmentNode(1,false), FragmentNode(2,false)),0.0),
      (List(FragmentNode(0,false), FragmentNode(3,false)),0.0),
      (List(FragmentNode(1,false), FragmentNode(3,false)),0.0),
      (List(FragmentNode(0,false), FragmentNode(4,false)),0.0),
      (List(FragmentNode(1,false), FragmentNode(4,false)),0.0)), maskedAlts)

  }

}
