package org.morpheus

import org.morpheus._
import org.junit.Assert._
import org.junit.Test

/**
 *
 * Created by zslajchrt on 13/03/15.
 */
class AltIteratorTest {

  @Test
  def testChoiceWithUnitNode(): Unit = {

    val root = DisjNode(List(UnitNode, FragmentNode(0)))
    val altRoot = root.toAltNode

    val iter: IdentAltIterator[FragmentNode] = new IdentAltIterator(altRoot)

    val expected = List(Nil, List(FragmentNode(0))).reverse

    def oneLoop(): Unit = {
      var result = List.empty[List[FragmentNode]]
      while (iter.hasNext) {
        val res = iter.next()
        result ::= res
      }

      assertEquals(expected, result)
    }

    oneLoop()

    iter.reset()

    oneLoop()

  }

  @Test
  def testReset(): Unit = {

    val tree = SeqAltNode(List(ChoiceAltNode(List(LeafAltNode(0), LeafAltNode(1))), ChoiceAltNode(List(LeafAltNode(2), LeafAltNode(3), LeafAltNode(4)))))
    val iter = new AltIterator[Int, List[Int]](tree) {
      override protected def mapAlt(alt: List[Int]): List[Int] = alt
    }

    val expected = List(
      List(0, 2),
      List(1, 2),
      List(0, 3),
      List(1, 3),
      List(0, 4),
      List(1, 4)).reverse


    def oneLoop(): Unit = {
      var result = List.empty[List[Int]]
      while (iter.hasNext) {
        val res = iter.next()
        result ::= res
      }

      assertEquals(expected, result)
    }

    oneLoop()

    iter.reset()

    oneLoop()

  }

}
