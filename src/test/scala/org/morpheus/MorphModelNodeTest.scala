package org.morpheus

import org.morpheus._
import org.junit.Assert._
import org.junit.Test

/**
 * Created by zslajchrt on 06/03/15.
 */
class MorphModelNodeTest {

  @Test
  def testFlattenConj() {
    val root = ConjNode(List(ConjNode(List(ConjNode(List(FragmentNode(0), FragmentNode(1)))))))
    val flattened = root.flatten
    assertEquals(ConjNode(List(FragmentNode(0), FragmentNode(1))), flattened)
  }

  @Test
  def testFlattenDisj() {
    val root = DisjNode(List(DisjNode(List(DisjNode(List(FragmentNode(0), FragmentNode(1)))))))
    val flattened = root.flatten
    assertEquals(DisjNode(List(FragmentNode(0), FragmentNode(1))), flattened)
  }

  @Test
  def testFlattenMixed() {
    val root =
      ConjNode(List(ConjNode(List(ConjNode(List(FragmentNode(0), FragmentNode(1))))),
        DisjNode(List(DisjNode(List(DisjNode(List(FragmentNode(2), FragmentNode(3))))))),
        ConjNode(List(ConjNode(List(FragmentNode(4), FragmentNode(5)))))
      ))

    val flattened = root.flatten
    assertEquals(ConjNode(List(FragmentNode(0), FragmentNode(1), DisjNode(List(FragmentNode(2), FragmentNode(3))), FragmentNode(4), FragmentNode(5))), flattened)
  }

  @Test
  def test_++(): Unit = {
    val root1 = ConjNode(List(FragmentNode(0), DisjNode(List(FragmentNode(1), FragmentNode(2)))))
    val root2 = ConjNode(List(FragmentNode(0), DisjNode(List(FragmentNode(1), FragmentNode(2))), FragmentNode(3)))
    val merged = root1 ++ root2

    assertEquals(
      ConjNode(List(FragmentNode(0), DisjNode(List(FragmentNode(1), FragmentNode(2))), FragmentNode(3), DisjNode(List(FragmentNode(4), FragmentNode(5))), FragmentNode(6))),
      merged)

  }

}
