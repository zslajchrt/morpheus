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

  @Test
  def testCreateAntagonistsMatrix(): Unit = {
    val root1 = DisjNode(List(FragmentNode(0), FragmentNode(1), FragmentNode(2)))
    val matrix1: Set[(FragmentNode, FragmentNode)] = root1.createAntagonistsMatrix()
    assertEquals(Set(
      (FragmentNode(0,false),FragmentNode(1,false)),
      (FragmentNode(0,false),FragmentNode(2,false)),
      (FragmentNode(1,false),FragmentNode(0,false)),
      (FragmentNode(1,false),FragmentNode(2,false)),
      (FragmentNode(2,false),FragmentNode(0,false)),
      (FragmentNode(2,false),FragmentNode(1,false))), matrix1)

    val root2 = ConjNode(List(DisjNode(List(FragmentNode(0), FragmentNode(1))), FragmentNode(2)))
    val matrix2: Set[(FragmentNode, FragmentNode)] = root2.createAntagonistsMatrix()
    assertEquals(Set(
      (FragmentNode(0,false),FragmentNode(1,false)),
      (FragmentNode(1,false),FragmentNode(0,false))), matrix2)
  }

  @Test
  def testCreateAntagonistsMatrix2(): Unit = {
    val root1 = DisjNode(List(FragmentNode(0), ConjNode(List(FragmentNode(1), FragmentNode(2)))))
    val matrix1: Set[(FragmentNode, FragmentNode)] = root1.createAntagonistsMatrix((f1, f2) => f1 == f2 || f1.id == 0 && f2.id == 1 || f1.id == 1 && f2.id == 0)
    assertTrue(matrix1.isEmpty)
  }

}
