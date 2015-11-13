package org.morpheus

import org.junit.Test
import org.junit.Assert._

/**
 * Created by zslajchrt on 12/11/15.
 */
class FragmentsHelperTest {

  @Test
  def testMapSecondaryToPrimary(): Unit = {
    val orig: Map[Int, String] = Map(3 -> "a", 2 -> "b", 1 -> "a")
    val prim2second: Map[Int, Int] = new FragmentsHelper[String, String](orig, (a) => a).mapSecondaryToPrimary()

    assertEquals(Map(1 -> 1, 3 -> 1, 2 -> 2), prim2second)
  }

  @Test
  def testRemoveDuplicateFragments(): Unit = {
    var orig: Map[Int, String] = Map(3 -> "a", 2 -> "b", 1 -> "a")
    var fragmentsHelper: FragmentsHelper[String, String] = new FragmentsHelper[String, String](orig, a => a)

    var result: List[FragmentNode] = fragmentsHelper.removeDuplicateFragments(List(FragmentNode(1), FragmentNode(2), FragmentNode(3)))
    assertEquals(List(FragmentNode(1), FragmentNode(2)), result)

    result = fragmentsHelper.removeDuplicateFragments(List(FragmentNode(2), FragmentNode(3)))
    assertEquals(List(FragmentNode(2), FragmentNode(1)), result)

    result = fragmentsHelper.removeDuplicateFragments(List(FragmentNode(1), FragmentNode(2)))
    assertEquals(List(FragmentNode(1), FragmentNode(2)), result)

    result = fragmentsHelper.removeDuplicateFragments(List(FragmentNode(1), FragmentNode(3)))
    assertEquals(List(FragmentNode(1)), result)

    result = fragmentsHelper.removeDuplicateFragments(List(FragmentNode(1)))
    assertEquals(List(FragmentNode(1)), result)

    result = fragmentsHelper.removeDuplicateFragments(List(FragmentNode(3)))
    assertEquals(List(FragmentNode(1)), result)

    //(A with D1) or (A with D2) or B
    orig = Map(0 -> "A", 1 -> "D1", 2 -> "A", 3 -> "D2", 4 -> "B")
    fragmentsHelper = new FragmentsHelper[String, String](orig, a => a)
    result = fragmentsHelper.removeDuplicateFragments(List(FragmentNode(2), FragmentNode(3)))
    assertEquals(List(FragmentNode(0), FragmentNode(3)), result)

  }

  def testFilteredTypesMap(): Unit = {
    val orig: Map[Int, String] = Map(3 -> "a", 2 -> "b", 1 -> "a")
    val fragmentsHelper: FragmentsHelper[String, String] = new FragmentsHelper[String, String](orig, a => a)
    assertEquals(Map(2 -> ("b", None), 1 -> ("a", None)), fragmentsHelper.filteredTypesMap)
  }

  @Test
  def testCreateAntagonistsMatrix(): Unit = {
    val root1 = DisjNode(List(FragmentNode(0), FragmentNode(1), FragmentNode(2)))
    val fragHelper = new FragmentsHelper[Int, Int](Map(0 -> 0, 1 -> 1, 2 -> 2), a => a)
    val matrix1: Set[(FragmentNode, FragmentNode)] = fragHelper.createAntagonistsMatrix(root1)
    assertEquals(Set(
      (FragmentNode(0, false), FragmentNode(1, false)),
      (FragmentNode(0, false), FragmentNode(2, false)),
      (FragmentNode(1, false), FragmentNode(0, false)),
      (FragmentNode(1, false), FragmentNode(2, false)),
      (FragmentNode(2, false), FragmentNode(0, false)),
      (FragmentNode(2, false), FragmentNode(1, false))), matrix1)

    val root2 = ConjNode(List(DisjNode(List(FragmentNode(0), FragmentNode(1))), FragmentNode(2)))
    val matrix2: Set[(FragmentNode, FragmentNode)] = fragHelper.createAntagonistsMatrix(root2)
    assertEquals(Set(
      (FragmentNode(0, false), FragmentNode(1, false)),
      (FragmentNode(1, false), FragmentNode(0, false))), matrix2)
  }

  @Test
  def testCreateAntagonistsMatrix2(): Unit = {
    val root1 = DisjNode(List(FragmentNode(0), ConjNode(List(FragmentNode(1), FragmentNode(2)))))
    val fragHelper = new FragmentsHelper[Int, Int](Map(0 -> 0, 1 -> 0, 2 -> 2), a => a)
    //val matrix1: Set[(FragmentNode, FragmentNode)] = root1.createAntagonistsMatrix((f1, f2) => f1 == f2 || f1.id == 0 && f2.id == 1 || f1.id == 1 && f2.id == 0)
    val matrix1: Set[(FragmentNode, FragmentNode)] = fragHelper.createAntagonistsMatrix(root1)
    assertTrue(matrix1.isEmpty)
  }


}
