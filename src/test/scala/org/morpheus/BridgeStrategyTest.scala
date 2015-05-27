package org.morpheus

import org.junit.Test

/**
 *
 * Created by zslajchrt on 22/03/15.
 */
class BridgeStrategyTest {

  class A
  class B
  class C
  class D

  @Test
  def testStrategy(): Unit = {

    // A with (B or C or D)
    val mainRoot = ConjNode(List(FragmentNode(0), DisjNode(List(FragmentNode(1), FragmentNode(2), FragmentNode(3)))))
    val subRoot = ConjNode(List(DisjNode(List(FragmentNode(0), FragmentNode(1)))))

    // TODO

//    // B or D
//    val mainModel = new MorphModelBase[Any](mainRoot) {
//      override val fragmentDescriptorsList: List[Frag[_, _]] = List(Frag.create[A, Unit](0), Frag.create[B, Unit](1), Frag.create[C, Unit](2), Frag.create[D, Unit](3))
//      override val lubComponents: Array[Class[_]] = Array.empty
//    }
//
//    val subModel = new MorphModelBase[Any](subRoot) {
//      override val fragmentDescriptorsList: List[Frag[_, _]] = List(Frag.create[B, Unit](0), Frag.create[D, Unit](1))
//      override val lubComponents: Array[Class[_]] = Array.empty
//    }
//
//    val strategy = BridgeStrategy[Any, Any](subModel, mainModel)
//
//    // The only valid alternatives from the main model are A with B and A with D
//    val expected = List((Set(FragmentNode(0), FragmentNode(1)/*B*/), 0.0d), (Set(FragmentNode(0), FragmentNode(3) /*D*/), 0.0d))
//    assertEquals(expected, strategy.alternatives)


  }

}
