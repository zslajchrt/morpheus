package org.morpheus

import org.morpheus.AltMappingModel._
import org.morpheus._
import org.junit.Assert._
import org.junit.Test
import scala.reflect.runtime.universe._


/**
 *
 * Created by zslajchrt on 01/04/15.
 */
class AltMappingModelTest {

  @Test
  def testBasicConversionFromFragSources(): Unit = {

    val holderFn_OK: FragmentNode => FragmentHolder[_] = fn => null
    val holderFn_Failing: FragmentNode => FragmentHolder[_] = fn => {
      fail(); null
    }

    var fragSources: List[FragInstSource] = List(OriginalInstanceSource(FragmentNode(0)))
    var altNodes: List[Node] = AltMappingModel.apply(fragSources, Map(), holderFn_Failing, holderFn_OK)
    assertEquals(1, altNodes.size)
    altNodes.head match {
      case h@Hidden(_) => assertNull(h.holder())
      case _ => fail()
    }

    fragSources = List(OriginalInstanceSource(FragmentNode(0)))
    altNodes = AltMappingModel.apply(fragSources, Map(1 -> 0), holderFn_Failing, holderFn_OK)
    assertEquals(1, altNodes.size)
    altNodes.head match {
      case r@Reference(1, 0, _) => assertNull(r.holder())
      case _ => fail()
    }

    // leading placeholders are fragment insertions if there is no counterpart
    fragSources = List(PlaceholderSource(FragmentNode(1)))
    altNodes = AltMappingModel.apply(fragSources, Map() /*no counterparts*/ , holderFn_OK, holderFn_Failing)
    assertEquals(1, altNodes.size)
    altNodes.head match {
      case fi@FragmentInsertion(_) => assertNull(fi.holder())
      case _ => fail()
    }

    fragSources = List(PlaceholderSource(FragmentNode(1)))
    altNodes = AltMappingModel.apply(fragSources, Map(1 -> 0), holderFn_OK, holderFn_Failing)
    assertEquals(1, altNodes.size)
    altNodes.head match {
      case repl@Replacement(1, 0, _) => assertNull(repl.holder())
      case _ => fail()
    }

    fragSources = List(PlaceholderSource(FragmentNode(0)), PlaceholderSource(FragmentNode(1)))
    altNodes = AltMappingModel.apply(fragSources, Map(0 -> 0), holderFn_OK, holderFn_Failing)
    assertEquals(2, altNodes.size)
    altNodes.head match {
      case Replacement(0, 0, _) => // OK
      case _ => fail()
    }
    altNodes(1) match {
      case wi@WrapperInsertion(_) => assertNull(wi.holder())
      case _ => fail()
    }

    fragSources = List(OriginalInstanceSource(FragmentNode(0)), PlaceholderSource(FragmentNode(1)))
    altNodes = AltMappingModel.apply(fragSources, Map(), holderFn_OK, holderFn_Failing)
    assertEquals(2, altNodes.size)
    altNodes.head match {
      case Hidden(_) => // OK
      case _ => fail()
    }
    altNodes(1) match {
      case wi@WrapperInsertion(_) => assertNull(wi.holder())
      case _ => fail()
    }

  }

  @Test
  def testMoreConversionsFromFragSources(): Unit = {
    var fragSources: List[FragInstSource] = List(
      PlaceholderSource(FragmentNode(0)), // -> fragment insertion
      PlaceholderSource(FragmentNode(1)), // -> replacement
      OriginalInstanceSource(FragmentNode(0)), // -> reference
      OriginalInstanceSource(FragmentNode(1)), // -> hidden
      PlaceholderSource(FragmentNode(2)))  // -> wrapper insertion
    var altNodes: List[Node] = AltMappingModel.apply(fragSources, Map(1 -> 10, 3 -> 0), fn => null, fn => null)
    assertEquals(5, altNodes.size)

    val List(FragmentInsertion(_), Replacement(1, 10, _), Reference(3, 0, _), Hidden(_), WrapperInsertion(_)) = altNodes
  }

  def holderFn[F: WeakTypeTag](cb: => Unit): () => FragmentHolder[_] = () => {
    cb
    new FragmentHolder[F] {
      override def proxy: Nothing = ???

      private val fragTag = implicitly[WeakTypeTag[F]]
      private val fragIndex = TestFragments.fragments.indexOf(fragTag)

      override val fragment: Frg = Frag[F, Unit](fragIndex, fragTag, weakTypeTag[Unit], None, None, None)
    }
  }

  @Test
  def testTransformCase1(): Unit = {

    val hf1 = holderFn[Frag0]{}
    val hf2 = holderFn[Frag1]{}

    val model1 = List(FragmentInsertion(hf1))
    val model2 = List(FragmentInsertion(hf2))

    val transformed: List[Node] = transform(model1, model2)
    val List(FragmentInsertion(ahf1), FragmentInsertion(ahf2)) = transformed
    assertSame(hf1, ahf1)
    assertSame(hf2, ahf2)
  }

  @Test
  def testTransformCase2(): Unit = {
    val hf1 = holderFn[Frag0]{}
    val hf2 = holderFn[Frag1]{}

    val model1 = List(FragmentInsertion(hf1))
    val model2 = List(Hidden(hf2))

    val transformed: List[Node] = transform(model1, model2)
    val List(FragmentInsertion(ahf1), Hidden(ahf2)) = transformed
    assertSame(hf1, ahf1)
    assertSame(hf2, ahf2)
  }

  @Test
  def testTransformCase3(): Unit = {
    val hf1 = holderFn[Frag0]{}
    val hf2 = holderFn[Frag1]{}

    val model1 = List(Reference(0, 1, hf1))
    val model2 = List(Reference(1, 2, hf2))

    val transformed: List[Node] = transform(model1, model2)
    val List(Reference(0, 2, ahf2)) = transformed
    assertSame(hf2, ahf2)
  }

  @Test
  def testTransformCase4(): Unit = {
    val hf1 = holderFn[Frag0]{}
    val hf2 = holderFn[Frag1]{}

    val model1 = List(Replacement(0, 1, hf1))
    val model2 = List(Reference(1, 2, hf2))

    val transformed: List[Node] = transform(model1, model2)
    val List(Replacement(0, 2, ahf1)) = transformed
    assertSame(hf1, ahf1)
  }

  @Test
  def testTransformCase5(): Unit = {
    val hf1 = holderFn[Frag0]{}
    val hf2 = holderFn[Frag1]{}

    val model1 = List(Reference(0, 1, hf1))
    val model2 = List(Replacement(1, 2, hf2))

    val transformed: List[Node] = transform(model1, model2)
    val List(Replacement(0, 2, ahf2)) = transformed
    assertSame(hf2, ahf2)
  }

  @Test
  def testTransformCase6(): Unit = {
    val hf1 = holderFn[Frag0]{}
    val hf2 = holderFn[Frag1]{}

    val model1 = List(Replacement(0, 1, hf1))
    val model2 = List(Replacement(1, 2, hf2))

    val transformed: List[Node] = transform(model1, model2)
    val List(Replacement(0, 2, ahf1)) = transformed
    assertSame(hf1, ahf1)
  }

  @Test
  def testTransformCase7(): Unit = {
    val hf1 = holderFn[Frag0]{}
    val hf2 = holderFn[Frag1]{}
    val hf3 = holderFn[Frag2]{}
    val hf4 = holderFn[Frag3]{}

    val model1 = List(Replacement(0, 1, hf1), WrapperInsertion(hf3), WrapperInsertion(hf4))
    val model2 = List(Replacement(1, 2, hf2))

    val transformed: List[Node] = transform(model1, model2)
    val List(Replacement(0, 2, ahf1), WrapperInsertion(ahf3), WrapperInsertion(ahf4)) = transformed
    assertSame(hf1, ahf1)
    assertSame(hf3, ahf3)
    assertSame(hf4, ahf4)
  }

  @Test
  def testTransformMoreComplex(): Unit = {
    val hf0 = holderFn[Frag0]{}
    val hf1 = holderFn[Frag1]{}
    val hf2 = holderFn[Frag2]{}
    val hf3 = holderFn[Frag3]{}
    val hf4 = holderFn[Frag4]{}
    val hf5 = holderFn[Frag5]{}
    val hf6 = holderFn[Frag6]{}
    val hf7 = holderFn[Frag7]{}
    val hf8 = holderFn[Frag8]{}

    val model1 = List(FragmentInsertion(hf0), Replacement(0, 1, hf1), Reference(1, 2, hf2), WrapperInsertion(hf3))
    val model2 = List(FragmentInsertion(hf4), Reference(1, 2, hf5), Reference(2, 3, hf6), Hidden(hf7), WrapperInsertion(hf8))

    val transformed: List[Node] = transform(model1, model2)
    val List(FragmentInsertion(ahf0), FragmentInsertion(ahf4), Replacement(0, 2, ahf1), Reference(1, 3, ahf6), Hidden(ahf7), WrapperInsertion(ahf8), WrapperInsertion(ahf3)) = transformed
    assertSame(hf0, ahf0)
    assertSame(hf4, ahf4)
    assertSame(hf1, ahf1)
    assertSame(hf6, ahf6)
    assertSame(hf3, ahf3)
    assertSame(hf7, ahf7)
    assertSame(hf8, ahf8)
  }


  @Test
  def testTransformTwoTransformations(): Unit = {
    val hf0 = holderFn[Frag0]{}
    val hf1 = holderFn[Frag1]{}
    val hf2 = holderFn[Frag2]{}
    val hf3 = holderFn[Frag3]{}
    val hf4 = holderFn[Frag4]{}
    val hf5 = holderFn[Frag5]{}
    val hf6 = holderFn[Frag6]{}
    val hf7 = holderFn[Frag7]{}
    val hf8 = holderFn[Frag8]{}
    val hf9 = holderFn[Frag9]{}
    val hf10 = holderFn[Frag10]{}
    val hf11 = holderFn[Frag11]{}
    val hf12 = holderFn[Frag12]{}
    val hf13 = holderFn[Frag13]{}

    val model1 = List(FragmentInsertion(hf0), Replacement(0, 1, hf1), Reference(1, 2, hf2), WrapperInsertion(hf3))
    val model2 = List(FragmentInsertion(hf4), Reference(1, 2, hf5), Reference(2, 3, hf6), Hidden(hf8), WrapperInsertion(hf7))
    val model3 = List(FragmentInsertion(hf9), Reference(2, 4, hf10), Reference(3, 4, hf11), Hidden(hf12), WrapperInsertion(hf13))

    val transformed1: List[Node] = transform(model1, model2)
    val transformed2: List[Node] = transform(transformed1, model3)

    val List(FragmentInsertion(ahf0), FragmentInsertion(ahf4), FragmentInsertion(ahf9), Replacement(0, 4, ahf1), Reference(1, 4, ahf11), Hidden(ahf12), WrapperInsertion(ahf13), WrapperInsertion(ahf7), WrapperInsertion(ahf3)) = transformed2
    assertSame(hf0, ahf0)
    assertSame(hf4, ahf4)
    assertSame(hf9, ahf9)
    assertSame(hf1, ahf1)
    assertSame(hf11, ahf11)
    assertSame(hf3, ahf3)
    assertSame(hf7, ahf7)
    assertSame(hf12, ahf12)
    assertSame(hf13, ahf13)
  }

}

class Frag0{}
class Frag1{}
class Frag2{}
class Frag3{}
class Frag4{}
class Frag5{}
class Frag6{}
class Frag7{}
class Frag8{}
class Frag9{}
class Frag10{}
class Frag11{}
class Frag12{}
class Frag13{}

object TestFragments {
  val fragments = List(
    weakTypeTag[Frag0],
    weakTypeTag[Frag1],
    weakTypeTag[Frag2],
    weakTypeTag[Frag3],
    weakTypeTag[Frag4],
    weakTypeTag[Frag5],
    weakTypeTag[Frag6],
    weakTypeTag[Frag7],
    weakTypeTag[Frag8],
    weakTypeTag[Frag9],
    weakTypeTag[Frag10],
    weakTypeTag[Frag11],
    weakTypeTag[Frag12],
    weakTypeTag[Frag13]
  )
}