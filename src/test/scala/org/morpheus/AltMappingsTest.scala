package org.morpheus

import org.junit.Test
import org.junit.Assert._

/**
 * Created by zslajchrt on 13/04/15.
 */
class AltMappingsTest {

  @Test
  def testPickling(): Unit = {

    val am = AltMappings(Map(0 -> 1, 1 -> 2), Map(
      List() -> List(OrigAlt(List(0), List(OriginalInstanceSource(FragmentNode(1)))), OrigAlt(List(0, 1), List(PlaceholderSource(FragmentNode(2, true))))),
      List(0) -> List(OrigAlt(List(0, 1), Nil))
    ))

    val parser = new AltMappingsParser
    val parsed = parser.parseAltMap(am.toString).get
    assertEquals(am, parsed)

  }

  @Test
  def testCompose(): Unit = {
    /**
     *     Model 1:
     *
     *     0 -> 1
     *     1 -> 2
     *
     *     {O} -> {1}, {1, 2}
     *     {0, 1} -> {1}
     *
     *     Model 2:
     *
     *     1 -> 2
     *     2 -> 3
     *
     *     {1} -> {2}, {2, 3}
     *     {1, 2} -> {2}
     *
     *     Model 3 = Model1°Model2)
     *
     *     0 -> 2
     *     1 -> 3
     *
     *     {0} -> {2}, {2, 3}, {2} = {2}, {2, 3}
     *     {0, 1} -> {2}, {2, 3}
     *
     */


    val am1 = AltMappings(Map(0 -> 1, 1 -> 2), Map(
      List(0) -> List(OrigAlt(List(1), List(OriginalInstanceSource(FragmentNode(1)))), OrigAlt(List(1, 2), List(OriginalInstanceSource(FragmentNode(1)), OriginalInstanceSource(FragmentNode(2))))),
      List(0, 1) -> List(OrigAlt(List(1), List(OriginalInstanceSource(FragmentNode(1)))))
    ))

    val am2 = AltMappings(Map(1 -> 2, 2 -> 3), Map(
      List(1) -> List(OrigAlt(List(2), List(OriginalInstanceSource(FragmentNode(2)))), OrigAlt(List(2, 3), List(OriginalInstanceSource(FragmentNode(2)), OriginalInstanceSource(FragmentNode(3))))),
      List(1, 2) -> List(OrigAlt(List(2), List(OriginalInstanceSource(FragmentNode(2)))))
    ))

    val composed: AltMappings = am1.compose(am2)
    assertEquals("am(Map(0 -> 2, 1 -> 3), Map(List(0) -> List(a(List(2),List(o(2))), a(List(2, 3),List(o(2), o(3)))), List(0, 1) -> List(a(List(2),List(o(2))), a(List(2, 3),List(o(2), o(3))))))",
    composed.serialize)
  }

  //  @Test
  //  def testValidConversion1(): Unit = {
  //
  //    // [A with /?[B]] => /?[B]
  //    val am = AltMappings(Map(0 -> 1), Map(
  //      List() -> List(OrigAlt(List(0), Nil), OrigAlt(List(0, 1), Nil)),
  //      List(0) -> List(OrigAlt(List(0, 1), Nil))
  //    ))
  //
  //    val expected = AltMappings(Map(0 -> 1), Map(
  //      List() -> List(OrigAlt(List(0), Nil)),
  //      List(0) -> List(OrigAlt(List(0, 1), Nil))))
  //
  //    val pam = am.preserveDynamics()
  //    assertEquals(expected, pam)
  //
  //  }
  //
  //  @Test
  //  def testInvalidConversion1(): Unit = {
  //
  //    // [A with B] => /?[B]
  //    val am = AltMappings(Map(0 -> 1), Map(
  //      List() -> List(OrigAlt(List(0, 1), Nil)),
  //      List(0) -> List(OrigAlt(List(0, 1), Nil))
  //    ))
  //
  //    val expected = AltMappings(Map(0 -> 1), Map(
  //      List() -> Nil,
  //      List(0) -> List(OrigAlt(List(0, 1), Nil))))
  //
  //    val pam = am.preserveDynamics()
  //    assertEquals(expected, pam)
  //
  //  }
  //
  //  @Test
  //  def testValidConversion2(): Unit = {
  //
  //    // [A with /?[B]] => /?[B with $[C]]
  //    val am = AltMappings(Map(0 -> 1), Map(
  //      List() -> List(OrigAlt(List(0), Nil), OrigAlt(List(0, 1), Nil)),
  //      List(0, 1) -> List(OrigAlt(List(0, 1), Nil))
  //    ))
  //
  //    val expected = AltMappings(Map(0 -> 1), Map(
  //      List() -> List(OrigAlt(List(0), Nil)),
  //      List(0, 1) -> List(OrigAlt(List(0, 1), Nil))))
  //
  //    val pam = am.preserveDynamics()
  //    assertEquals(expected, pam)
  //
  //  }
  //
  //  @Test
  //  def testInvalidConversion2(): Unit = {
  //
  //    // [A with B] => /?[B with $[C]]
  //    val am = AltMappings(Map(0 -> 1), Map(
  //      List() -> List(OrigAlt(List(0, 1), Nil)),
  //      List(0, 1) -> List(OrigAlt(List(0, 1), Nil))
  //    ))
  //
  //    val expected = AltMappings(Map(0 -> 1), Map(
  //      List() -> Nil,
  //      List(0, 1) -> List(OrigAlt(List(0, 1), Nil))))
  //
  //    val pam = am.preserveDynamics()
  //    assertEquals(expected, pam)
  //
  //  }
  //
  //  @Test
  //  def testValidConversion3(): Unit = {
  //
  //    // [/?[A] with /?[B]] => /?[B] with /?[$[C]]
  //    val am = AltMappings(Map(0 -> 1), Map(
  //      List() -> List(OrigAlt(Nil, Nil), OrigAlt(List(0), Nil), OrigAlt(List(1), Nil), OrigAlt(List(0, 1), Nil)),
  //      List(0) -> List(OrigAlt(List(1), Nil), OrigAlt(List(0, 1), Nil)),
  //      List(1) -> List(OrigAlt(Nil, Nil), OrigAlt(List(0), Nil), OrigAlt(List(1), Nil), OrigAlt(List(0, 1), Nil)),
  //      List(0, 1) -> List(OrigAlt(List(1), Nil), OrigAlt(List(0, 1), Nil))
  //    ))
  //
  //    val expected = AltMappings(Map(0 -> 1), Map(
  //      List() -> List(OrigAlt(Nil, Nil), OrigAlt(List(0), Nil)),
  //      List(0) -> List(OrigAlt(List(1), Nil), OrigAlt(List(0, 1), Nil)),
  //      List(1) -> List(OrigAlt(Nil, Nil), OrigAlt(List(0), Nil)),
  //      List(0, 1) -> List(OrigAlt(List(1), Nil), OrigAlt(List(0, 1), Nil))
  //    ))
  //
  //    val pam = am.preserveDynamics()
  //    assertEquals(expected, pam)
  //
  //  }
  //
  //  @Test
  //  def testInvalidConversion3(): Unit = {
  //
  //    // /?[A] with B => A with /?[B]
  //    val am = AltMappings(Map(0 -> 1, 1 -> 0), Map(
  //      List(0) -> List(OrigAlt(List(0, 1), Nil)),
  //      List(0, 1) -> List(OrigAlt(List(0, 1), Nil))
  //    ))
  //
  //    val expected = AltMappings(Map(0 -> 1, 1 -> 0), Map(
  //      List(0) -> Nil,
  //      List(0, 1) -> List(OrigAlt(List(0, 1), Nil))
  //    ))
  //
  //    val pam = am.preserveDynamics()
  //    assertEquals(expected, pam)
  //
  //  }
  //
  //  @Test
  //  def testValidConversion4(): Unit = {
  //
  //    // /?[A with B] => /?[B] with /?[$[C]]
  //    val am = AltMappings(Map(0 -> 1), Map(
  //      List() -> List(OrigAlt(Nil, Nil), OrigAlt(List(0, 1), Nil)),
  //      List(0) -> List(OrigAlt(List(0, 1), Nil)),
  //      List(1) -> List(OrigAlt(Nil, Nil), OrigAlt(List(0, 1), Nil)),
  //      List(0, 1) -> List(OrigAlt(List(0, 1), Nil))
  //    ))
  //
  //    val expected = AltMappings(Map(0 -> 1), Map(
  //      List() -> List(OrigAlt(Nil, Nil)),
  //      List(0) -> List(OrigAlt(List(0, 1), Nil)),
  //      List(1) -> List(OrigAlt(Nil, Nil)),
  //      List(0, 1) -> List(OrigAlt(List(0, 1), Nil))
  //    ))
  //
  //    val pam = am.preserveDynamics()
  //    assertEquals(expected, pam)
  //
  //  }
  //
  //  @Test
  //  def testValidConversion5(): Unit = {
  //
  //    // A or B => A or B or $[C]
  //    val am = AltMappings(Map(0 -> 0, 1 -> 1), Map(
  //      List(0) -> List(OrigAlt(List(0), Nil)),
  //      List(1) -> List(OrigAlt(List(1), Nil)),
  //      List(2) -> List(OrigAlt(Nil, Nil), OrigAlt(List(0), Nil), OrigAlt(List(1), Nil))
  //    ))
  //
  //    val expected = AltMappings(Map(0 -> 0, 1 -> 1), Map(
  //      List(0) -> List(OrigAlt(List(0), Nil)),
  //      List(1) -> List(OrigAlt(List(1), Nil)),
  //      List(2) -> List(OrigAlt(Nil, Nil))
  //    ))
  //
  //    val pam = am.preserveDynamics()
  //    assertEquals(expected, pam)
  //
  //  }

}
