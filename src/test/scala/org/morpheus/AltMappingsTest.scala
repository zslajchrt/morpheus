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
