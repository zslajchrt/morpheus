package org.morpheus

import org.junit.Assert._
import org.junit.Test

/**
 * Created by zslajchrt on 17/03/15.
 */
class MorphingStrategyTest {

  @Test
  def testFittestAlternative(): Unit = {
    val alts = List(
      (List(FragmentNode(0), FragmentNode(1)), 0.5d),
      (List(FragmentNode(0), FragmentNode(2)), 1d),
      (List(FragmentNode(0), FragmentNode(3)), 0d)
    )

    assertEquals(Some(List(FragmentNode(0), FragmentNode(2)), 1d), MorphingStrategy.fittestAlternative(null, alts))
  }
}
