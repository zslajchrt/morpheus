package org.morpheus

/**
 *
 * Created by zslajchrt on 17/03/15.
 */
case class MockStrategy[M](alts: Alternatives[M]) extends MorphingStrategy[M] {
  def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]) = alts

}

