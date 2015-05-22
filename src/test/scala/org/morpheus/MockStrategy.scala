package org.morpheus

import org.morpheus.{Alternatives, CompositeInstance, MorpherStrategy}

/**
 *
 * Created by zslajchrt on 17/03/15.
 */
case class MockStrategy[M](alts: Alternatives[M]) extends MorpherStrategy[M] {
  def chooseAlternatives(instance: CompositeInstance[M])(owningMutableProxy: Option[instance.MutableLUB]) = alts

}

