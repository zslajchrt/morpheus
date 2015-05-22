package org.morpheus

/**
 * Created by zslajchrt on 29/04/15.
 */

object FragmentHolder {
  def apply[F, C](frag: F, fragDesc: Frag[F, C]): FragmentHolder[F] = new FragmentHolder[F] {
    override def proxy: F = frag

    override val fragment: Frg = fragDesc
  }
}

abstract class FragmentHolder[F] {
  outer =>

  type Frg = Frag[F, _]
  val fragment: Frg

  def proxy: F

  def renumber(newFragId: Int) = {
    new FragmentHolder[F] {

      def proxy: F = outer.proxy

      val fragment: Frg = outer.fragment.copy(index = newFragId)

    }
  }
}

