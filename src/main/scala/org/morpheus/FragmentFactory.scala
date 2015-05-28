package org.morpheus

/**
 *
 * Created by zslajchrt on 28/05/15.
 */

class FragmentFactory[F, C](cfgOpt: Option[C]) extends (Frag[F, C] => F) {
//  def this(cfg: C) = this(if (cfg ==()) None else Some(cfg))

  override def apply(frag: Frag[F, C]): F = ReflectHelper.newFragment(frag, cfgOpt)
}


trait SingletonFragmentFactory[F, C] extends (Frag[F, C] => F) {
  private[this] var cachedProxy: Option[F] = None

  abstract override def apply(frag: Frag[F, C]): F = cachedProxy match {
    case None =>
      cachedProxy = Some(super.apply(frag))
      cachedProxy.get
    case Some(cached) =>
      cached
  }
}
