package org.morpheus

/**
 * Created by zslajchrt on 29/04/15.
 */
sealed trait FragmentProvider

case object FactoryProvider extends FragmentProvider

case object SingletonProvider extends FragmentProvider

//case object ConfiguratorProvider extends FragmentProvider
case object InstanceProvider extends FragmentProvider

/**
 * This class is used as a carrier of objects needed for two operations: dereference and completion.
 * In both cases a new composite instance refers to another one carried in the `src` field.
 * See Morpheus.complete, Morpheus.deref macros.
 * @param src
 * @param placeholderFactMap
 * @param conformanceLevel
 * @param conformanceLevelTpe
 * @param delegation if true the `src` field carries an instance of `MorphKernelBase`, otherwise it carries a `MorphKernelRef`.
 *
 */
case class CopyProvider(src: Any, placeholderFactMap: Any, conformanceLevel: Morpheus.ConformanceLevel, conformanceLevelTpe: Any, delegation: Boolean) extends FragmentProvider

//case class ForkProvider(src1: Any, src2: Any) extends FragmentProvider

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
