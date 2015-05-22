package org.morpheus

import scala.reflect.macros.whitebox
import scala.reflect.runtime.universe._
import scala.language.experimental.macros

/**
 * Created by zslajchrt on 29/04/15.
 */

class Morpher[M]() {

  def morph(instance: CompositeInstance[M], strategy: MorpherStrategy[M])(owningMutableProxy: Option[instance.MutableLUB]): instance.ImutableLUB = {

    val alternatives: Alternatives[M] = strategy.chooseAlternatives(instance)(owningMutableProxy)
    val altsHolders: List[FragmentHolder[_]] = MorpherStrategy.fittestAlternative(instance, alternatives.toList) match {
      case None => sys.error("No alternative chosen")
      case Some(alternative) =>
        //strategy.convertToHolders(instance, alternative._1, alternative._2, None)
        // The instance's default strategy is responsible for the conversion to fragment holders.
        // This is important mainly because of the AltStrategy, which returns holders from another model.
        //instance.defaultStrategy.convertToHolders(instance, alternative._1, alternative._2, None)
        instance.altComposer.convertToHolders(instance, alternative._1, alternative._2, None)
      //alternative._1.map(fn => instance.fragmentHolder(fn).get)
    }

    owningMutableProxy match {
      case Some(proxy) if proxy.delegate != null && proxy.myAlternative == altsHolders =>
        // There is no need to instantiate a new proxy's delegate, provided that the delegate composition is same as the current one.
        proxy.delegate.asInstanceOf[instance.ImutableLUB]

      case _ =>

        // Instantiate new object from the current composition

        val filterChains: List[List[FragmentHolder[_]]] = altsHolders.foldLeft[List[List[FragmentHolder[_]]]](Nil)((res, holder) => res match {
          case hd :: tl if holder.fragment.wrapperAnnotation.isDefined =>
            val filterChain: List[FragmentHolder[_]] = holder :: hd
            filterChain :: tl
          case _ =>
            List(holder) :: res
        })

        val altsProxies: List[(List[Class[_]], _)] = filterChains.map(chain => {

          var top: Any = null

          val chainInst = chain.foldRight[(List[Class[_]], _)](null)((holder, res) => if (res == null) {
            (List(holder.fragment.fragmentClass), holder.proxy)
          } else {
            val chainInterface = holder.fragment.fragmentClass :: res._1
            (chainInterface, FilterChainProxy(res._2.asInstanceOf[AnyRef], chainInterface.toArray.reverse, holder.proxy.asInstanceOf[AnyRef], holder.fragment, top))
          })

          top = chainInst._2

          chainInst
        }).reverse

        //todo: Change it to a debug logging
        //println(s"Morph fragments: ${altsProxies.flatMap(_._1).map(_.getName)}")

        val compInst = CompositeFactory.newComposite[M, instance.LUB, instance.ConformLevel](altsProxies.toArray, instance, altsHolders, alternatives, strategy, owningMutableProxy)

        compInst.asInstanceOf[instance.ImutableLUB]

    }

  }

}

object Morpher {

  def hasFragment[F: WeakTypeTag](factor: (Boolean, Double) => Double) = FindFragment(implicitly[WeakTypeTag[F]].tpe, factor)

  //  def addRating[F: WeakTypeTag](factor: Double) = AddRating(implicitly[WeakTypeTag[F]].tpe, factor)

  //  def mulRating[F: WeakTypeTag](factor: Double) = MulRating(implicitly[WeakTypeTag[F]].tpe, factor)

  //  def fragRating[F: WeakTypeTag](factor: Double => Double) = FragmentRating(implicitly[WeakTypeTag[F]].tpe, factor)

  def activator[M](activator: PartialFunction[Frag[_, _], Boolean]) = new FragmentSelector[M](activator)

  def left[M] = new LeftAltsMorpherStrategy[M]()

  def right[M] = new RightAltsMorpherStrategy[M]()

  def morph[M](instance: CompositeInstance[M], customStrategy: Option[MorpherStrategy[M]])(owningMutableProxy: Option[instance.MutableLUB]): instance.ImutableLUB = {
    val strategy = customStrategy match {
      case None => instance.defaultStrategy
      case Some(custStrat) => custStrat
    }
    morph(instance, strategy)(owningMutableProxy)
  }

  def morph[M](instance: CompositeInstance[M], strategy: MorpherStrategy[M])(owningMutableProxy: Option[instance.MutableLUB]): instance.ImutableLUB = {
    new Morpher[M]().morph(instance, strategy)(owningMutableProxy)
  }

  def ?[F: WeakTypeTag](activator: Frag[_, _] => Boolean): PartialFunction[Frag[_, _], Boolean] = FragmentSelector.?[F](activator)

  //implicit def implicitMorpher[M, T]: Morpher[M, T] = macro MorpherMacros.implicitMorpher[M, T]
}

class MorpherMacros(val c: whitebox.Context) {

  import c.universe._

  def compositeStrategy[M: WeakTypeTag]: Tree = {
    val modelTag: WeakTypeTag[M] = implicitly[WeakTypeTag[M]]

    val partialStrategies: List[Tree] = modelTag.tpe match {
      case RefinedType(parents, _) =>
        parents.map(tp => {
          q"implicitly[MorpherStrategy[$tp]]"
        })
      case _ => c.abort(c.enclosingPosition, "Composite strategy supports refined types only: ie. T1 with T2 with ...")
    }

    //println(s"Composite stategy for model: $modelTag")

    val modelTp = modelTag.tpe.dealias
    q"""
        {
          import org.morpheus.Morpher._
          val partialStrategies = List(..$partialStrategies)
          left[$modelTp]
        }
    """
  }

}
