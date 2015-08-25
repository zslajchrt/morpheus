package org.morpheus

import scala.annotation.tailrec
import scala.reflect.macros.whitebox
import scala.reflect.runtime.universe._
import scala.language.experimental.macros

/**
 * Created by zslajchrt on 29/04/15.
 */

class Morpher[M]() {

  def morph(instance: MorphKernel[M], strategy: MorphingStrategy[M], altFailover: Boolean = true)(owningMutableProxy: Option[instance.MutableLUB]): instance.ImmutableLUB = {

    val alternatives: Alternatives[M] = owningMutableProxy match {
      case Some(proxy) if proxy.delegate != null => strategy.chooseAlternatives(instance)(Some(proxy))
      case _ => strategy.chooseAlternatives(instance)(None)
    }

    @tailrec
    def makeFragHolders(candidates: List[(List[FragmentNode], Double)]): List[FragmentHolder[_]] = {
      try {
        MorphingStrategy.fittestAlternative(instance, candidates) match {
          case None => throw new NoAlternativeChosenException("No alternative can be chosen")
          case Some(alternative) =>
            instance.altComposer.convertToHolders(instance, alternative._1, alternative._2, None)
        }
      }
      catch {
        case ae: AlternativeNotAvailableException => if (altFailover) {
          val failedAlt = ae.alt
          val newCandidates = candidates.filterNot(_._1 == failedAlt)
          if (newCandidates.isEmpty) {
            throw ae
          } else {
            // try it again without the failed candidate alt
            makeFragHolders(newCandidates)
          }
        } else {
          throw ae
        }
      }
    }

    val altCandidates: List[(List[FragmentNode], Double)] = alternatives.toMaskedList
    val altFragHolders = makeFragHolders(altCandidates)

    owningMutableProxy match {
      case Some(proxy) if proxy.delegate != null && proxy.myAlternative == altFragHolders && proxy.strategy == strategy =>
        // There is no need to instantiate a new proxy's delegate, provided that the delegate composition is same as the current one.
        proxy.delegate

      case _ =>

        // Instantiate new object from the current composition
        def partitionHolders(holders: List[FragmentHolder[_]]): List[List[FragmentHolder[_]]] = if (holders == Nil) {
          Nil
        } else {
          val hd = holders.head.fragment
          val partitioned: (List[FragmentHolder[_]], List[FragmentHolder[_]]) = holders.partition(_.fragment.sameWrapperGroup(hd))
          partitioned._1 :: partitionHolders(partitioned._2)
        }

        val filterChains: List[List[FragmentHolder[_]]] = partitionHolders(altFragHolders)

//        val filterChains2: List[List[FragmentHolder[_]]] = altFragHolders.foldLeft[List[List[FragmentHolder[_]]]](Nil)((res, holder) => res match {
//          case hd :: tl if holder.fragment.wrapperAnnotation.isDefined =>
//            val filterChain: List[FragmentHolder[_]] = holder :: hd
//            filterChain :: tl
//          case _ =>
//            List(holder) :: res
//        })

        val altsProxies: List[(List[Class[_]], _)] = filterChains.map(chain => {

          var top: Any = null

          val chainInst = chain.foldLeft[(List[Class[_]], _)](null)((res, holder) => if (res == null) {
            (List(holder.fragment.fragmentClass), holder.proxy)
          } else {
            val chainInterface = holder.fragment.fragmentClass :: res._1
            (chainInterface, FilterChainProxy(res._2.asInstanceOf[AnyRef], chainInterface.toArray, holder.proxy.asInstanceOf[AnyRef], holder.fragment, top))
          })

          top = chainInst._2

          chainInst
        })

        //todo: Change it to a debug logging
        //println(s"Morph fragments: ${altsProxies.flatMap(_._1).map(_.getName)}")

        val compInst = MorphFactory.newComposite[M, instance.LUB, instance.ConformLevel](altsProxies.toArray, instance, altFragHolders, alternatives, strategy)(owningMutableProxy)

        compInst.asInstanceOf[instance.ImmutableLUB]

    }

  }

}

object Morpher {

  def hasFragment[F: WeakTypeTag](factor: (Boolean, Double) => Double) = FindFragment(implicitly[WeakTypeTag[F]].tpe, factor)

  //  def addRating[F: WeakTypeTag](factor: Double) = AddRating(implicitly[WeakTypeTag[F]].tpe, factor)

  //  def mulRating[F: WeakTypeTag](factor: Double) = MulRating(implicitly[WeakTypeTag[F]].tpe, factor)

  //  def fragRating[F: WeakTypeTag](factor: Double => Double) = FragmentRating(implicitly[WeakTypeTag[F]].tpe, factor)

  def activator[M](activator: PartialFunction[Frag[_, _], Boolean]) = new FragmentSelector[M](activator)

  def left[M] = new LeftAltsMorphingStrategy[M]()

  def right[M] = new RightAltsMorphingStrategy[M]()

  def morph[M](instance: MorphKernel[M], customStrategy: Option[MorphingStrategy[M]])(owningMutableProxy: Option[instance.MutableLUB]): instance.ImmutableLUB = {
    val strategy = customStrategy match {
      case None => instance.defaultStrategy
      case Some(custStrat) => custStrat
    }
    morph(instance, strategy)(owningMutableProxy)
  }

  def morph[M](instance: MorphKernel[M], strategy: MorphingStrategy[M])(owningMutableProxy: Option[instance.MutableLUB]): instance.ImmutableLUB = {
    new Morpher[M]().morph(instance, strategy, altFailover = true)(owningMutableProxy)
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
          q"implicitly[MorphingStrategy[$tp]]"
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
