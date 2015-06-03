package org.morpheus

import org.morpheus.AltMappingModel.Node

import scala.reflect.runtime.universe._
import scala.util.DynamicVariable
import scala.language.experimental.macros

/**
 *
 * Created by zslajchrt on 29/04/15.
 */
trait MorphingStrategy[M] {

  type Model = M

  def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M]

}

trait AlternativeComposer[M] {

  def convertToHolders(instance: MorphKernel[M], alternative: List[FragmentNode], rating: Double, altStruct: Option[List[Node]]): List[FragmentHolder[_]]

}

class DefaultAlternativeComposer[M] extends AlternativeComposer[M] {

  def convertToHolders(instance: MorphKernel[M], alternative: List[FragmentNode], rating: Double, altStruct: Option[List[Node]]): List[FragmentHolder[_]] = {
    altStruct match {
      case None =>
        alternative.map(fn => instance.fragmentHolder(fn).get)
      case Some(altStructTemplate) => altStructTemplate.map(_.holder())
    }
  }

}

case class FixedStrategy[M](alternatives: Alternatives[M]) extends MorphingStrategy[M] {

  override def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M] = alternatives

}

case class LastRatingStrategy[M](mirrorOpt: Option[MorphMirror[M]]) extends MorphingStrategy[M] {

  def this() = this(None)

  def this(mirror: MorphMirror[M]) = this(Some(mirror))

  override def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M] = mirrorOpt match {
    case Some(mirror) => mirror.alternatives
    case None => owningMutableProxy match {
      case None => instance.model.alternatives
      case Some(proxy) =>
          if (proxy.delegate == null)
            instance.model.alternatives
          else
            proxy.alternatives
    }
  }

}

case class Normalizer[M](delegate: MorphingStrategy[M], minRating: Double = 0, maxRating: Double = 1) extends MorphingStrategy[M] {

  override def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M] = {

    var minFound: Option[Double] = None
    var maxFound: Option[Double] = None

    val origAlts: Alternatives[M] = delegate.chooseAlternatives(instance)(owningMutableProxy)
    val alternatives: List[(List[FragmentNode], Double)] = origAlts.toList

    for (alt <- alternatives) {
      minFound = minFound match {
        case None => Some(alt._2)
        case Some(m) => Some(math.min(m, alt._2))
      }

      maxFound = maxFound match {
        case None => Some(alt._2)
        case Some(m) => Some(math.max(m, alt._2))
      }

    }

    val oldMin = minFound.get
    val oldMax = maxFound.get

    def newRating(oldRating: Double) = {
      if (oldMax - oldMin > 0)
        ((oldRating - oldMin) / (oldMax - oldMin)) * (maxRating - minRating) + minRating
      else
        minRating
    }

    origAlts.rate((alt, origRating) => newRating(origRating))

  }

}

case class RootStrategy[M]() extends MorphingStrategy[M] {
  override def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M] = instance.model.alternatives
}

object RootStrategy {
  def apply[M](model: MorphModel[M]) = new RootStrategy[M]()
}

case class RatingStrategy[M](delegate: MorphingStrategy[M], ratingOperators: ((List[FragmentHolder[_]], Double) => Double)*) extends MorphingStrategy[M] {

  def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M] = {

    def calculateRating(alt: List[FragmentHolder[_]], origRating: Double): Double = {
      ratingOperators.foldRight(origRating)((ratOper, res) => ratOper(alt, res))
    }

    val origRatedAlts = delegate.chooseAlternatives(instance)(owningMutableProxy)
    val newRatedAlts = origRatedAlts.rate((origAlt, origRating) => {
      // replace the frag nodes for frag holders
      val altWithHolders = origAlt.map(frag => instance.fragmentHolder(frag).get)
      val newRating = calculateRating(altWithHolders, origRating)
      newRating
    })

    newRatedAlts
  }

}

case class AltMapRatingStrategy[M, S](delegate: MorphingStrategy[M], switchModel: MorphModel[S], altMap: AltMappings, switchFn: () => Set[(Int, Int)], negative: Boolean) extends MorphingStrategy[M] {

  val switchAlts: List[List[Int]] = switchModel.altIterator().toList.map(_.map(_.id))
  val altMapReduced = altMap.preserveDynamics()

  val altsCount = altMap.newAltToOrigAlt.size

  def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M] = {
    val origAlts = delegate.chooseAlternatives(instance)(owningMutableProxy)

    val altRatings: Set[(Int, Int)] = switchFn()
    altRatings.foldLeft(origAlts)((alts, altRat) => {
      rateAlts(alts, altRat._1, altRat._2)
    })

  }

  private def rateAlts(origAlts: Alternatives[M], altIdx: Int, altRating: Int): Alternatives[M] = {

      val activeAltIndex = altIdx % altsCount
      val activeAlt: List[Int] = switchAlts(activeAltIndex)
      val activeOrigAlts: Set[List[Int]] = altMapReduced.newAltToOrigAlt(activeAlt).map(_.fragments).toSet

      origAlts.rate((origAltFrags, origRating) => {
        val origAlt = origAltFrags.map(_.id)
        val newRating = if (activeOrigAlts.contains(origAlt)) {
          if (negative) origRating else altRating
        } else {
          if (negative) altRating else origRating
        }
        newRating
      })

  }
}

case class PromotingStrategy[M, S](delegate: MorphingStrategy[M], switchModel: MorphModel[S], altMap: AltMappings, switchFn: () => Option[Int]) extends MorphingStrategy[M] {

  val switchAlts: List[List[Int]] = switchModel.altIterator().toList.map(_.map(_.id))
  val altMapReduced = altMap.preserveDynamics()

  val altsCount = altMap.newAltToOrigAlt.size

  def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M] = {

    val origAlts = delegate.chooseAlternatives(instance)(owningMutableProxy)

    switchFn() match {
      case None => origAlts

      case Some(altIdx) =>
        val activeAltIndex = altIdx % altsCount
        val activeAlt: List[Int] = switchAlts(activeAltIndex)
        val activeOrigAlts: Set[List[Int]] = altMapReduced.newAltToOrigAlt(activeAlt).map(_.fragments).toSet

        origAlts.promote(activeOrigAlts)
    }

  }
}

/**
 * This is the default strategy in composite instances. It assigns rate 0 to all alternatives in the composite model given by
 * the type `M`.
 * @tparam M
 */
case class DefaultCompositeStrategy[M](model: MorphModel[M]) extends MorphingStrategy[M] {

  val alternatives = model.alternatives

  def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]) = alternatives

}

case class BridgeStrategy[MT, MS](srcInstanceRef: MorphKernelRef[MT, MS]) extends MorphingStrategy[MT] {

  outer =>

  val actualStrategy = srcInstanceRef.sourceStrategy match {
    case Some(sourceStr) => sourceStr
    case None => srcInstanceRef.instance.defaultStrategy
  }

  def chooseAlternatives(instance: MorphKernel[MT])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[MT] = {
    val suggestedOrigAlternatives: List[(List[Int], Double)] = actualStrategy.chooseAlternatives(srcInstanceRef.instance)(None).toList
      .map(origAlt => (origAlt._1.map(_.id), origAlt._2)) // maps FragmentNode -> Int

    def findRatingForOrigAlt(origAlt: List[Int]): Double = {
      suggestedOrigAlternatives.find(_._1 == origAlt) match {
        case Some(found) => found._2
        case None => sys.error(s"Cannot find the corresponding alternative to that from alt mapping $origAlt")
      }
    }

    val altMap: AltMappings = srcInstanceRef.altMappings

    val suggestedWinnerAlt = suggestedOrigAlternatives.head._1 // the suggested winner
    // find the target alts referring the source winner - it is used for promoting the preferred target alts
    val targetAltsReferringWinner: Set[List[Int]] = altMap.newAltToOrigAlt.filter(newAltEntry => {
        val referredSourceAlts: List[List[Int]] = newAltEntry._2.map(_.fragments)
        referredSourceAlts.contains(suggestedWinnerAlt)
      }).map(newAltEntry => newAltEntry._1).toSet

    val al: Alternatives[MT] = instance.model.alternatives
    // transform the new alts stored on altMap and associate the rating to them
    al.filter((newAlt, altRating) => {
      // There may be some alternatives missing in the alt mapping if the mapping was built for an existential reference.
      // Filter out such an alternative.
      val newAltIds: List[Int] = newAlt.map(_.id)
      altMap.newAltToOrigAlt.get(newAltIds).isDefined
    }).rate((newAlt, altRating) => {
      val newAltIds: List[Int] = newAlt.map(_.id)
      val origAlt = altMap.newAltToOrigAlt(newAltIds)
      val origAltRatings: List[Double] = origAlt.
        filter(_.fragments.nonEmpty).
        map(oa => findRatingForOrigAlt(oa.fragments))

      val maxRating = if (origAltRatings.nonEmpty)
        origAltRatings.max
      else
        0d

      maxRating

    }).promote(targetAltsReferringWinner)

  }

}

case class BridgeAlternativeComposer[MT, MS](srcInstanceRef: MorphKernelRef[MT, MS]) extends AlternativeComposer[MT] {

  outer =>

  val actualStrategy = srcInstanceRef.sourceStrategy match {
    case Some(sourceStr) => sourceStr
    case None => srcInstanceRef.instance.defaultStrategy
  }

  private def restrictRatedAltsToSpecifiedAlts(matchingAlts: List[List[Int]], ratedOrigAlts: List[(List[FragmentNode], Double)], emptyAltRating: Double): List[(List[FragmentNode], Double)] = {
    if (matchingAlts.size == 1 && matchingAlts.head == Nil) {
      List((Nil, emptyAltRating))
    } else {

      // We must preserve the order of the ratedOrigAlts, which originate from the source model morphing strategy.
      // Thus we have to iterate the ratedAlts list and filter out those which do not have their counterpart in matchingAlts.

      def isMatching(origAlt: List[Int]): Boolean = {
        matchingAlts.contains(origAlt)
      }

      for (ratedOrigAlt <- ratedOrigAlts if isMatching(ratedOrigAlt._1.map(_.id))) yield ratedOrigAlt

    }

//    for (matchAlt <- matchingAlts) yield ratedOrigAlts.find(_._1.map(_.id) == matchAlt) match {
//      case Some(foundRated) => foundRated
//      case None =>
//        // there can be an empty alternative corresponding to an alternatives consisting of placeholders only
//        require(matchAlt == Nil, s"Cannot find matching alt $matchAlt among rated alts $ratedOrigAlts")
//        (Nil, emptyAltRating)
//    }

  }

  override def convertToHolders(newModelInstance: MorphKernel[MT], newAlt: List[FragmentNode], rating: Double, newAltStruct: Option[List[Node]]): List[FragmentHolder[_]] = {
    val altMap: AltMappings = srcInstanceRef.altMappings

    val newAltIds: List[Int] = newAlt.map(_.id)
    val origAltsForNewAlt: List[OrigAlt] = altMap.newAltToOrigAlt.get(newAltIds) match {
      case None => sys.error(s"There is no corresponding original alternative for $newAlt")
      case Some(origAlts) => origAlts
    }

    //val origAltForNewAltWithRating = origAltsForNewAlt.map(origAlt => (origAlt.fragments.map(FragmentNode(_)), 0d))
    val origAltForNewAltFrags = origAltsForNewAlt.map(origAlt => origAlt.fragments)
    // consult the original strategy to choose the best orig alt
    val suggestedOrigAlternatives = actualStrategy.chooseAlternatives(srcInstanceRef.instance)(None).toList

    val chosenAlts = restrictRatedAltsToSpecifiedAlts(origAltForNewAltFrags, suggestedOrigAlternatives, rating)

    // find the best alt from the subset of the orig alts
    MorphingStrategy.fittestAlternative(srcInstanceRef.instance, chosenAlts) match {
      case Some(bestOrigAlt) =>
        // find the template for the chosen alternative
        val bestOrigAltIds = bestOrigAlt._1.map(_.id)
        origAltsForNewAlt.find(_.fragments == bestOrigAltIds) match {
          case Some(origAltForNewAlt) =>

            // merge the new and orig template structures
            val origAltStruct = AltMappingModel(origAltForNewAlt.template, altMap.newFragToOrigFrag,
              (fn) => newModelInstance.fragmentHolder(fn).get,
              (fn) => srcInstanceRef.instance.fragmentHolder(fn).get)

            val mergedAltStruct = newAltStruct match {
              case None => origAltStruct
              case Some(newStruct) => AltMappingModel.transform(newStruct, origAltStruct)
            }

            // Pass the template with the required alt structure to the original default strategy
            srcInstanceRef.instance.altComposer.convertToHolders(srcInstanceRef.instance, bestOrigAlt._1, bestOrigAlt._2, Some(mergedAltStruct))
          case None =>
            sys.error(s"Cannot find template for chosen alternative $bestOrigAltIds")
        }

      case None => sys.error("No alternative chosen")
    }
  }

}


//case class ForkAlternativeComposer[M](ci1: MorphKernelBase[M], ci2: MorphKernelBase[M]) extends AlternativeComposer[M] {
//  override def convertToHolders(instance: MorphKernel[M], alternative: List[FragmentNode], rating: Double, altStruct: Option[List[Node]]): List[FragmentHolder[_]] = {
//
//    def pickWinner(): MorphKernelBase[M] = {
//      // calculate the distance between the chosen alternative and either source corresponding alternative as
//      // the rating difference
//
//      val alt1 = ci1.defaultStrategy.chooseAlternatives(instance).toList
//      val alt2 = ci2.defaultStrategy.chooseAlternatives(instance).toList
//
//      val winRat1 = alt1.find(_._1 == alternative) match {
//        case Some((_, rat1)) => rat1
//        case None => sys.error(s"First fork source does not contain alternative $alternative")
//      }
//
//      val winRat2 = alt2.find(_._1 == alternative) match {
//        case Some((_, rat2)) => rat2
//        case None => sys.error(s"Second fork source does not contain alternative $alternative")
//      }
//
//      if (winRat1 <= winRat2)
//        ci1
//      else
//        ci2
//    }
//
//    val winner: MorphKernelBase[M] = pickWinner()
//    winner.altComposer.convertToHolders(winner, alternative, rating, altStruct)
//
//    //    // the holders contain dummy fragment factories, we need to replace them
//    //    for (holder <- holders) yield winner.fragmentHolder(holder.fragment).get
//
//  }
//}

object IncRating extends ((Boolean, Double) => Double) {
  override def apply(fragmentFound: Boolean, origRating: Double): Double = if (fragmentFound)
    origRating + 1
  else
    origRating
}

object IncRating_! extends ((Boolean, Double) => Double) {
  override def apply(fragmentFound: Boolean, origRating: Double): Double = if (!fragmentFound)
    origRating + 1
  else
    origRating
}

object DecRating extends ((Boolean, Double) => Double) {
  override def apply(fragmentFound: Boolean, origRating: Double): Double = if (fragmentFound)
    origRating - 1
  else
    origRating
}

object DecRating_! extends ((Boolean, Double) => Double) {
  override def apply(fragmentFound: Boolean, origRating: Double): Double = if (!fragmentFound)
    origRating - 1
  else
    origRating
}

case class FindFragment(targetFragTpe: Type, factor: (Boolean, Double) => Double) extends ((List[FragmentHolder[_]], Double) => Double) {

  override def apply(alt: List[FragmentHolder[_]], origRating: Double): Double = {
    val found: Boolean = alt.exists(_.fragment.fragTag.tpe =:= targetFragTpe)
    factor(found, origRating)
  }
}

//case class FragmentRating(targetFragTpe: Type, factor: Double => Double) extends ((List[FragmentHolder[_]], Double) => Double) {
//
//  override def apply(alt: List[FragmentHolder[_]], origRating: Double): Double = {
//    val found: Boolean = alt.exists(_.fragment.fragTag.tpe =:= targetFragTpe)
//    if (found)
//      factor(origRating)
//    else
//      origRating
//  }
//}

trait TraversingMorphingStrategy[M] extends MorphingStrategy[M] {

  // coupling with the stategy on the same model
  def orElse(other: TraversingMorphingStrategy[M]): TraversingMorphingStrategy[M] = {
    new CoupledStrategy[M](this, other)
  }

  // coupling with the stategy on a orthogonal model
  //  def join[M2](other: TraversingMorphingStrategy[M2]): TraversingMorphingStrategy[M with M2] = {
  //    new JoinedStrategy[M, M2](this, other)
  //  }
  //
  def compareNodes[N <: M](instance: MorphKernelBase[N])(x: MorphModelNode, y: MorphModelNode): Int

  def isDefinedFor[N <: M](instance: MorphKernelBase[N])(x: MorphModelNode, y: MorphModelNode): Boolean

  def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M] = {

    val ord = new Ordering[MorphModelNode] {
      override def compare(x: MorphModelNode, y: MorphModelNode): Int = {
        val cmp: Int = compareNodes(instance)(x, y)
        cmp
      }
    }

    def traverse(node: MorphModelNode): List[FragmentNode] = node match {
      case ConjNode(children) =>
        children.flatMap(ch => traverse(ch))
      case DisjNode(children) =>
        traverse(children.max(ord))
      case fn@FragmentNode(_, _) => List(fn)
      case UnitNode => Nil
    }

    // the traversing strategy produces only one alternative in contrast to the lookup strategy
    val chosenFragments: List[FragmentNode] = traverse(instance.rootNode)
    //List((chosenFragments, 1d))

    //    val alternatives = {
    //      val altIter = instance.model.altIterator()
    //      var alts: List[(List[FragmentNode], Double)] = Nil
    //      while (altIter.hasNext) {
    //        val a: List[FragmentNode] = altIter.next()
    //        if (a == chosenFragments)
    //          alts ::= (a, 1d)
    //        else
    //          alts ::= (a, 0d)
    //      }
    //      alts.reverse
    //    }
    //
    //    alternatives

    instance.model.alternatives.rate((alt, _) => {
      if (alt == chosenFragments)
        1d
      else
        0d
    })
  }

}

object MorphingStrategy {

  type GlobalStrategy = (MorphKernelBase[_], List[(List[FragmentNode], Double)]) => List[(List[FragmentNode], Double)]

  object DefaultGlobalStrategy extends GlobalStrategy {
    override def apply(instance: MorphKernelBase[_], altRating: List[(List[FragmentNode], Double)]): List[(List[FragmentNode], Double)] = altRating
  }

  private val globalStrategy = new DynamicVariable[GlobalStrategy](DefaultGlobalStrategy)

  def withGlobalStrategy[S](gs: GlobalStrategy)(thunk: => S): S = {
    val prevGS = globalStrategy.value

    def wrappedGS(instance: MorphKernelBase[_], altRating: List[(List[FragmentNode], Double)]): List[(List[FragmentNode], Double)] =
      gs(instance, prevGS(instance, altRating))

    globalStrategy.withValue(wrappedGS) {
      thunk
    }
  }

  def compositeStrategy[M]: MorphingStrategy[M] = macro MorpherMacros.compositeStrategy[M]

  /**
   * It returns the first alternative with the highest rating.
   * @param alts
   * @tparam M
   * @return
   */
  def fittestAlternative[M](instance: MorphKernelBase[M], alts: List[(List[FragmentNode], Double)]): Option[(List[FragmentNode], Double)] = {

    val finalAlts = globalStrategy.value.apply(instance, alts)

    var fittestOpt: Option[(List[FragmentNode], Double)] = None
    for ((alt, fitness) <- finalAlts) {
      fittestOpt = fittestOpt match {
        case None => Some(alt, fitness)
        case cur@Some((_, curFitness)) =>
          if (curFitness < fitness)
            Some(alt, fitness)
          else
            cur
      }
    }

    fittestOpt
  }

  //implicit def implicitStrategy[M]: MorphingStrategy[M] = macro MorpherMacros.implicitStrategy[M]
}

class CoupledStrategy[M](strat1: TraversingMorphingStrategy[M], strat2: TraversingMorphingStrategy[M]) extends TraversingMorphingStrategy[M] {
  def compareNodes[N <: M](model: MorphKernelBase[N])(x: MorphModelNode, y: MorphModelNode): Int = {
    if (strat1.isDefinedFor(model)(x, y)) {
      strat1.compareNodes(model)(x, y)
    } else if (strat2.isDefinedFor(model)(x, y)) {
      strat2.compareNodes(model)(x, y)
    } else {
      sys.error("Should not be here")
    }
  }

  def isDefinedFor[N <: M](model: MorphKernelBase[N])(x: MorphModelNode, y: MorphModelNode): Boolean = {
    strat1.isDefinedFor(model)(x, y) || strat2.isDefinedFor(model)(x, y)
  }

}

//class JoinedStrategy[M1, M2](strat1: TraversingMorphingStrategy[M1], strat2: TraversingMorphingStrategy[M2]) extends TraversingMorphingStrategy[M1 with M2] {
//
//  override def compareNodes[N <: M1 with M2](model: MorphKernel[N])(x: MorphModelNode, y: MorphModelNode): Int = {
//    if (strat1.isDefinedFor(model)(x, y)) {
//      strat1.compareNodes(model)(x, y)
//    } else if (strat2.isDefinedFor(model)(x, y)) {
//      strat2.compareNodes(model)(x, y)
//    } else {
//      sys.error("Should not be here")
//    }
//  }
//
//  override def isDefinedFor[N <: M1 with M2](model: MorphKernel[N])(x: MorphModelNode, y: MorphModelNode): Boolean = {
//    strat1.isDefinedFor(model)(x, y) || strat2.isDefinedFor(model)(x, y)
//  }
//
//  override def extendBy[M3]: MorphingStrategy[M1 with M2 with M3] = new JoinedStrategy[M1 with M2, M3](this, strat2.extendBy[M3])
//}

//class PartialMorphingStrategy[M](chain: PartialFunction[MorphModel[M], Int]) extends MorphingStrategy[M] {
////class PartialMorphingStrategy[M](chain: PartialFunction[(MorphModel[M], MorphModelNode, MorphModelNode), Int]) extends MorphingStrategy[M] {
//  //override protected def compareNodes[N <: M](model: MorphModel[N])(x: MorphModelNode, y: MorphModelNode): Int = chain.apply((model, x, y))
//  override protected def compareNodes[N <: M](model: MorphModel[N])(x: MorphModelNode, y: MorphModelNode): Int = chain.apply(model)
//
//  override protected def isDefinedFor[N <: M](model: MorphModel[N])(x: MorphModelNode, y: MorphModelNode): Boolean = chain.isDefinedAt((model, x, y))
//}

case class LeftAltsMorphingStrategy[M]() extends TraversingMorphingStrategy[M] {

  override def compareNodes[N <: M](model: MorphKernelBase[N])(x: MorphModelNode, y: MorphModelNode): Int = 1

  override def isDefinedFor[N <: M](model: MorphKernelBase[N])(x: MorphModelNode, y: MorphModelNode): Boolean = true

}

case class RightAltsMorphingStrategy[M]() extends TraversingMorphingStrategy[M] {

  override def compareNodes[N <: M](model: MorphKernelBase[N])(x: MorphModelNode, y: MorphModelNode): Int = -1

  override def isDefinedFor[N <: M](model: MorphKernelBase[N])(x: MorphModelNode, y: MorphModelNode): Boolean = true

}

case class AlternatingMorphingStrategy[M](alts: MorphingStrategy[M]*) extends MorphingStrategy[M] {

  require(alts.nonEmpty, "List of alternatives is empty")

  private var current: MorphingStrategy[M] = alts.head

  def switch(index: Int): Unit = {
    require(index < alts.size, s"Index out of range: $index<${alts.size}")
    current = alts(index)
  }

  def morph_~(comp: MorphKernel[M]) = new {
    val proxy = comp.morph_~(AlternatingMorphingStrategy.this)

    def switch(index: Int): Unit = {
      AlternatingMorphingStrategy.this.switch(index)
      proxy.remorph()
    }
  }

  //  override def compareNodes[N <: M](model: MorphKernel[N])(x: MorphModelNode, y: MorphModelNode): Int =
  //    current.compareNodes(model)(x, y)
  //
  //  override def isDefinedFor[N <: M](model: MorphKernel[N])(x: MorphModelNode, y: MorphModelNode): Boolean =
  //    current.isDefinedFor(model)(x, y)

  def chooseAlternatives(instance: MorphKernel[M])(owningMutableProxy: Option[instance.MutableLUB]): Alternatives[M] = current.chooseAlternatives(instance)(owningMutableProxy)

}

//class NodePotentialStrategy[M, P](potentialFn: (MorphModel[M], MorphModelNode) => P)(implicit ord: Ordering[P]) extends MorphingStrategy[M]  {
//  override def compareNodes[N <: M](model: MorphModel[N])(x: MorphModelNode, y: MorphModelNode): Int = {
//    ord.compare(potentialFn(model, x), potentialFn(model, y))
//  }
//
//  override def isDefinedFor[N <: M](model: MorphModel[N])(x: MorphModelNode, y: MorphModelNode): Boolean = true
//}

object FragmentSelector {

  /**
   * This operator returns the partial function that determines whether the fragment `F` is active or not. The activity
   * status is given by the `activator` function. One usually chains several `?` operators by means of `scala.PartialFunction#orElse`.
   * in order to control the activity status of a group of fragments.
   * {{{
   *         implicit def strategy = FragmentSelector[Model](
   *            ?[Logger] { _ => context.logCalls } orElse
   *            ?[JsonPrettyPrinter] { _ => context.useJsonPrettyPrinter } orElse
   *            ?[ResponseProfiler] { _ => context.profileResponse })
   *
   * }}}
   * @param activator the function returning the activity status of the fragment `F`
   * @tparam F the fragment type for which the activity status is determined
   * @return the fragment's activity status partial function
   */
  def ?[F: WeakTypeTag](activator: Frag[_, _] => Boolean): PartialFunction[Frag[_, _], Boolean] = {
    case f: Frag[_, _] if f.fragTag.tpe <:< weakTypeOf[F] => activator(f)
  }

  //  def apply[M](activator: PartialFunction[Frag[_, _], Boolean]): FragmentSelector[M] = {
  //    new FragmentSelector[M](activator) {
  //      def registerMutator[M2 <: M](mutator: MutableMorphMirror[M2]): Unit = ()
  //    }
  //  }
}

class FragmentSelector[M](activator: PartialFunction[Frag[_, _], Boolean]) extends TraversingMorphingStrategy[M] {

  private def isDef[N](model: MorphKernelBase[N], n: MorphModelNode) = n match {
    case fn@FragmentNode(_, _) =>
      val fh = model.fragmentHolder(fn).get
      activator.isDefinedAt(fh.fragment)
    case _ => false
  }

  def compareNodes[N <: M](model: MorphKernelBase[N])(x: MorphModelNode, y: MorphModelNode): Int = {

    def cmp(n: MorphModelNode) = n match {
      case fn@FragmentNode(_, _) =>
        val fh = model.fragmentHolder(fn).get
        if (activator(fh.fragment)) 1 else -1
      case _ => 0
    }

    if (isDef(model, x))
      cmp(x)
    else if (isDef(model, y))
      -cmp(y)
    else
      sys.error("Should not be here")
  }

  def isDefinedFor[N <: M](model: MorphKernelBase[N])(x: MorphModelNode, y: MorphModelNode): Boolean = {
    isDef(model, x) || isDef(model, y)
  }

}

