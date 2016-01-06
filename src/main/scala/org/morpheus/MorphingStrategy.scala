package org.morpheus

import org.morpheus.AltMappingModel.{FragmentInsertion, Node}
import org.morpheus.FragmentValidator.ValidationResult

import scala.annotation.tailrec
import scala.reflect.runtime.universe._
import scala.util.DynamicVariable
import scala.language.experimental.macros

/**
 *
 * Created by zslajchrt on 29/04/15.
 */
trait MorphingStrategy[M] {

  type Model = M

  def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M]

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

  override def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = alternatives

}

case class LastRatingStrategy[M](mirrorOpt: Option[MorphMirror[M]]) extends MorphingStrategy[M] {

  def this() = this(None)

  def this(mirror: MorphMirror[M]) = this(Some(mirror))

  override def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = mirrorOpt match {
    case Some(mirror) => mirror.alternatives
    case None => morphProxy match {
      case None => instance.model.alternatives
      case Some(proxy) =>
//        if (proxy.delegate == null)
//          instance.model.alternatives
//        else
          proxy.alternatives
    }
  }

}

case class Normalizer[M](delegate: MorphingStrategy[M], minRating: Double = 0, maxRating: Double = 1) extends MorphingStrategy[M] {

  override def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {

    var minFound: Option[Double] = None
    var maxFound: Option[Double] = None

    val origAlts: Alternatives[M] = delegate.chooseAlternatives(instance)(morphProxy)
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
  override def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = instance.model.alternatives
}

object RootStrategy {
  def apply[M](model: MorphModel[M]) = new RootStrategy[M]()
}

case class RatingStrategy[M](delegate: MorphingStrategy[M], ratingOperators: ((List[FragmentHolder[_]], Double) => Double)*) extends MorphingStrategy[M] {

  def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {

    def calculateRating(alt: List[FragmentHolder[_]], origRating: Double): Double = {
      ratingOperators.foldRight(origRating)((ratOper, res) => ratOper(alt, res))
    }

    val origRatedAlts = delegate.chooseAlternatives(instance)(morphProxy)
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

  def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {
    val origAlts = delegate.chooseAlternatives(instance)(morphProxy)

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


//case class PromotingStrategy[M, S](delegate: MorphingStrategy[M], switchModel: MorphModel[S], altMap: AltMappings, switchFn: () => Option[Int]) extends MorphingStrategy[M] {
//
//  val switchAlts: List[List[Int]] = switchModel.altIterator().toList.map(_.map(_.id))
//  val altMapReduced = altMap.preserveDynamics()
//
//  val altsCount = altMap.newAltToOrigAlt.size
//
//  def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {
//
//    val origAlts = delegate.chooseAlternatives(instance)(morphProxy)
//
//    switchFn() match {
//      case None => origAlts
//
//      case Some(altIdx) =>
//        val activeAltIndex = altIdx % altsCount
//        val activeAlt: List[Int] = switchAlts(activeAltIndex)
//        val activeOrigAlts: Set[List[Int]] = altMapReduced.newAltToOrigAlt(activeAlt).map(_.fragments).toSet
//
//        origAlts.promote(activeOrigAlts)
//    }
//
//  }
//}

abstract class PromotingStrategyCommon[M, S](delegate: MorphingStrategy[M], switchModel: MorphModel[S], altMap: AltMappings) extends MorphingStrategy[M] {

  val switchAlts: List[List[Int]] = switchModel.altIterator().toList.map(_.map(_.id))
  val altMapReduced = altMap.preserveDynamics()

  val altsCount = altMap.newAltToOrigAlt.size

  override def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {

    val origAlts = delegate.chooseAlternatives(instance)(morphProxy)

    switch(instance)(morphProxy) match {
      case None => origAlts

      case Some(altIdx) =>
        val activeAltIndex = altIdx % altsCount
        val activeAlt: List[Int] = switchAlts(activeAltIndex)
        val activeOrigAlts: Set[List[Int]] = altMapReduced.newAltToOrigAlt(activeAlt).map(_.fragments).toSet

        origAlts.promote(activeOrigAlts)
    }

  }

  protected def switch(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Option[Int]

}

case class PromotingStrategy[M, S](delegate: MorphingStrategy[M], switchModel: MorphModel[S], altMap: AltMappings, switchFn: () => Option[Int])
  extends PromotingStrategyCommon[M, S](delegate, switchModel, altMap) {

  override protected def switch(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Option[Int] = switchFn()
}

class PromotingStrategyWithModel[M, S, ImmutableLUB](val morphModel: MorphModel[M]) {

  class Strat(delegate: MorphingStrategy[morphModel.Model], switchModel: MorphModel[S], altMap: AltMappings, switchFn: (Option[ImmutableLUB]) => Option[Int])
    extends PromotingStrategyCommon[M, S](delegate, switchModel, altMap) {

    override protected def switch(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Option[Int] = {
      switchFn(morphProxy.asInstanceOf[Option[ImmutableLUB]])
    }

  }

}

//class PromotingStrategyWithModel[M, S, MutableLUB](val morphModel: MorphModel[M]) {
//
//  class Strat(delegate: MorphingStrategy[morphModel.Model], switchModel: MorphModel[S], altMap: AltMappings, switchFn: (Option[MutableLUB]) => Option[Int]) extends MorphingStrategy[morphModel.Model] {
//
//    val switchAlts: List[List[Int]] = switchModel.altIterator().toList.map(_.map(_.id))
//    val altMapReduced = altMap.preserveDynamics()
//
//    val altsCount = altMap.newAltToOrigAlt.size
//
//    override def chooseAlternatives(instance: MorphKernel[morphModel.Model])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[morphModel.Model] = {
//
//      val origAlts = delegate.chooseAlternatives(instance)(morphProxy)
//
//      switchFn(morphProxy.asInstanceOf[Option[MutableLUB]]) match {
//        case None => origAlts
//
//        case Some(altIdx) =>
//          val activeAltIndex = altIdx % altsCount
//          val activeAlt: List[Int] = switchAlts(activeAltIndex)
//          val activeOrigAlts: Set[List[Int]] = altMapReduced.newAltToOrigAlt(activeAlt).map(_.fragments).toSet
//
//          origAlts.promote(activeOrigAlts)
//      }
//
//    }
//  }
//
//}


case class MaskExplicitStrategy[M](delegate: MorphingStrategy[M], negative: Boolean, fragments: () => Option[Set[Int]], isStrict: Boolean = false) extends MorphingStrategy[M] {
  override def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {
    val origAlts = delegate.chooseAlternatives(instance)(morphProxy)

    fragments() match {
      case None => origAlts
      case Some(ff) =>
        val newAlts = if (negative)
          origAlts.unmask(ff)
        else
          origAlts.mask(ff)
        if (isStrict) newAlts.strict() else newAlts.nonStrict()
    }

  }
}

object DisableInvalidFragmentsStrategy {

  trait InvalidFragmentsUpdater {
    def updateInvalidFragments(invalidFragments: Set[Int]): Unit

    def updateInvalidFragments(validationResults: Iterable[ValidationResult[_]]): Unit
  }

  def apply[M](delegate: MorphingStrategy[M]) = {
    var invalFrags: Option[Set[Int]] = None
    new MaskExplicitStrategy[M](delegate, negative = true, () => invalFrags) with InvalidFragmentsUpdater {

      override def updateInvalidFragments(invalidFragments: Set[Int]): Unit = {
        invalFrags = Some(invalidFragments)
      }

      override def updateInvalidFragments(validationResults: Iterable[ValidationResult[_]]): Unit = {
        val failedFragments = ValidationResult.extractInvalidFragments(validationResults)
        updateInvalidFragments(failedFragments)
      }
    }
  }
}

object EnableValidFragmentsOnlyStrategy {

  trait ValidFragmentsUpdater {
    def updateValidFragments(validationResults: Iterable[ValidationResult[_]]): Unit
  }

  def apply[M](delegate: MorphingStrategy[M]) = {
    // (allValidatedFragment, validFragmentsOnly)
    var fragmentMasks: Option[(Set[Int], Set[Int])] = None
    new MorphingStrategy[M] with ValidFragmentsUpdater {

      override def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {
        val origAlts = delegate.chooseAlternatives(instance)(morphProxy)

        fragmentMasks match {
          case None => origAlts
          case Some(fragMasks) =>
            origAlts.unmask(fragMasks._1).mask(fragMasks._2)
        }
      }

      override def updateValidFragments(validationResults: Iterable[ValidationResult[_]]): Unit = {
        val allValidatedFragments = ValidationResult.extractAllFragments(validationResults)
        val validFragments = ValidationResult.extractValidFragments(validationResults)
        fragmentMasks = Some((allValidatedFragments, validFragments))
      }

    }
  }
}

case class MaskAllStrategy[M](delegate: MorphingStrategy[M], negative: Boolean) extends MorphingStrategy[M] {

  override def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {
    val origAlts = delegate.chooseAlternatives(instance)(morphProxy)
    if (negative)
      origAlts.unmaskAll()
    else
      origAlts.maskAll()
  }

}

case class StrictStrategy[M](delegate: MorphingStrategy[M]) extends MorphingStrategy[M] {

  override def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {
    val origAlts = delegate.chooseAlternatives(instance)(morphProxy)
    origAlts.strict()
  }

}

abstract class MaskingStrategyCommon[M, S](delegate: MorphingStrategy[M], switchModel: MorphModel[S], altMap: AltMappings, negative: Boolean, cumulative: Boolean) extends MorphingStrategy[M] {

  val switchAlts: List[List[Int]] = switchModel.altIterator().toList.map(_.map(_.id))
  val altMapReduced = altMap.preserveDynamics()

  val altsCount = altMap.newAltToOrigAlt.size

  def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {

    val origAlts = delegate.chooseAlternatives(instance)(morphProxy)

    switch(instance)(morphProxy) match {
      case None => origAlts

      case Some(altIdx) =>
        val activeAltIndex = altIdx % altsCount

        if (cumulative) {
          val swAlt: List[Int] = switchAlts(activeAltIndex)
          val maskedFragments: List[Int] = swAlt.map(altMapReduced.newFragToOrigFrag)
          if (negative) {
            origAlts.unmask(maskedFragments.toSet)
          } else {
            origAlts.mask(maskedFragments.toSet)
          }

        } else {
          var updatedAlts = origAlts
          for (altId <- 0 until altsCount) {
            val swAlt: List[Int] = switchAlts(altId)
            val selectedFragments: List[Int] = swAlt.map(altMapReduced.newFragToOrigFrag)

            updatedAlts = if (altId == activeAltIndex) {
              if (negative)
                updatedAlts.unmask(selectedFragments.toSet)
              else
                updatedAlts.mask(selectedFragments.toSet)
            } else {
              if (negative)
                updatedAlts.mask(selectedFragments.toSet)
              else
                updatedAlts.unmask(selectedFragments.toSet)
            }
          }
          updatedAlts
        }
    }

  }

  protected def switch(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Option[Int]

}

case class MaskingStrategy[M, S](delegate: MorphingStrategy[M], switchModel: MorphModel[S], altMap: AltMappings, switchFn: () => Option[Int], negative: Boolean, cumulative: Boolean)
  extends MaskingStrategyCommon[M, S](delegate, switchModel, altMap, negative, cumulative) {

  override protected def switch(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Option[Int] = switchFn()

}

class MaskingStrategyWithModel[M, S, ImmutableLUB](val morphModel: MorphModel[M]) {

  class Strat(delegate: MorphingStrategy[morphModel.Model], switchModel: MorphModel[S], altMap: AltMappings, switchFn: (Option[ImmutableLUB]) => Option[Int], negative: Boolean, cumulative: Boolean)
    extends MaskingStrategyCommon[M, S](delegate, switchModel, altMap, negative, cumulative) {

    override protected def switch(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Option[Int] = {
      switchFn(morphProxy.asInstanceOf[Option[ImmutableLUB]])
    }

  }

}

abstract class FragmentMaskingStrategyCommon[M, F](delegate: MorphingStrategy[M], fragmentDesc: Frag[F, _], negative: Boolean) extends MorphingStrategy[M] {

  val fragMask: Set[Int] = Set(fragmentDesc.index)

  def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {

    val origAlts = delegate.chooseAlternatives(instance)(morphProxy)

    if (switch(instance)(morphProxy)) {
      if (negative) {
        origAlts.unmask(fragMask)
      } else {
        origAlts.mask(fragMask)
      }
    } else {
      origAlts
    }

  }

  protected def switch(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Boolean

}

case class FragmentMaskingStrategy[M, F](delegate: MorphingStrategy[M], fragmentDesc: Frag[F, _], switchFn: () => Boolean, negative: Boolean)
  extends FragmentMaskingStrategyCommon[M, F](delegate, fragmentDesc, negative) {

  override protected def switch(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Boolean = switchFn()
}

class FragmentMaskingStrategyWithModel[M, F, ImmutableLUB](val morphModel: MorphModel[M]) {

  class Strat(delegate: MorphingStrategy[morphModel.Model], fragmentDesc: Frag[F, _], switchFn: (Option[ImmutableLUB]) => Boolean, negative: Boolean)
    extends FragmentMaskingStrategyCommon[M, F](delegate, fragmentDesc, negative) {

    override protected def switch(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Boolean = {
      switchFn(morphProxy.asInstanceOf[Option[ImmutableLUB]])
    }

  }

}

abstract class FragmentRatingStrategyCommon[M, F](delegate: MorphingStrategy[M], fragmentDesc: Frag[F, _]) extends MorphingStrategy[M] {

  def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {

    val origAlts = delegate.chooseAlternatives(instance)(morphProxy)

    origAlts.rate((alt, curRat) => if (alt.map(_.id).contains(fragmentDesc.index)) {
      curRat + rateAlt(instance)(morphProxy)
    } else {
      curRat
    })
  }

  protected def rateAlt(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Double

}

case class FragmentRatingStrategy[M, F](delegate: MorphingStrategy[M], fragmentDesc: Frag[F, _], rateFn: () => Double)
  extends FragmentRatingStrategyCommon[M, F](delegate, fragmentDesc) {

  override protected def rateAlt(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Double = rateFn()
}

class FragmentRatingStrategyWithModel[M, F, ImmutableLUB](val morphModel: MorphModel[M]) {

  class Strat(delegate: MorphingStrategy[morphModel.Model], fragmentDesc: Frag[F, _], rateFn: (Option[ImmutableLUB]) => Double)
    extends FragmentRatingStrategyCommon[M, F](delegate, fragmentDesc) {

    override protected def rateAlt(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Double = {
      rateFn(morphProxy.asInstanceOf[Option[ImmutableLUB]])
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

  def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]) = alternatives

}

case class BridgeStrategy[MT, MS](srcInstanceRef: MorphKernelRef[MT, MS]) extends MorphingStrategy[MT] {

  outer =>

  val actualStrategy = srcInstanceRef.sourceStrategy match {
    case Some(sourceStr) => sourceStr
    case None => srcInstanceRef.instance.defaultStrategy
  }

  def chooseAlternatives(instance: MorphKernel[MT])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[MT] = {
    val sourceAlts: Alternatives[MS] = actualStrategy.chooseAlternatives(srcInstanceRef.instance)(Some(srcInstanceRef.instance.!))

    val suggestedWinnerAlt: List[Int] = MorphingStrategy.fittestAlternative(srcInstanceRef.instance, sourceAlts.toMaskedList) match {
      case None => throw new NoAlternativeChosenException("Source model yields no viable alternative")
      case Some(winnerAlt) => winnerAlt._1.map(_.id)
    }

    val altMap: AltMappings = srcInstanceRef.altMappings

    val al: Alternatives[MT] = instance.model.alternatives

    // find the first target alt referring to the source winner - it is used for promoting the preferred target alt
    val targetAltsReferringWinner: Set[List[Int]] = al.toMaskedList.find(tgtAlt => {
      val tgtAltIds: List[Int] = tgtAlt._1.map(_.id)
      val referredSourceAlts: List[List[Int]] = altMap.newAltToOrigAlt.getOrElse(tgtAltIds, List.empty).map(_.fragments)
      referredSourceAlts.contains(suggestedWinnerAlt)
    }).map(_._1.map(_.id)).toSet

    //    // find the target alts referring the source winner - it is used for promoting the preferred target alts
//    val targetAltsReferringWinner: Set[List[Int]] = altMap.newAltToOrigAlt.filter(newAltEntry => {
//        val referredSourceAlts: List[List[Int]] = newAltEntry._2.map(_.fragments)
//        referredSourceAlts.contains(suggestedWinnerAlt)
//      }).map(newAltEntry => newAltEntry._1).toSet

    val targetFragmentMask: Set[Int] = altMap.newFragToOrigFrag.filter(f2f => {
      sourceAlts.fragmentMask.contains(f2f._2)
    }).map(_._1).toSet

    // maps FragmentNode -> Int
    val suggestedOrigAlternatives: List[(List[Int], Double)] = sourceAlts.toMaskedList
      .map(origAlt => (origAlt._1.map(_.id), origAlt._2))

    def findRatingForOrigAlt(origAlt: List[Int]): Double = {
      suggestedOrigAlternatives.find(_._1 == origAlt) match {
        case Some(found) => found._2
        case None => 0d
      }
    }

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

    }).promote(targetAltsReferringWinner).mask(targetFragmentMask)

  }

}


abstract class NoViableAlternativeException(msg: String) extends Exception(msg) {

}

class AlternativeNotAvailableException(val alt: List[FragmentNode], msg: String) extends NoViableAlternativeException(msg) {

}

class NoAlternativeChosenException(msg: String) extends NoViableAlternativeException(msg) {

}

case class BridgeAlternativeComposer[MT, MS](srcInstanceRef: MorphKernelRef[MT, MS]) extends AlternativeComposer[MT] {

  outer =>

  val actualStrategy = srcInstanceRef.sourceStrategy match {
    case Some(sourceStr) => sourceStr
    case None => srcInstanceRef.instance.defaultStrategy
  }

  private def restrictRatedAltsToSpecifiedAlts(matchingAlts: List[List[Int]], ratedOrigAlts: List[(List[FragmentNode], Double)], defaultAltRating: Double): List[(List[FragmentNode], Double)] = {
    if (matchingAlts.size == 1 && matchingAlts.head == Nil) {
      List((Nil, defaultAltRating))
    } else {

      // We must preserve the order of the ratedOrigAlts, which originate from the source model morphing strategy.
      // Thus we have to iterate the ratedAlts list and filter out those which do not have their counterpart in matchingAlts.

      def isMatching(origAlt: List[Int]): Boolean = {
        matchingAlts.contains(origAlt)
      }

      val restricted = for (ratedOrigAlt <- ratedOrigAlts if isMatching(ratedOrigAlt._1.map(_.id))) yield ratedOrigAlt

      if (restricted.isEmpty) {
        val emptyAltAllowed = srcInstanceRef.altMappings.newAltToOrigAlt.keySet.contains(Nil)
        if (emptyAltAllowed) {
          List((Nil, defaultAltRating))
        } else {
          Nil
        }
      } else {
        restricted
      }

    }

  }

  override def convertToHolders(newModelInstance: MorphKernel[MT], newAlt: List[FragmentNode], rating: Double, newAltStruct: Option[List[Node]]): List[FragmentHolder[_]] = {

    if (newAlt == Nil && !newAltStruct.isDefined) {
      return Nil
    }

    val altMap: AltMappings = srcInstanceRef.altMappings

    val newAltIds: List[Int] = newAlt.map(_.id)
    val origAltsForNewAlt: List[OrigAlt] = altMap.newAltToOrigAlt.get(newAltIds) match {
      case None =>
        if (newAlt.isEmpty && newAltStruct.isDefined) {
          require(newAltStruct.get.forall({
            case FragmentInsertion(_) => true
            case _ => false
          }), "Empty new alternative structure must be accompanied by its structure composed of fragment insertions only")

          return newAltStruct.get.map(_.holder())
        } else {
          sys.error(s"There is no corresponding original alternative for $newAlt")
        }
      case Some(origAlts) => origAlts
    }

    //val origAltForNewAltWithRating = origAltsForNewAlt.map(origAlt => (origAlt.fragments.map(FragmentNode(_)), 0d))
    val origAltForNewAltFrags = origAltsForNewAlt.map(origAlt => origAlt.fragments)
    // consult the original strategy to choose the best orig alt

    val suggestedOrigAlternatives = actualStrategy.chooseAlternatives(srcInstanceRef.instance)(Some(srcInstanceRef.instance.!))
    // First, try to get the candidate original alts from the masked list
    val chosenAltsFirstAttempt = restrictRatedAltsToSpecifiedAlts(origAltForNewAltFrags, suggestedOrigAlternatives.toMaskedList, rating)
    val chosenAlts = if (chosenAltsFirstAttempt == Nil) {
      // If the masked list does not contain any target alt or the target alts do not contain the empty one
      // then we have to throw an exception
      throw new AlternativeNotAvailableException(newAlt, s"No source alternative found for target alternative $newAlt")
    } else {
      chosenAltsFirstAttempt
    }

    @tailrec
    def makeFragHolders(candidates: List[(List[FragmentNode], Double)]): List[FragmentHolder[_]] = {
      try {
        // find the best alt from the subset of the orig alts
        MorphingStrategy.fittestAlternative(srcInstanceRef.instance, candidates) match {
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
                if (bestOrigAltIds == Nil) {
                  // The chosen alt is the empty alt, but it is not among the source alts. It's OK.
                  Nil
                } else {
                  // This one should not occur
                  sys.error(s"Cannot find template for chosen alternative $bestOrigAltIds")
                }
            }

          case None => sys.error("No alternative chosen")
        }

      } catch {
        case ae: AlternativeNotAvailableException =>
          val newCandidates = candidates.filterNot(_._1 == ae.alt)
          if (newCandidates.isEmpty) {
            throw new AlternativeNotAvailableException(newAlt, s"No source alternative found for target alternative $newAlt")
          } else {
            // try it again without the failed candidate alt
            makeFragHolders(newCandidates)
          }
      }
    }


    makeFragHolders(chosenAlts)

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

  def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = {

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

  def chooseAlternatives(instance: MorphKernel[M])(morphProxy: Option[instance.ImmutableLUB]): Alternatives[M] = current.chooseAlternatives(instance)(morphProxy)

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

