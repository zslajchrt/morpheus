package org.morpheus

import scala.runtime.BoxedUnit

/**
 * Created by zslajchrt on 29/01/15.
 *
 */
object MorphFactory {

  def newComposite[M, L, ConformLev <: ConformanceLevelMarker](contextClasses: Array[(List[Class[_]], _)], compositeInstance: MorphKernel[M] { type LUB = L; type ConformLevel = ConformLev },
                           alternative: List[FragmentHolder[_]], alternatives: Alternatives[M],
                           strategy: MorphingStrategy[M])(owningMutableProxy: Option[compositeInstance.MutableLUB]): AnyRef = {

    val actualClsAndFrags = if (contextClasses.isEmpty) {
      Array((List(classOf[AnyRef]), BoxedUnit.UNIT))
    } else {
      contextClasses
    }

    val compositeContext: CompleteMorphContext[M, L, ConformLev] =  new CompleteMorphContext[M, L, ConformLev](
      actualClsAndFrags.map(_._1), actualClsAndFrags.map(_._2), compositeInstance, alternative, alternatives, strategy) {
      override val owningProxy = owningMutableProxy
    }

    compositeContext.proxy
  }

}
