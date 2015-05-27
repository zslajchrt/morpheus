package org.morpheus

/**
 * Created by zslajchrt on 29/01/15.
 *
 */
object MorphFactory {

  def newComposite[M, L, ConformLev <: ConformanceLevelMarker](contextClasses: Array[(List[Class[_]], _)], compositeInstance: MorphKernel[M] { type LUB = L; type ConformLevel = ConformLev },
                           alternative: List[FragmentHolder[_]], alternatives: Alternatives[M],
                           strategy: MorphingStrategy[M])(owningMutableProxy: Option[compositeInstance.MutableLUB]): AnyRef = {
    val compositeContext: CompleteMorphContext[M, L, ConformLev] =  new CompleteMorphContext[M, L, ConformLev](
      contextClasses.map(_._1), contextClasses.map(_._2), compositeInstance, alternative, alternatives, strategy) {
      override val owningProxy = owningMutableProxy
    }

    compositeContext.proxy
  }

}
