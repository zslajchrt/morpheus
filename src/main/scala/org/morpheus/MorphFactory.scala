package org.morpheus

/**
 * Created by zslajchrt on 29/01/15.
 *
 */
object MorphFactory {

  def newComposite[M, LUB, ConformLev <: ConformanceLevelMarker](contextClasses: Array[(List[Class[_]], _)], compositeInstance: MorphKernel[M],
                           alternative: List[FragmentHolder[_]], alternatives: Alternatives[M],
                           strategy: MorphingStrategy[M],
                           owningMutableProxy: Option[LUB with MutableMorpherMirror[M, LUB]]): AnyRef = {
    var compositeContext: CompleteMorphContext[M, LUB, ConformLev] =  new CompleteMorphContext[M, LUB, ConformLev](
      contextClasses.map(_._1), contextClasses.map(_._2), compositeInstance.asInstanceOf[MorphKernel[M] with ConformLev], alternative, alternatives, strategy, owningMutableProxy)

    compositeContext.proxy
  }

}
