package org.morpheus

/**
 * Created by zslajchrt on 29/01/15.
 *
 */
object CompositeFactory {

  def newComposite[M, LUB, ConformLev <: ConformanceLevelMarker](contextClasses: Array[(List[Class[_]], _)], compositeInstance: CompositeInstance[M],
                           alternative: List[FragmentHolder[_]], alternatives: Alternatives[M],
                           strategy: MorpherStrategy[M],
                           owningMutableProxy: Option[LUB with MutableCompositeMirror[M, LUB]]): AnyRef = {
    var compositeContext: CompleteCompositeContext[M, LUB, ConformLev] =  new CompleteCompositeContext[M, LUB, ConformLev](
      contextClasses.map(_._1), contextClasses.map(_._2), compositeInstance.asInstanceOf[CompositeInstance[M] with ConformLev], alternative, alternatives, strategy, owningMutableProxy)

    compositeContext.proxy
  }

}
