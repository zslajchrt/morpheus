package org.morpheus

import scala.reflect.runtime.universe._

/**
 * Created by zslajchrt on 29/04/15.
 */
trait CompositeMirror[M, L] {

  type ConfLev <: ConformanceLevelMarker
  type Model = M
  type LUB = L

  def toCompositeInstance: CompositeInstance[M] with ConfLev

  def myAlternative: List[FragmentHolder[_]]

  /**
   * @return The alternatives from which the winning myAlternative was chosen. It is suitable for creating FixedStrategy
   *         from it. Without this set of the alternatives we would have to evaluate the strategy again.
   */
  def alternatives: Alternatives[M]

  /**
   * @return the strategy used to rank the alternatives
   */
  def strategy: MorpherStrategy[M]

  def findFragmentHolder[F: WeakTypeTag]: Option[FragmentHolder[_]] = {
    val fragTpe = implicitly[WeakTypeTag[F]].tpe
    myAlternative.find(_.fragment.fragTag.tpe <:< fragTpe)
  }

  def owningMutableProxy: Option[LUB with MutableCompositeMirror[M, LUB]]
}


trait Mutator[M] {

  /**
   * It causes a re-instantiation only if the new composition of the proxy's delegate differs from the current one.
   */
  def remorph(): Unit

  def remorph(altStrategy: MorpherStrategy[M])
}

trait MutableCompositeMirror[M, LUB] extends CompositeMirror[M, LUB] with Mutator[M] {

  private lazy val changeListener = new MutableFragmentListener {

    @volatile var eventSelector = PartialFunction.empty[CompositeEvent[Any], Boolean]

    override def onEvent(eventName: String, eventValue: Any, eventSource: Any) = {
      val ce = CompositeEvent(eventName, eventValue, eventSource)
      if (eventSelector.isDefinedAt(ce) && eventSelector(ce)) {
        remorph()
      }
      Nil
    }
  }

  private def registerChangeListener(): Boolean = {
    delegate match {
      case mf: MutableFragment =>
        mf.addListener(changeListener)
        true
      case _ => false
    }
  }

  private def unregisterChangeListener(): Boolean = {
    delegate match {
      case mf: MutableFragment =>
        mf.removeListener(changeListener)
        true
      case _ => false
    }
  }

  def delegate: LUB

  def startListening(eventSelector: PartialFunction[CompositeEvent[Any], Boolean] = { case _ => true }): Boolean = {
    delegate match {
      case mf: MutableFragment =>
        changeListener.eventSelector = changeListener.eventSelector.orElse(eventSelector)
        registerChangeListener()
        true
      case _ => false
    }
  }

  def stopListening(): Boolean = {
    delegate match {
      case mf: MutableFragment =>
        changeListener.eventSelector = PartialFunction.empty[CompositeEvent[Any], Boolean]
        unregisterChangeListener()
      case _ => false
    }
  }
}

