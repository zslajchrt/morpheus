package org.morpheus

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Created by zslajchrt on 29/04/15.
 */
trait MutableFragmentListener {
  def onEvent(eventName: String, eventValue: Any, eventSource: Any): List[CompositeEvent[_]]
}

trait MutableFragmentConfig {
  val listeners: List[MutableFragmentListener]
}

object MutableFragmentConfig {

  def apply(lst: MutableFragmentListener*) = new MutableFragmentConfig {
    override val listeners: List[MutableFragmentListener] = {
      lst.toList
    }
  }
}

@fragment
trait MutableFragment extends MutableFragmentConfig {
  private val _lst = mutable.WeakHashMap.empty[MutableFragmentListener, Unit]

  def addListener(listener: MutableFragmentListener): this.type = {
    _lst += (listener -> ())
    this
  }

  def removeListener(listener: MutableFragmentListener): this.type = {
    _lst -= listener
    this
  }

  @tailrec
  final def fireEvents(events: List[CompositeEvent[_]]): Unit = {

    def fireOneEvent(event: CompositeEvent[_]): List[CompositeEvent[_]] = {

      def notifyListener(l: MutableFragmentListener): List[CompositeEvent[_]] = {
        try {
          l.onEvent(event.eventName, event.eventValue, event.eventSource)
        }
        catch {
          case t: Throwable =>
          // todo: some logging?
            Nil
        }
      }

      val newEvents = listeners.foldLeft[List[CompositeEvent[_]]](Nil)((res, l) => {
        res ::: notifyListener(l)
      })
      _lst.toList.foldLeft(newEvents)((res, l) => {
        res ::: notifyListener(l._1)
      })
    }

    val secondaryEvents = events.foldLeft[List[CompositeEvent[_]]](Nil)((res, event) => res ::: fireOneEvent(event))
    if (secondaryEvents.nonEmpty)
      fireEvents(secondaryEvents)
  }

  def fireEvent(event: CompositeEvent[_]): Unit = {
    fireEvents(List(event))
  }

  def fire(eventName: String, eventValue: Any, eventSource: Any): Unit = {
    fireEvent(CompositeEvent(eventName, eventValue, eventSource))
  }

}

class MutableFragment$fragment(config: MutableFragmentConfig) extends MutableFragment {
  override val listeners: List[MutableFragmentListener] = config.listeners
}

case class CompositeEvent[T](eventName: String, eventValue: T, eventSource: Any) {
  def nameSelector: PartialFunction[CompositeEvent[Any], Boolean] = {
    case CompositeEvent(en, _, _) if en == eventName => true
  }

  def nameValueSelector: PartialFunction[CompositeEvent[Any], Boolean] = {
    case CompositeEvent(en, ev, _) if en == eventName && ev == eventValue => true
  }

  def fullSelector: PartialFunction[CompositeEvent[Any], Boolean] = {
    case ce@CompositeEvent(_, _, _) if ce == this => true
  }
}
