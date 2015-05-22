package org.morpheus

import scala.collection.mutable

/**
 * Created by zslajchrt on 29/04/15.
 */
case class EventMonitor[X](eventName: String, replies: CompositeEvent[_]*) extends MutableFragmentListener with (() => Option[X]) {

  @volatile private [this] var _status: Option[X] = None

  private val _lst = mutable.WeakHashMap.empty[MutableFragmentListener, Unit]

  def makeEvent(eventValue: X) = CompositeEvent[X](eventName, eventValue, null)

  def addListener(listener: MutableFragmentListener): this.type = {
    _lst += (listener -> ())
    this
  }

  override def onEvent(firedEventName: String, eventValue: Any, eventSource: Any) = {

    def notifyListener(l: MutableFragmentListener): List[CompositeEvent[_]] = {
      try {
        l.onEvent(eventName, eventValue, eventSource)
      }
      catch {
        case t: Throwable =>
        // todo: some logging?
          Nil
      }
    }

    if (firedEventName == eventName) {
      _status = Some(eventValue.asInstanceOf[X])

      _lst.toList.foldLeft(replies.toList)((res, l) => {
        res ::: notifyListener(l._1)
      })
    } else {
      Nil
    }

  }

  def status(): Option[X] = _status
  def apply() = status()
  def apply(expectedStatus: X, defaultResult: Boolean = false) = status() match {
    case None => defaultResult
    case Some(stat) => stat == expectedStatus
  }

}


