package org.morpheus

/**
 * Created by zslajchrt on 29/04/15.
 */
sealed trait FragInstSource {
  val fragment: FragmentNode
}

case class PlaceholderSource(fragment: FragmentNode) extends FragInstSource {
  override def toString: String = s"p(${fragment.id})"
}

case class OriginalInstanceSource(fragment: FragmentNode) extends FragInstSource {
  override def toString: String = s"o(${fragment.id})"
}

case class OrigAlt(fragments: List[Int], template: List[FragInstSource]) {
  override def toString: String = s"a($fragments,$template)"
}
