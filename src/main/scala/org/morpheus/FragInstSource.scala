package org.morpheus

/**
 * Created by zslajchrt on 29/04/15.
 */
sealed trait FragInstSource {
  val fragment: FragmentNode
}

case class PlaceholderSource(fragment: FragmentNode) extends FragInstSource

case class OriginalInstanceSource(fragment: FragmentNode) extends FragInstSource

case class OrigAlt(fragments: List[Int], template: List[FragInstSource])
