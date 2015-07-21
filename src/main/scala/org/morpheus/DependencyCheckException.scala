package org.morpheus

/**
 * Created by zslajchrt on 14/04/15.
 */
class DependencyCheckException(msg: String, cause: Exception) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
}
