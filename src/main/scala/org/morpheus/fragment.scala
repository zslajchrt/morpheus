package org.morpheus

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

/**
 * @param confLevel the conformance level of the fragment. See [[org.morpheus.Morpheus.ConformanceLevel]]
 */
class fragment(val confLevel: String = "") extends scala.annotation.StaticAnnotation
