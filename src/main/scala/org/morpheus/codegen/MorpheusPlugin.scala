package org.morpheus.codegen

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

/**
 * Created by zslajchrt on 19/01/15.
 */
class MorpheusPlugin(val global: Global) extends Plugin {

  val name = "morpheus"
  val description = "Morpheus code generator"

  val components = List[PluginComponent](
    new MorpheusPreTyper(global)
    , new SuperProxyGenerator(global)
//    , new MorpheusPostTyper(global)
  )

}