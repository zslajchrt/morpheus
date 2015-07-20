package org.morpheus

import org.morpheus.Morpheus.ConformanceLevel

/**
 * Created by zslajchrt on 29/04/15.
 */
class MorphKernelBaseRef[MT, MS](val instance: MorphKernelBase[MS], val altMappings: AltMappings, val sourceStrategy: Option[MorphingStrategy[MS]]) {
  type TargetModel = MT
}

abstract class MorphKernelRef[MT, MS](override val instance: MorphKernel[MS], override val altMappings: AltMappings, override val sourceStrategy: Option[MorphingStrategy[MS]] = None) extends MorphKernelBaseRef[MT, MS](instance, altMappings, sourceStrategy)

case class &[M](override val instance: MorphKernel[Any], override val altMappings: AltMappings, override val sourceStrategy: Option[MorphingStrategy[Any]] = None) extends MorphKernelRef[M, Any](instance.asInstanceOf[MorphKernel[Any]], altMappings, sourceStrategy)

//case class &~[M](override val instance: MorphKernel[Any], override val altMappings: AltMappings) extends MorphKernelRef[M, Any](instance.asInstanceOf[MorphKernel[Any]], altMappings)

case class ~&[M](override val instance: MorphKernel[Any], override val altMappings: AltMappings, override val sourceStrategy: Option[MorphingStrategy[Any]] = None) extends MorphKernelRef[M, Any](instance.asInstanceOf[MorphKernel[Any]], altMappings, sourceStrategy)

case class &?[M](override val instance: MorphKernel[Any], override val altMappings: AltMappings, override val sourceStrategy: Option[MorphingStrategy[Any]] = None) extends MorphKernelRef[M, Any](instance.asInstanceOf[MorphKernel[Any]], altMappings, sourceStrategy)

//case class &~?[M](override val instance: MorphKernel[Any], override val altMappings: AltMappings) extends MorphKernelRef[M, Any](instance.asInstanceOf[MorphKernel[Any]], altMappings)

case class ~&?[M](override val instance: MorphKernel[Any], override val altMappings: AltMappings, override val sourceStrategy: Option[MorphingStrategy[Any]] = None) extends MorphKernelRef[M, Any](instance.asInstanceOf[MorphKernel[Any]], altMappings, sourceStrategy)

case class &![M](override val instance: MorphKernel[Any], override val altMappings: AltMappings, override val sourceStrategy: Option[MorphingStrategy[Any]] = None) extends MorphKernelRef[M, Any](instance.asInstanceOf[MorphKernel[Any]], altMappings, sourceStrategy)
case class ~&![M](override val instance: MorphKernel[Any], override val altMappings: AltMappings, override val sourceStrategy: Option[MorphingStrategy[Any]] = None) extends MorphKernelRef[M, Any](instance.asInstanceOf[MorphKernel[Any]], altMappings, sourceStrategy)

// conformance level marker traits
trait ConformanceLevelMarker {
  val conformanceLevel: Morpheus.ConformanceLevel
}
trait PartialConformance extends ConformanceLevelMarker {
  override val conformanceLevel: ConformanceLevel = Morpheus.Partial
}
trait TotalConformance extends PartialConformance {
  override val conformanceLevel: ConformanceLevel = Morpheus.Total
}
//trait ExclusiveConformance extends ConformanceLevelMarker {
//  override val conformanceLevel: ConformanceLevel = Morpheus.Exclusive
//}


