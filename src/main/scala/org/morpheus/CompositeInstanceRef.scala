package org.morpheus

import org.morpheus.Morpheus.ConformanceLevel

/**
 * Created by zslajchrt on 29/04/15.
 */
class CompositeProtoInstanceRef[MT, MS](val instance: CompositeProtoInstance[MS], val altMappings: AltMappings, val sourceStrategy: Option[MorpherStrategy[MS]]) {
  type TargetModel = MT
}

abstract class CompositeInstanceRef[MT, MS](override val instance: CompositeInstance[MS], override val altMappings: AltMappings, override val sourceStrategy: Option[MorpherStrategy[MS]] = None) extends CompositeProtoInstanceRef[MT, MS](instance, altMappings, sourceStrategy)

case class &[M](override val instance: CompositeInstance[Any], override val altMappings: AltMappings, override val sourceStrategy: Option[MorpherStrategy[Any]] = None) extends CompositeInstanceRef[M, Any](instance.asInstanceOf[CompositeInstance[Any]], altMappings, sourceStrategy)

//case class &~[M](override val instance: CompositeInstance[Any], override val altMappings: AltMappings) extends CompositeInstanceRef[M, Any](instance.asInstanceOf[CompositeInstance[Any]], altMappings)

case class ~&[M](override val instance: CompositeInstance[Any], override val altMappings: AltMappings, override val sourceStrategy: Option[MorpherStrategy[Any]] = None) extends CompositeInstanceRef[M, Any](instance.asInstanceOf[CompositeInstance[Any]], altMappings, sourceStrategy)

case class &?[M](override val instance: CompositeInstance[Any], override val altMappings: AltMappings, override val sourceStrategy: Option[MorpherStrategy[Any]] = None) extends CompositeInstanceRef[M, Any](instance.asInstanceOf[CompositeInstance[Any]], altMappings, sourceStrategy)

//case class &~?[M](override val instance: CompositeInstance[Any], override val altMappings: AltMappings) extends CompositeInstanceRef[M, Any](instance.asInstanceOf[CompositeInstance[Any]], altMappings)

case class ~&?[M](override val instance: CompositeInstance[Any], override val altMappings: AltMappings, override val sourceStrategy: Option[MorpherStrategy[Any]] = None) extends CompositeInstanceRef[M, Any](instance.asInstanceOf[CompositeInstance[Any]], altMappings, sourceStrategy)

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


