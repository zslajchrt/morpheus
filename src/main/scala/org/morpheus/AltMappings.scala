package org.morpheus

import scala.collection.immutable.Iterable
import scala.reflect.macros.whitebox

/**
 *
 * Created by zslajchrt on 13/04/15.
 */
case class AltMappings(newFragToOrigFrag: Map[Int, Int], newAltToOrigAlt: Map[List[Int], List[OrigAlt]]) {

  def preserveDynamics(): AltMappings = {

    val origFragToNewFrag: Map[Int, Int] = newFragToOrigFrag.map(new2old => (new2old._2, new2old._1))

    // Convert the mapping so that the source (original) alternatives contains only the fragments having counterparts
    // in the target model. The original fragment ids are converted to those from the target model. The placeholders
    // are ignored.
    // Example 1: Valid mapping
    //      Source model: [A with B with \?C]
    //      Target model: [\?[C] with \?[$[X]]]
    //      Fragment mapping: 0 -> 2
    //      Alt mapping:
    //          () -> ((0, 1), (0, 1, 2))
    //          (1) -> ((0, 1), (0, 1, 2))
    //          (0) -> ((0, 1, 2))
    //          (0, 1) -> ((0, 1, 2))
    //      Result:
    //          1. () -> ((), (0))
    //          2. () -> ((), (0))
    //          3. (0) -> ((0))
    //          4. (0) -> ((0))
    //      Changes: 1,2->3,4: +0; the original alternatives CAN follow the changes

    // Example 2: Invalid mapping
    //      Source model: [A with B with C]
    //      Target model: [\?[C] with \?[$[X]]]
    //      Fragment mapping: 0 -> 2
    //      Alt mapping:
    //          () -> ((0, 1, 2))
    //          (1) -> ((0, 1, 2))
    //          (0) -> ((0, 1, 2))
    //          (0, 1) -> ((0, 1, 2))
    //      Result:
    //          1. () -> ((0))
    //          2. () -> ((0))
    //          3. (0) -> ((0))
    //          4. (0) -> ((0))
    //      Changes: 1,2->3,4: +0; the original alternatives CANNOT follow the changes

    // Strip placeholders from the target alts, filter out the hidden fragments from the original alts,
    // transform the remaining target fragments to their target counterparts and
    // map the stripped target alts to the filtered and transformed orig alts along with the original orig alts.

    val unifiedOrigAlts: Map[List[Int], List[(List[Int], OrigAlt)]] =
      for ((newAlt, origAlts) <- newAltToOrigAlt)
        yield (
          newAlt,
          for (origAlt <- origAlts)
            yield
            (for (origFragId <- origAlt.fragments if origFragToNewFrag.contains(origFragId))
              yield origFragToNewFrag(origFragId),
              origAlt)
          )

    val newToUnifiedNewAlts: Map[List[Int], List[Int]] =
      for ((newAlt, _) <- newAltToOrigAlt)
        yield (newAlt, for (newFragId <- newAlt if newFragToOrigFrag.contains(newFragId)) yield newFragId)
    //val unifiedNewToNewAlts = newToUnifiedNewAlts.map(a => (a._2, a._1))

    // Remove OrigAlts from unifiedAlts and convert new alts to unified new alts
    case class UnifiedAltHolder(unifiedNewAlt: List[Int], unifiedOrigAlts: List[List[Int]])
    var origAltsWithTargetFrags: Map[List[Int], UnifiedAltHolder] = unifiedOrigAlts.map(e => (e._1, UnifiedAltHolder(newToUnifiedNewAlts(e._1), e._2.map(_._1))))

    val newAlts = origAltsWithTargetFrags.keySet.toList

    val mappingCnt: Int = origAltsWithTargetFrags.size

    // Try all combinations of possible transitions between the target alternatives and check if
    // the change list of fragments is identical with the transitions between the corresponding original alternatives.
    // Retain only such original transitions having the same fragment change list.
    for (i <- 0 until mappingCnt;
         newAlt1 = newAlts(i);
         j <- (i + 1) until mappingCnt;
         newAlt2 = newAlts(j)) {

      val origAlts1Holder = origAltsWithTargetFrags(newAlt1)
      val origAlts1 = origAlts1Holder.unifiedOrigAlts
      val origAlts2Holder = origAltsWithTargetFrags(newAlt2)
      val origAlts2 = origAlts2Holder.unifiedOrigAlts

      val changesBetweenNewAlts = changeSet(origAlts1Holder.unifiedNewAlt, origAlts2Holder.unifiedNewAlt)
      val validAlts1 =
        for (origAlt1 <- origAlts1 if changesBetweenNewAlts.outgoing.forall(!origAlt1.contains(_))) // the start alt must have no outgoing fragment
          yield origAlt1
      val validAlts2 =
        for (origAlt2 <- origAlts2 if changesBetweenNewAlts.incoming.forall(!origAlt2.contains(_))) // the end alt must have no incoming fragment
          yield origAlt2

      origAltsWithTargetFrags += (newAlt1 -> origAlts1Holder.copy(unifiedOrigAlts = validAlts1))
      origAltsWithTargetFrags += (newAlt2 -> origAlts2Holder.copy(unifiedOrigAlts = validAlts2))
    }

    // Reconstruct the mapping from origAltsWithTargetFrags containing the reduced mappings

    val newAltsToAllowedOrigAlts: Map[List[Int], List[OrigAlt]] =
      for ((newAlt, UnifiedAltHolder(_, allowedOrigAlts)) <- origAltsWithTargetFrags) yield (newAlt, {
        val allowed: List[OrigAlt] = unifiedOrigAlts(newAlt).
          filter(origAlt => allowedOrigAlts.contains(origAlt._1)). // retain only those orig alts that are among the allowed orig alts
          map(_._2) // accommodate the result to List[OrigAlt]
        allowed
    })

    copy(newAltToOrigAlt = newAltsToAllowedOrigAlts)
  }

  case class ChangeSet(outgoing: Set[Int], incoming: Set[Int])

  private def changeSet(alt1: List[Int], alt2: List[Int]): ChangeSet = {
    val notInAlt2 = for (f1 <- alt1 if !alt2.contains(f1)) yield f1
    val notInAlt1 = for (f2 <- alt2 if !alt1.contains(f2)) yield f2
    ChangeSet(outgoing = notInAlt1.toSet, incoming = notInAlt2.toSet)
  }


  def toCompositeType(c: whitebox.Context)(srcTypes: (FragmentNode) => c.Type, tgtTypes: (FragmentNode) => c.Type): c.Type = {
    import c.universe._

    val allAltTemplates: Set[List[c.Type]] =
      (for ((_, origAlts) <- newAltToOrigAlt;  origAlt <- origAlts)
        yield for (fragSrc <- origAlt.template) yield fragSrc match {
          case OriginalInstanceSource(fn) => srcTypes(fn)
          case PlaceholderSource(fn) => tgtTypes(fn)
        }
        ).toSet

    val anyTpe = c.universe.rootMirror.typeOf[Any]
    val unitTp = implicitly[WeakTypeTag[Unit]].tpe

    val altLUBs: List[c.Type] =
      (for (altTemplate <- allAltTemplates) yield Morpheus.conjunctionLUB(c)(altTemplate)._1)
        .map(tp => if (tp =:= anyTpe) unitTp else tp) // replace Any with Unit
        .toList

    val headAltTpe = altLUBs.head
    val tpeTree = altLUBs.tail.foldLeft(tq"$headAltTpe")((res, altTpe) => {
      tq"org.morpheus.Morpheus.or[$res, $altTpe]"
    })

    c.typecheck(tpeTree, mode = c.TYPEmode).tpe
  }

}

