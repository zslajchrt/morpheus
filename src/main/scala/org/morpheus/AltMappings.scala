package org.morpheus

import scala.io.Source
import scala.reflect.macros.whitebox
import scala.util.parsing.combinator.JavaTokenParsers

/**
 *
 * Created by zslajchrt on 13/04/15.
 */
object AltMappings {

  private [this] var altMapCache = Map.empty[String, AltMappings]

  def apply(altMapSerializedOrPath: String): AltMappings = {

    if (altMapSerializedOrPath.startsWith("file:")) {
      this.synchronized {
        altMapCache.get(altMapSerializedOrPath) match {
          case Some(am) => am
          case None =>
            val altMapResourcePath = altMapSerializedOrPath.drop("file:".length)
            val altMapIS = Thread.currentThread().getContextClassLoader.getResourceAsStream(altMapResourcePath)
            val altMapSource = Source.fromInputStream(altMapIS)
            val altMapContent = try altMapSource.mkString finally altMapSource.close()

            val parser = new AltMappingsParser
            val am = parser.parseAltMap(altMapContent).get
            altMapCache += (altMapSerializedOrPath -> am)

            am
        }
     }
    } else {
      val parser = new AltMappingsParser
      parser.parseAltMap(altMapSerializedOrPath).get
    }


  }
}

case class AltMappings(newFragToOrigFrag: Map[Int, Int], newAltToOrigAlt: Map[List[Int], List[OrigAlt]]) {

  import scala.util.parsing.combinator._

  class ReversePolishCalculator extends JavaTokenParsers {
    def num: Parser[Float] = floatingPointNumber ^^ (_.toFloat)
  }

  def serialize: String = toString

  lazy val sketch = newAltToOrigAlt.map(e => (e._1, e._2.map(_.fragments)))

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

    Morpheus.modelTypeFromAltTypes(c)(allAltTemplates)
//    val anyTpe = c.universe.rootMirror.typeOf[Any]
//    val unitTp = implicitly[WeakTypeTag[Unit]].tpe
//
//    val altLUBs: List[c.Type] =
//      (for (altTemplate <- allAltTemplates) yield Morpheus.conjunctionLUB(c)(altTemplate))
//        .map(tp => if (tp =:= anyTpe) unitTp else tp) // replace Any with Unit
//        .toList
//
//    val headAltTpe = altLUBs.head
//    val tpeTree = altLUBs.tail.foldLeft(tq"$headAltTpe")((res, altTpe) => {
//      tq"org.morpheus.Morpheus.or[$res, $altTpe]"
//    })
//
//    c.typecheck(tpeTree, mode = c.TYPEmode).tpe
  }

  def checkNoPlaceholder(): Unit = {
    for ((na, oaa) <- this.newAltToOrigAlt; oa <- oaa; fragSrc <- oa.template) {
      fragSrc match {
        case OriginalInstanceSource(_) => // ok
        case PlaceholderSource(_) => sys.error(s"AltMapping $this contains a placeholder $fragSrc")
      }
    }
  }

  def compose(other: AltMappings): AltMappings = {
    this.checkNoPlaceholder()
    other.checkNoPlaceholder()

    val composedNewFragToOrigFrag =
      for ((tf, sf) <- this.newFragToOrigFrag;
           sf2 <- other.newFragToOrigFrag.get(sf))
        yield (tf, sf2)

    def composeOrigAlt(origAlt: OrigAlt): List[OrigAlt] = {
      other.newAltToOrigAlt.get(origAlt.fragments) match {
        case None => Nil
        case Some(otherOrigAlts) => otherOrigAlts
      }
    }

//    val composedNewAltToOrigAlt: Map[List[Int], List[OrigAlt]] =
//      for ((newAlt, origAlts) <- this.newAltToOrigAlt;
//           origAlt <- origAlts)
//      yield (newAlt, composeOrigAlt(origAlt))

    val composedNewAltToOrigAlt = this.newAltToOrigAlt.map(e => {
      val (newAlt, origAlts) = e
      val composedOrigAlts = origAlts.flatMap(origAlt => {
        composeOrigAlt(origAlt)
      })
      (newAlt, composedOrigAlts.toSet.toList)
    })

    AltMappings(composedNewFragToOrigFrag, composedNewAltToOrigAlt)

  }

  override def toString: String = s"am($newFragToOrigFrag, $newAltToOrigAlt)"
}

class AltMappingsParser extends JavaTokenParsers {

  def num: Parser[Int] = floatingPointNumber ^^ (_.toInt)

  def int2intPair: Parser[(Int, Int)] = num ~ """->""" ~ num ^^ {
    case n1 ~ arr ~ n2 => (n1.toInt, n2.toInt)
  }

  def int2intPairDelim: Parser[(Int, Int)] = int2intPair ~ "," ^^ { case p1 ~ comma => p1 } | int2intPair

  def intDelim: Parser[Int] = num ~ "," ^^ { case n ~ _ => n } | num

  def numbers: Parser[List[Int]] = rep(intDelim)

  def intList: Parser[List[Int]] = "List(" ~ numbers ~ ")" ^^ { case _ ~ nums ~ _ => nums }

  def int2intPairList: Parser[List[(Int, Int)]] = rep(int2intPairDelim)

  def int2intMap: Parser[Map[Int, Int]] = "Map(" ~ int2intPairList ~ ")" ^^ { case _ ~ pairs ~ _ => pairs.toMap }

  def origInstSrc: Parser[OriginalInstanceSource] = "o(" ~ num ~ ")" ^^ { case _ ~ fragId ~ _ => OriginalInstanceSource(FragmentNode(fragId)) }

  def placeholderSrc: Parser[PlaceholderSource] = "p(" ~ num ~ ")" ^^ { case _ ~ fragId ~ _ => PlaceholderSource(FragmentNode(fragId, placeholder = true)) }

  def fragSrc: Parser[FragInstSource] = origInstSrc | placeholderSrc

  def fragSrcDelim: Parser[FragInstSource] = fragSrc ~ "," ^^ { case fs ~ _ => fs } | fragSrc

  def fragSrcList: Parser[List[FragInstSource]] = "List(" ~ rep(fragSrcDelim) ~ ")" ^^ { case _ ~ srcLst ~ _ => srcLst }

  def origAlt: Parser[OrigAlt] = "a(" ~ intList ~ "," ~ fragSrcList ~ ")" ^^ { case _ ~ l1 ~ _ ~ l2 ~ _ => OrigAlt(l1, l2) }

  def origAltDelim: Parser[OrigAlt] = origAlt ~ "," ^^ { case oa ~ _ => oa } | origAlt

  def origAltList: Parser[List[OrigAlt]] = "List(" ~ rep(origAltDelim) ~ ")" ^^ { case _ ~ origAlts ~ _ => origAlts }

  def alt2altPair: Parser[(List[Int], List[OrigAlt])] = intList ~ """->""" ~ origAltList ^^ { case l1 ~ arr ~ l2 => (l1, l2) }

  def alt2altPairDelim: Parser[(List[Int], List[OrigAlt])] = alt2altPair ~ "," ^^ { case p1 ~ comma => p1 } | alt2altPair

  def alt2altPairList: Parser[List[(List[Int], List[OrigAlt])]] = rep(alt2altPairDelim)

  def newAltToOrigAlt: Parser[Map[List[Int], List[OrigAlt]]] = "Map(" ~ alt2altPairList ~ ")" ^^ { case _ ~ pairs ~ _ => pairs.toMap }

  def altMappings: Parser[AltMappings] = "am(" ~ int2intMap ~ "," ~ newAltToOrigAlt ~ ")" ^^ { case _ ~ m1 ~ _ ~ m2 ~ _ => AltMappings(m1, m2) }

  def parseAltMap(s: String) = {
    parseAll(altMappings, s)
  }


  //def int2intMap: Parser[Map[Int, Int]] =

}


