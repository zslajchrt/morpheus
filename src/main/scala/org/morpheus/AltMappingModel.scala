package org.morpheus

/**
 *
 * Created by zslajchrt on 31/03/15.
 */
object AltMappingModel {

  // the relation refers from the "older" to the "newer" model nodes

  sealed trait Node {
    val holder: () => FragmentHolder[_]
  }

  sealed trait Referred extends Node {
    val from: Int
    val to: Int
  }

  case class Hidden(holder: () => FragmentHolder[_]) extends Node

  case class FragmentInsertion(holder: () => FragmentHolder[_]) extends Node

  case class WrapperInsertion(holder: () => FragmentHolder[_]) extends Node

  case class Reference(from: Int, to: Int, holder: () => FragmentHolder[_]) extends Referred

  case class Replacement(from: Int, to: Int, holder: () => FragmentHolder[_]) extends Referred

  def transform(model1: List[Node], model2: List[Node]): List[Node] = {

    // the prefix of the model1 are fragment insertion, i.e. new orphan fragments
    val fragInsInModel1 = model1.takeWhile({
      case fi: FragmentInsertion => true
      case _ => false
    })

    var remainingInModel1 = model1.drop(fragInsInModel1.size)
    val (wrapperIns1, rem) = remainingInModel1.partition(_.isInstanceOf[WrapperInsertion])
    remainingInModel1 = rem

    val newModel2 = model2.map({
      case r2: Referred =>
        // find a node in model1 referring to the current node in model2
        remainingInModel1.find({
          case r1: Referred => r1.to == r2.from
          case _ => false
        }) match {
          case None =>
            // there is no node in model1 referring the current node in model2, thus no change
            r2
          case Some(n1) =>
            // there is some referring node in model1
            //val out = n1 match {
            n1 match {
              case ref1: Reference =>
                // It is just a reference. Use the original node from model1, (ref1 -> r2 = ref2)
                r2 match {
                  case Reference(_, _, _) =>
                    Reference(ref1.from, r2.to, r2.holder)
                  case Replacement(_, _, _) =>
                    Replacement(ref1.from, r2.to, r2.holder)
                }
              case repl1: Replacement =>
                // use the replacement from the model1, (repl1 -> r2 = repl1)
                Replacement(repl1.from, r2.to, repl1.holder)
              case _ => sys.error("Unexpected")
            }
        }
      case h: Hidden => h
      case fi: FragmentInsertion => fi
      case wi: WrapperInsertion => wi
      case _ => sys.error("Unexpected")
    })

    var newModel = fragInsInModel1 ::: newModel2 ::: wrapperIns1

    // It is possible that there is an insertion of a fragment already present as a hidden fragment. In such a case
    // the hidden fragment is removed. See method checkIfPlhdViolatesDeps in Morpheus.
    def removeHiddenHavingDuplicityInInsertion(): List[Node] = {
      newModel.filter({
        case Hidden(hiddenFragHolderFn) => !newModel.exists({
          case FragmentInsertion(insFragHolder) => hiddenFragHolderFn().fragment.fragTag.tpe =:= insFragHolder().fragment.fragTag.tpe
          case _ => false
        })
        case _ => true
      })
    }

    newModel = removeHiddenHavingDuplicityInInsertion()

    newModel
  }

  def apply(template: List[FragInstSource], newFragToOrigFragMap: Map[Int, Int], model1Holders: (FragmentNode) => FragmentHolder[_], model2Holders: (FragmentNode) => FragmentHolder[_]): List[Node] = {

//    // The prefix of the template consists of fragment insertions.
//    val fragInsertions = template.takeWhile({
//      case PlaceholderSource(fn) =>
//        newFragToOrigFragMap.find(_._1 == fn.id) match {
//          case None => // no counterpart
//            true
//          case Some(_) =>
//            false
//        }
//      case _ => false
//    }).map(plh => {
//      // Def: An element from model1 indicates a fragment insertion if it has no counterpart in model2 and all previous elements are also fragment insertions.
//      FragmentInsertion(() => model1Holders(plh.fragment))
//    })
//    val remaining = template.drop(fragInsertions.size)

    // Separate the non-wrapping and non-replacing fragment placeholders and place them in the front of the result template
    val (newNonWrapperPlhd, others) = template.partition({
      case PlaceholderSource(fn) =>
        val frgHolder: FragmentHolder[_] = model1Holders(fn)
        // A non-replacing fragment has no counterpart in model2
        !newFragToOrigFragMap.exists(_._1 == fn.id) && frgHolder.fragment.wrapperAnnotation.isEmpty

      case OriginalInstanceSource(_) => false
    })

    val (otherNonWrappers, wrappers) = others.partition(frgSrc => {
      val frgHolder = frgSrc match {
        case PlaceholderSource(fn) => model1Holders(fn)
        case OriginalInstanceSource(fn) => model2Holders(fn)
      }
      frgHolder.fragment.wrapperAnnotation.isEmpty
    })

    val fragInsertions = newNonWrapperPlhd.map(plh => FragmentInsertion(() => model1Holders(plh.fragment)))
    val nonInsertions = otherNonWrappers ::: wrappers

    val nonInsertionsNodes = nonInsertions.map({
      case OriginalInstanceSource(fn) =>
        newFragToOrigFragMap.find(_._2 == fn.id) match {
          case None =>
            // Def: An element from model2 indicates a hidden node if it has no counterpart in model1
            Hidden(() => model2Holders(fn))
          case Some(frag1ToFrag2Entry) =>
            // Def: An element from model2 indicates a reference if it is not a placeholder and has a counterpart in model1
            Reference(frag1ToFrag2Entry._1, frag1ToFrag2Entry._2, () => model2Holders(fn))
        }
      case PlaceholderSource(fn) =>
        newFragToOrigFragMap.find(_._1 == fn.id) match {
          case None =>
            // an inserted wrapper (model 1) never has the counterpart in the other alternative (model 2)
            // Def: An element from model2 indicates a wrapper insertion if it is a placeholder and has no counterpart in model1
            WrapperInsertion(() => model1Holders(fn))
          case Some(frag1ToFrag2Entry) =>
            // Def: An element from model2 indicates a replacement if it is a placeholder and has a counterpart in model1
            Replacement(frag1ToFrag2Entry._1, frag1ToFrag2Entry._2, () => model1Holders(fn))
        }
    })

    fragInsertions ::: nonInsertionsNodes
  }

}
