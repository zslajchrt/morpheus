package org.morpheus

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.{immutable, GenTraversableOnce}
import scala.language.experimental.macros
import scala.reflect.internal.Symbols
import scala.reflect.macros.whitebox


/**
 *
 * Created by zslajchrt on 29/01/15.
 */

import scala.reflect.runtime.universe._

object Morpheus {

  trait or[F0, F1]

  trait $[F]

  type \?[F] = or[Unit, F]
  type /?[F] = or[F, Unit]

  type ~[M] = MutableMorphMirror[M]

  /**
   * Using the identity type constructor help preserve type annotations in morph model types. See CompositeMappingsTest.
   * @tparam T
   */
  type ident[T] = T

  type dlg[T] = T

  def external[E](implicit instance: E): (Frag[E, Unit]) => E = _ => {
    instance
  }

  def existing[F, Cfg](implicit instance: F): (Frag[F, Cfg]) => F = _ => {
    instance
  }

  def expose[F](ci: MorphKernel[_]): Any = macro expose_impl[F]

  def frag[F]: FragmentFactory[F, Unit] = macro fragNoConf[F]

  def frag[F, C](cfg: C): FragmentFactory[F, C] = macro fragWithConf[F, C]

  def single[F]: FragmentFactory[F, Unit] = macro singleNoConf[F]

  def single[F, C](cfg: C): FragmentFactory[F, C] = macro singleWithConf[F, C]

  def mutableFragment(lst: MutableFragmentListener*): Frag[MutableFragment, MutableFragmentConfig] => MutableFragment = macro mutableFragment_impl

  def selectProxy[F](ci: MorphKernel[_]): Any = macro selectProxy_impl[F]

  implicit def fragmentHolderToFragmentFactory[F, C](holder: FragmentHolder[F]): (Frag[F, C] => F) = (frag) => {
    holder.proxy
  }

  implicit def byNameArgToFunction[T](t: => T): () => T = {
    () => t
  }

  implicit def byNameArgToFunctionOpt[T](t: => T): () => Option[T] = {
    () => Some(t)
  }

  def dependOnAlt[M, S](parentStrategy: PromotingStrategy[M, S], parentAlt: Int, switchFn: () => Option[Int]): () => Option[Int] = {
    dependOnAlt(parentStrategy.switchFn, parentAlt, switchFn)
  }

  def dependOnAlt(parentSwitchFn: () => Option[Int], parentAlt: Int, switchFn: () => Option[Int]): () => Option[Int] = {
    () => parentSwitchFn() match {
      case None => None
      case Some(channel) if channel == parentAlt => switchFn()
      case _ => None
    }
  }

  def parse[M](checkDeps: Boolean): MorphModel[M] = macro parse_impl[M]

  def parse[M](checkDeps: Boolean, removePlaceholders: Boolean): Any = macro parseWithRemovePlaceholders_impl[M]

  def build[M](compositeModel: MorphModel[M], checkDeps: Boolean, fragmentProvider: FragmentProvider,
               defaultStrategy: MorphingStrategy[M], conformanceLevel: org.morpheus.Morpheus.ConformanceLevel): Any = macro build_impl[M]

  def compose[M]: Any = macro compose_impl[M]

  def compose[M](compositeModel: MorphModel[M], defaultStrategy: MorphingStrategy[M]): Any = macro composeWithModel_impl[M]

  def singleton[M]: Any = macro singleton_impl[M]

  def singleton[M](compositeModel: MorphModel[M], defaultStrategy: MorphingStrategy[M]): Any = macro singletonWithModel_impl[M]

  //  @deprecated
  //  def arrangeY[M]: MorphModel[M] = macro arrange_impl[M]

  def glean[M]: Any = macro glean_impl[M]

  def glean[M](compositeModel: MorphModel[M], defaultStrategy: MorphingStrategy[M]): Any = macro gleanWithModel_impl[M]

  def compose_?[M]: Any = macro composePartial_impl[M]

  def singleton_?[M]: Any = macro singletonPartial_impl[M]

  def glean_?[M]: Any = macro gleanPartial_impl[M]

  //def \/[M1, M2](ci1: MorphKernelBase[M1], ci2: MorphKernelBase[M2]): Any = macro fork_impl[M1, M2]

  //def mirror[S](self: S): Option[S with MorphMirror[\?[S], Any]] = macro mirror_impl[S]
  def mirror[S](self: S): Any = macro mirror_impl[S]

  def remorph[S](arg: S, altNum: Int): Unit = macro remorph_impl[S]

  def select[F](mutableProxy: Any): Option[F] = macro select_impl[F]

  def inspect[T <: MutableMorphMirror[_], R](mutableProxy: T)(fork: PartialFunction[Any, R]): Any = macro inspect_impl[T, R]

  implicit def convertMorphToPartialRef[M1, M2](morph: MorphMirror[M1]): ~&[M2] = macro convertMorphToPartialRef_impl[M1, M2]

  implicit def convertMorphToTotalRef[M1, M2](morph: MorphMirror[M1]): &[M2] = macro convertMorphToTotalRef_impl[M1, M2]

  implicit def convertMorphKernelToPartialRef[M1, M2](ci: MorphKernel[M1]): ~&[M2] = macro convertMorphKernelToPartialRef_impl[M1, M2]

  implicit def convertMorphKernelToPartialRefNoDepsCheck[M1, M2](ci: MorphKernel[M1]): ~&?[M2] = macro convertMorphKernelToPartialRefNoDepsCheck_impl[M1, M2]

  implicit def convertMorphKernelToTotalRef[M1, M2](ci: MorphKernel[M1]): &[M2] = macro convertMorphKernelToTotalRef_impl[M1, M2]

  implicit def convertMorphKernelToTotalRefNoDepsCheck[M1, M2](ci: MorphKernel[M1]): &?[M2] = macro convertMorphKernelToTotalRefNoDepsCheck_impl[M1, M2]

  implicit def convertMorphKernelToTotalRefNoHidden[M1, M2](ci: MorphKernel[M1]): &![M2] = macro convertMorphKernelToTotalRefNoHidden_impl[M1, M2]

  def rootStrategy[M](src: Any): Any = macro rootStrategy_impl[M]

  //  implicit def convertMorphKernelToExclusiveRef[M1, M2](ci: MorphKernel[M1]): &~[M2] = macro convertMorphKernelToExclusiveRef_impl[M1, M2]
  //
  //  implicit def convertMorphKernelToExclusiveRefNoDepsCheck[M1, M2](ci: MorphKernel[M1]): &~?[M2] = macro convertMorphKernelToExclusiveRefNoDepsCheck_impl[M1, M2]

  def *[M](ciRef: MorphKernelRef[M, _], placeholders: Any*): Any = macro deref_impl[M]
  def *[M](ciRef: MorphKernelRef[M, _], strategy: MorphingStrategy[_], placeholders: Any*): Any = macro derefWithStrategy_impl[M]

  def selfRef(self: Any): Any = macro selfRefOpt_impl
  def &&(self: Any): Any = macro selfRef_impl
  def self(self: Any): Any = macro self_impl

  def tupled[T](arg: T): Any = macro tupled_impl[T]

  def deref[M](ciRef: MorphKernelRef[M, _], placeholders: Any*): Any = macro deref_impl[M]

  def proxies[M](ci: MorphKernel[M] with WithHiddenFragments): Any = macro proxies_impl[M]

  /**
   *
   * @param ci
   * @tparam M
   * @return the LUB of the submodel `M` of the model of the composite instance `ci`
   */
  def asMorphOf[M](ci: MorphKernel[_], placeholders: Any*): Any = macro asMorphOf_impl[M]

  def asMorphOf[M](morph: MorphMirror[_], placeholders: Any*): Any = macro asMorphOfFromMorph_impl[M]

  def asMorphOf_~[M](ci: MorphKernel[_], placeholders: Any*): Any = macro asMorphOfMutable_impl[M]

  def promote[M](sw: () => Option[Int]): Any = macro promote_implOneArg[M]

  def promote[S](delegate: MorphingStrategy[_], sw: () => Option[Int]): Any = macro promote_impl[S]

  def rate[M](sw: () => Set[(Int, Int)]): Any = macro rate_implOneArgPos[M]

  def rate_![M](sw: () => Set[(Int, Int)]): Any = macro rate_implOneArgNeg[M]

  def rate[S](delegate: MorphingStrategy[_], sw: () => Set[(Int, Int)]): Any = macro ratePos_impl[S]

  def rate_![S](delegate: MorphingStrategy[_], sw: () => Set[(Int, Int)]): Any = macro rateNeg_impl[S]

  def mask[M](sw: () => Option[Int]): Any = macro mask_implOneArg[M]

  def mask[S](delegate: MorphingStrategy[_], sw: () => Option[Int]): Any = macro mask_impl[S]

  def fragNoConf[F: c.WeakTypeTag](c: whitebox.Context): c.Expr[FragmentFactory[F, Unit]] = {
    import c.universe._

    val fragTpe = implicitly[WeakTypeTag[F]].tpe
    if (!isFragment(c)(fragTpe) && !isWrapper(c)(fragTpe)) {
      c.abort(c.enclosingPosition, s"Type $fragTpe is neither fragment or wrapper")
    }

    c.Expr[FragmentFactory[F, Unit]](q"new org.morpheus.FragmentFactory[$fragTpe, Unit](None)")
  }

  def fragWithConf[F: c.WeakTypeTag, C: c.WeakTypeTag](c: whitebox.Context)(cfg: c.Expr[C]): c.Expr[FragmentFactory[F, C]] = {
    import c.universe._

    val fragTpe = implicitly[WeakTypeTag[F]].tpe
    if (!isFragment(c)(fragTpe) && !isWrapper(c)(fragTpe)) {
      c.abort(c.enclosingPosition, s"Type $fragTpe is neither fragment or wrapper")
    }

    val cfgTpe = implicitly[WeakTypeTag[C]].tpe

    c.Expr[FragmentFactory[F, C]](q"new org.morpheus.FragmentFactory[$fragTpe, $cfgTpe](Some($cfg))")
  }

  def singleNoConf[F: c.WeakTypeTag](c: whitebox.Context): c.Expr[FragmentFactory[F, Unit]] = {
    import c.universe._

    val fragTpe = implicitly[WeakTypeTag[F]].tpe
    if (!isFragment(c)(fragTpe) && !isWrapper(c)(fragTpe)) {
      c.abort(c.enclosingPosition, s"Type $fragTpe is neither fragment or wrapper")
    }

    c.Expr[FragmentFactory[F, Unit]](q"new org.morpheus.FragmentFactory[$fragTpe, Unit](None) with org.morpheus.SingletonFragmentFactory[$fragTpe, Unit]")
  }

  def singleWithConf[F: c.WeakTypeTag, C: c.WeakTypeTag](c: whitebox.Context)(cfg: c.Expr[C]): c.Expr[FragmentFactory[F, C]] = {
    import c.universe._

    val fragTpe = implicitly[WeakTypeTag[F]].tpe
    if (!isFragment(c)(fragTpe) && !isWrapper(c)(fragTpe)) {
      c.abort(c.enclosingPosition, s"Type $fragTpe is neither fragment or wrapper")
    }

    val cfgTpe = implicitly[WeakTypeTag[C]].tpe

    c.Expr[FragmentFactory[F, C]](q"new org.morpheus.FragmentFactory[$fragTpe, $cfgTpe](Some($cfg)) with org.morpheus.SingletonFragmentFactory[$fragTpe, $cfgTpe]")
  }

  def remorph_impl[S: c.WeakTypeTag](c: whitebox.Context)(arg: c.Expr[Any], altNum: c.Expr[Int]): c.Expr[Unit] = {
    import c.universe._

    val res = arg.tree match {
      case This(_) =>
      // OK
        val self = arg
        val tp = self.actualType
        val thisSelfTpe = tp.typeSymbol.asClass.selfType
        //val cs: scala.reflect.internal.Symbols#RefinementClassSymbol = null

        //c.info(c.enclosingPosition, s"Remorph type: $thisSelfTpe, cls: ${thisSelfTpe.asInstanceOf[c.universe.TypeRef].sym.asClass.asInstanceOf[scala.reflect.internal.Symbols#ClassSymbol].tpe.parents}", true)

        q"""
          {
             import org.morpheus._
             import org.morpheus.Morpheus._
             for (m <- mirror(this); mm <- m.owningMutableProxy) {
               val self = &&(this)
               val ci = m.kernel
               val defaultStrategy = FixedStrategy[m.Model](m.alternatives)
               val am = self.altMappings
               val swModel = parse[$thisSelfTpe](false)
               val actualStrategy = PromotingStrategy(defaultStrategy, swModel, am, () => Some($altNum))
               mm.remorph(actualStrategy)
             }
             ()
           }
        """

      case _ =>

        if (arg.actualType.erasure <:< implicitly[WeakTypeTag[MorphKernelRef[_, _]]].tpe.erasure) {
          val ref = arg
          val targetModelTpe = ref.actualType.typeArgs.head

          q"""
           {
             import org.morpheus._
             import org.morpheus.Morpheus._
             val ci = $ref.instance
             val am = $ref.altMappings
             val swModel = parse[$targetModelTpe](false)
             val actualStrategy = PromotingStrategy(rootStrategy(ci.model), swModel, am, () => Some($altNum))
             val cc = ci.~
             cc.remorph(actualStrategy)
           }
         """

        } else {
          c.abort(c.enclosingPosition, s"The argument in remorph must be 'this' or a composite reference. Found ${show(arg)}.")
        }
    }


    c.Expr[Unit](res)
  }


  def expose_impl[F: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[MorphKernel[_]]): c.Expr[Any] = {
    import c.universe._

    c.Expr(q"org.morpheus.Morpheus.external($ci.fragments.select[org.morpheus.FragmentHolder[${implicitly[WeakTypeTag[F]]}]].proxy)")
  }

//  def expose_impl2[F: c.WeakTypeTag](c: whitebox.Context)(morph: c.Expr[MorphMirror[_]]): c.Expr[Any] = {
//    import c.universe._
//
//    //c.Expr(q"org.morpheus.Morpheus.external($ci.fragments.select[org.morpheus.FragmentHolder[${implicitly[WeakTypeTag[F]]}]].proxy)")
//    c.Expr(q"""
//              {
//                $.kernel.fragmentHolder[Contact] match {
//                  case None => sys.error("")
//                  case Some(holder) => holder.proxy
//                }
//              }
//           org.morpheus.Morpheus.external($ci.fragments.select[org.morpheus.FragmentHolder[${implicitly[WeakTypeTag[F]]}]].proxy)
//      """)
//  }

  def selectProxy_impl[F: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[MorphKernel[_]]): c.Expr[Any] = {
    import c.universe._

    c.Expr(q"$ci.fragments.select[org.morpheus.FragmentHolder[${implicitly[WeakTypeTag[F]]}]].proxy")
  }

  def self_impl(c: whitebox.Context)(self: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    val selfRef = selfRef_impl(c)(self)
    c.Expr(q"org.morpheus.Morpheus.*($selfRef)")
  }

  def selfRef_impl(c: whitebox.Context)(self: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    val selfRefOpt = selfRefOpt_impl(c)(self)
    c.Expr(q"$selfRefOpt.get")
  }

  def selfRefOpt_impl(c: whitebox.Context)(self: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    val depsTpe: Type = self.actualType
    val fragTpe = depsTpe match {
      case RefinedType(parents, _) => parents.head
      case _ => depsTpe
    }

    val refTpe = getConfLevelFromAnnotation(c)(fragTpe) match {
      case Total => tq"org.morpheus.&[org.morpheus.Morpheus.or[$depsTpe, Unit]]"
      case Partial => tq"org.morpheus.~&[org.morpheus.Morpheus.or[$depsTpe, Unit]]"
    }

    val res = q"""
        {
          import org.morpheus._
          import org.morpheus.Morpheus._
          mirror(this) match {
            case None => None
            case Some(m) =>
              val depsMaps = m.kernel.fragmentHolder[$fragTpe].get.fragment.depsMappings.get
              Some(new $refTpe(m.kernel.asInstanceOf[MorphKernel[Any]], depsMaps))
          }
        }

    """

    c.Expr(res)

  }

  def expandAlternatives(c: whitebox.Context)(modelTpe: c.Type): (Set[c.Type], List[(List[FragmentNode], List[c.Type], c.Type)]) = {
    import c.universe._

    var expandedFragTypes: Set[c.Type] = Set.empty[c.Type]

    var expandedAlts: List[(List[FragmentNode], List[c.Type], c.Type)] = Nil

    def expandAlts(altsModelTpe: c.Type): Unit = try {

      val altIter = alternativesIterator(c)(altsModelTpe, checkDeps = false, excludePlaceholders = false, Total)._3

      // For each alternative collect all dependencies of fragments and extend the alternative by the difference
      // between the set of complete dependencies and the set of types of the fragments constituting the alternative.
      while (altIter.hasNext) {
        val alt = altIter.next()

        //val fragsWithSelfTpe = alt._2.map(_.typeSymbol.asClass.selfType)
        var expandCnt = 0
        val fragsWithSelfTpe = alt._2.map(fragTpe => if (!expandedFragTypes.contains(fragTpe)) {
          expandCnt += 1
          expandedFragTypes += fragTpe
          fragTpe.typeSymbol.asClass.selfType
        } else {
          fragTpe
        })

        if (expandCnt > 0) {
          // some expansion occurred, thus dive one level more

          val hdTpeTree = tq"${fragsWithSelfTpe.head}"
          val altExpandedTpeTree = fragsWithSelfTpe.tail.foldLeft(hdTpeTree)((res, fragSelfTpe) => {
            tq"$res with $fragSelfTpe"
          })
          val altExpandedTpe = c.typecheck(altExpandedTpeTree, mode = c.TYPEmode).tpe

          expandAlts(altExpandedTpe)

        } else {
          // no expansion occurred in this alternative, thus add the alt to the result list
          expandedAlts ::= alt
        }
      }
    } catch {
      case e: Exception =>
        throw new Exception(s"Cannot expand alternatives of morph type $modelTpe", e)
    }

    expandAlts(modelTpe)

    (expandedFragTypes, expandedAlts.reverse)
  }

  def proxies_impl[M: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    //var completeAltLUBs = Set.empty[c.Type]

    val compTpe = implicitly[WeakTypeTag[M]].tpe.dealias
    val expandedAlts = expandAlternatives(c)(compTpe)
    val expandedFragTypes = expandedAlts._1

//    c.info(c.enclosingPosition, s"Expanded frag types: ${expandedAlts._1}", true)
//    c.info(c.enclosingPosition, s"Expanded alts: ${expandedAlts._2.map(_._2)}", true)

    // For each type found when traversing the dependencies create an implicit value

    def fragmentProxyImplicit(fragTpe: c.Type): Tree = {
      val proxyImplValName = TermName(c.freshName("proxy"))
      q"implicit def $proxyImplValName = $ci.fragmentHolder[$fragTpe].get.proxy"
    }

    var objectWithImplicitsTree: Tree = {
      val implVals: List[Tree] = expandedFragTypes.map(fragTpe => fragmentProxyImplicit(fragTpe)).toList
      q"""
        new {
          ..$implVals
        }
      """
    }

    objectWithImplicitsTree = c.typecheck(objectWithImplicitsTree)

    c.Expr(objectWithImplicitsTree)

  }

  def mutableFragment_impl(c: whitebox.Context)(lst: c.Expr[MutableFragmentListener]*): c.Expr[Frag[MutableFragment, MutableFragmentConfig] => MutableFragment] = {
    import c.universe._

    val lstArgs = lst.map(_.tree).toList
    val result = q"""
          import org.morpheus._
          import org.morpheus.Morpheus._
          single[MutableFragment, MutableFragmentConfig](MutableFragmentConfig(..$lstArgs))
    """

    c.Expr[Frag[MutableFragment, MutableFragmentConfig] => MutableFragment](result)
  }

  def rootStrategy_impl[M: c.WeakTypeTag](c: whitebox.Context)(src: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    val result = if (src.actualType.erasure <:< typeOf[MorphKernelRef[_, _]]) {
      val modelTpe = src.actualType.typeArgs.head.dealias
      val (_, modelRoot, _, _, _, fragmentTypesMap) = buildModel(c)(modelTpe, None, Total)
      val modelNoPlh = transformToNoPlaceholders(c)(modelRoot, fragmentTypesMap, None)
      val modelNoPlhTpe = c.typecheck(modelNoPlh, mode = c.TYPEmode).tpe

      q"""
         {
          val ref = $src.asInstanceOf[org.morpheus.MorphKernelRef[$modelNoPlhTpe, _]]
          org.morpheus.BridgeStrategy(ref)
         }
       """

    } else if (src.actualType.erasure <:< typeOf[MorphModel[_]]) {

      //val modelTpe = src.actualType.typeArgs.head.dealias
      q"org.morpheus.RootStrategy($src)"

    } else {
      c.abort(c.enclosingPosition, s"Illegal argument. Expected ${typeOf[MorphKernelRef[_, _]]} or ${typeOf[MorphModel[_]]}")
    }


    c.Expr(result)

  }

  def rate_implOneArgPos[M: c.WeakTypeTag](c: whitebox.Context)(sw: c.Expr[() => Set[(Int, Int)]]): c.Expr[Any] = {
    import c.universe._

    val modelTag: WeakTypeTag[M] = implicitly[WeakTypeTag[M]]
    val modelTpe = modelTag.tpe.dealias
    val delegate = c.Expr[MorphingStrategy[_]](c.typecheck(q"org.morpheus.RootStrategy[$modelTpe]()"))
    rate_impl(c)(delegate, sw, negative = false)(modelTag)
  }

  def rate_implOneArgNeg[M: c.WeakTypeTag](c: whitebox.Context)(sw: c.Expr[() => Set[(Int, Int)]]): c.Expr[Any] = {
    import c.universe._

    val modelTag: WeakTypeTag[M] = implicitly[WeakTypeTag[M]]
    val modelTpe = modelTag.tpe.dealias
    val delegate = c.Expr[MorphingStrategy[_]](c.typecheck(q"org.morpheus.RootStrategy[$modelTpe]()"))
    rate_impl(c)(delegate, sw, negative = true)(modelTag)
  }

  def ratePos_impl[S: c.WeakTypeTag](c: whitebox.Context)(delegate: c.Expr[MorphingStrategy[_]], sw: c.Expr[() => Set[(Int, Int)]]): c.Expr[Any] = {
    rate_impl(c)(delegate, sw, negative = false)(implicitly[c.WeakTypeTag[S]])
  }

  def rateNeg_impl[S: c.WeakTypeTag](c: whitebox.Context)(delegate: c.Expr[MorphingStrategy[_]], sw: c.Expr[() => Set[(Int, Int)]]): c.Expr[Any] = {
    rate_impl(c)(delegate, sw, negative = true)(implicitly[c.WeakTypeTag[S]])
  }

  private def rate_impl[S: c.WeakTypeTag](c: whitebox.Context)(delegate: c.Expr[MorphingStrategy[_]], sw: c.Expr[() => Set[(Int, Int)]], negative: Boolean): c.Expr[Any] = {
    import c.universe._

    val modelTpe = delegate.actualType.typeArgs.head.dealias
    val switchTpe = implicitly[WeakTypeTag[S]].tpe.dealias

    val switchModelTree = parseMorphModelFromType(c)(switchTpe, false, false, None, Total)._1
    val altMapTree = checkMorphKernelAssignment(c, s"Checking compatibility of switch model $switchTpe with $modelTpe")(modelTpe, switchTpe, false, Total, Total, false, noHiddenFragments = false)._1

    //val result = q"org.morpheus.AltMapRatingStrategy($delegate, $switchModelTree, $altMapTree, $sw, $negative)"
    val result = q"""
        {
          def createStrategy() = org.morpheus.AltMapRatingStrategy($delegate, $switchModelTree, $altMapTree, $sw, $negative)
          createStrategy
        }
       """

    c.Expr(result)
  }

  def promote_implOneArg[M: c.WeakTypeTag](c: whitebox.Context)(sw: c.Expr[() => Option[Int]]): c.Expr[Any] = {
    import c.universe._

    val modelTag: WeakTypeTag[M] = implicitly[WeakTypeTag[M]]
    val modelTpe = modelTag.tpe.dealias
    val delegate = c.Expr[MorphingStrategy[_]](c.typecheck(q"org.morpheus.RootStrategy[$modelTpe]()"))
    promote_impl(c)(delegate, sw)(modelTag)
  }

  def promote_impl[S: c.WeakTypeTag](c: whitebox.Context)(delegate: c.Expr[MorphingStrategy[_]], sw: c.Expr[() => Option[Int]]): c.Expr[Any] = {
    import c.universe._

    val modelTpe = delegate.actualType.typeArgs.head.dealias
    val switchTpe = implicitly[WeakTypeTag[S]].tpe.dealias

    val switchModelTree = parseMorphModelFromType(c)(switchTpe, false, false, None, Total)._1
    val altMapTree = checkMorphKernelAssignment(c, s"Checking compatibility of switch model $switchTpe with $modelTpe")(modelTpe, switchTpe, false, Total, Total, false, noHiddenFragments = false)._1

    //val result = q"org.morpheus.PromotingStrategy($delegate, $switchModelTree, $altMapTree, $sw)"

    // Wrapping the creation of the strategy to the local method helps spreading the JVM code accross the class.
    // Otherwise it would tend to the accumulation of enormous number of instructions as long as this macro would
    // be invoked several times in the same method.
    val result =  q"""
         {
            def createStrategy() = org.morpheus.PromotingStrategy($delegate, $switchModelTree, $altMapTree, $sw)
            createStrategy
         }
       """

    c.Expr(result)
  }

  def mask_implOneArg[M: c.WeakTypeTag](c: whitebox.Context)(sw: c.Expr[() => Option[Int]]): c.Expr[Any] = {
    import c.universe._

    val modelTag: WeakTypeTag[M] = implicitly[WeakTypeTag[M]]
    val modelTpe = modelTag.tpe.dealias
    val delegate = c.Expr[MorphingStrategy[_]](c.typecheck(q"org.morpheus.RootStrategy[$modelTpe]()"))
    mask_impl(c)(delegate, sw)(modelTag)
  }

  def mask_impl[S: c.WeakTypeTag](c: whitebox.Context)(delegate: c.Expr[MorphingStrategy[_]], sw: c.Expr[() => Option[Int]]): c.Expr[Any] = {
    import c.universe._

    val modelTpe = delegate.actualType.typeArgs.head.dealias
    val switchTpe = implicitly[WeakTypeTag[S]].tpe.dealias

    val switchModelTree = parseMorphModelFromType(c)(switchTpe, false, false, None, Total)._1
    val altMapTree = checkMorphKernelAssignment(c, s"Checking compatibility of switch model $switchTpe with $modelTpe")(modelTpe, switchTpe, false, Total, Total, false, noHiddenFragments = false)._1

    val result =  q"""
         {
            def createStrategy() = org.morpheus.MaskingStrategy($delegate, $switchModelTree, $altMapTree, $sw)
            createStrategy
         }
       """

    c.Expr(result)
  }

  def asMorphOfFromMorph_impl[M: c.WeakTypeTag](c: whitebox.Context)(morph: c.Expr[MorphMirror[_]], placeholders: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    asMorphOf_impl(c)(c.Expr[MorphKernel[_]](q"$morph.kernel"), placeholders:_*)(implicitly[WeakTypeTag[M]])
  }

  def asMorphOf_impl[M: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[MorphKernel[_]], placeholders: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val plhList = placeholders.toList

    val targetTpe = implicitly[WeakTypeTag[M]].tpe

    val result = q"""
        {
            import org.morpheus._
            import org.morpheus.Morpheus._
            val specCompRef: ~&?[$targetTpe] = $ci
            *(specCompRef, ..$placeholders).make
        }
      """
    c.Expr(result)
  }

  def asMorphOfMutable_impl[M: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[MorphKernel[_]], placeholders: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

//    val plhList = placeholders.toList
//    val plhListTree = q"List(..$plhList)"

    val targetTpe = implicitly[WeakTypeTag[M]].tpe

    val result = q"""
        {
            import org.morpheus._
            import org.morpheus.Morpheus._
            val specCompRef: &?[$targetTpe] = $ci
            *(specCompRef, ..$placeholders).make_~
        }
      """
    c.Expr(result)
  }

  def tupled_impl[T: c.WeakTypeTag](c: whitebox.Context)(arg: c.Expr[T]): c.Expr[Any] = {
    import c.universe._

    // Currently, only MorphKernelRef can be tuplified
    val result = if (arg.actualType.erasure <:< implicitly[WeakTypeTag[MorphKernelRef[_, _]]].tpe) {
      val compTpe = arg.actualType.typeArgs.head
      val (_, modelRoot, _, _, _, typesMap) = buildModel(c)(compTpe, None, Total)

      val compInstTree = q"$arg.instance"

      val fragFactTrees = modelRoot.fragments.filterNot(_.placeholder).map(fn => {
        val (fragTpe, cfgClsOpt) = typesMap(fn.id)
        val fragFactTree = cfgClsOpt match {
          case Some(cf) =>
            q"(f: org.morpheus.Frag[$fragTpe, $cf]) => $compInstTree.fragmentHolder[$fragTpe].get.proxy"
          case None =>
            q"(f: org.morpheus.Frag[$fragTpe, Unit]) => $compInstTree.fragmentHolder[$fragTpe].get.proxy"
        }
        c.typecheck(fragFactTree)
      })

      q"(..$fragFactTrees)"

    } else {
      c.abort(c.enclosingPosition, s"Illegal argument")
    }


    c.Expr(result)
  }

  def deref_impl[M: c.WeakTypeTag](c: whitebox.Context)(ciRef: c.Expr[MorphKernelRef[M, _]], placeholders: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    derefWithStrategy_impl[M](c)(ciRef, null, placeholders :_*)
  }

  def derefWithStrategy_impl[M: c.WeakTypeTag](c: whitebox.Context)(ciRef: c.Expr[MorphKernelRef[M, _]], strategy: c.Expr[MorphingStrategy[_]], placeholders: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    // The placeholders can be passed as a tuple. The following code detects such a condition and transforms the tuple to a sequence
    val actualPlaceholders = if (placeholders.size == 1 && placeholders.head.actualType <:< implicitly[WeakTypeTag[Product]].tpe) {
      val placeholdersTuple = placeholders.head
      val argTypes = placeholdersTuple.actualType.typeArgs
      for (argIndex <- 0 until argTypes.size)
        yield c.Expr(c.typecheck(q"$placeholdersTuple.productElement($argIndex).asInstanceOf[${argTypes(argIndex)}]"))
    } else {
      placeholders
    }

    val existRefTpe = implicitly[WeakTypeTag[~&[_]]].tpe
    val existRefNoDepsCheckTpe = implicitly[WeakTypeTag[~&?[_]]].tpe
    val inclRefTpe = implicitly[WeakTypeTag[&[_]]].tpe
    val inclRefNoDepsCheckTpe = implicitly[WeakTypeTag[&?[_]]].tpe
    val inclRefNoHiddenTpe = implicitly[WeakTypeTag[&![_]]].tpe

    val noHiddenFragments = inclRefNoHiddenTpe.erasure =:= ciRef.actualType.erasure

    def conformanceLevel: (Morpheus.ConformanceLevel, c.Tree) = {
      val (confLev, confTpe) = if (noHiddenFragments) {
        (Morpheus.Total, c.universe.rootMirror.typeOf[TotalConformance])
      } else if (existRefTpe.erasure =:= ciRef.actualType.erasure || existRefNoDepsCheckTpe.erasure =:= ciRef.actualType.erasure) {
        (Morpheus.Partial, c.universe.rootMirror.typeOf[PartialConformance])
      } else if (inclRefTpe.erasure =:= ciRef.actualType.erasure || inclRefNoDepsCheckTpe.erasure =:= ciRef.actualType.erasure) {
        (Morpheus.Total, c.universe.rootMirror.typeOf[TotalConformance])
      } else {
        c.abort(c.enclosingPosition, s"Unexpected reference type ${ciRef.actualType}")
      }

      (confLev, tq"$confTpe")
    }

    val confLev = conformanceLevel

    val tgtTpe = implicitly[WeakTypeTag[M]].tpe
    // transform the composite type so as not to contain placeholders
    val (_, modelRoot, _, _, _, typesMap) = buildModel(c)(tgtTpe, None, confLev._1)

    // verify that the placeholders arguments contain all necessary placeholder factories
    val placeholderFrags = modelRoot.fragments.filter(_.placeholder).map(phf => (phf, typesMap(phf.id)._1))
    val fragFactFnTpe: Type = c.universe.rootMirror.typeOf[(Frag[_, _] => _)]
    val fragFactFnCls = fragFactFnTpe.typeSymbol.asClass
    val fragFactFnArg0 = fragFactFnCls.typeParams.head.asType.toType

    def findFragmentFactoryFunction(fragArgTpe: c.Type): Option[c.Type] = if (fragArgTpe.erasure <:< fragFactFnTpe.erasure) {
      val fragFnActualTpe = fragFactFnArg0.asSeenFrom(fragArgTpe, fragFactFnCls)
      Some(fragFnActualTpe.typeArgs.head)
    } else {
      None
    }

    def findPlaceholderFragmentTypeForArgument(plArgTpe: c.Type): List[(FragmentNode, c.Type)] = {
      val isArgWrapper = isWrapper(c)(plArgTpe)

      val plhFragsWitTypes = placeholderFrags.filter(phFragWithTpe => {
        val phFragTpe = phFragWithTpe._2
        val isFragWrapper = isWrapper(c)(phFragTpe)
        if ((isArgWrapper && !isFragWrapper) || (!isArgWrapper && isFragWrapper)) {
          // the arg's and the frag's kind must match
          false
        } else {
          //if (!isFragment(c)(phFragTpe) && !isWrapper(c)(phFragTpe)) {
          if (isAbstractFragment(c)(phFragTpe)) {
            plArgTpe <:< phFragTpe
          } else {
            plArgTpe =:= phFragTpe
          }
        }
      })

      if (plhFragsWitTypes.isEmpty) {
        c.abort(c.enclosingPosition, s"Placeholder fragment not found for placeholder argument $plArgTpe")
      } else {
        plhFragsWitTypes
      }
    }

    // Maps declared placeholder type to the actual actual type
    var placeholderArgTypes: List[(c.Type, c.Type)] = Nil

    def getFragTypeForDeclType(declTpe: c.Type): Option[c.Type] = {
      placeholderArgTypes.find(plaArgTpe => compareFragmentDeclarations(c)(plaArgTpe._1, declTpe)).flatMap(t => Some(t._2))
    }

    val placeholderMapFn: PartialFunction[c.Type, c.Type] = new PartialFunction[c.Type, c.Type] {
      override def isDefinedAt(declTpe: c.Type): Boolean = getFragTypeForDeclType(declTpe).isDefined

      override def apply(declTpe: c.Type): c.Type = getFragTypeForDeclType(declTpe).get
    }

    val placeholderFactMapEntries: List[Tree] = actualPlaceholders.flatMap(placeholderArg => {
      findFragmentFactoryFunction(placeholderArg.actualType) match {
        case None =>
          c.abort(c.enclosingPosition, s"Placeholder argument $placeholderArg is not a fragment factory function (Frag[F, C] => F)")
        case Some(placeholderFragTpe) =>
          val plhFragmentsForArg: List[(FragmentNode, c.Type)] = findPlaceholderFragmentTypeForArgument(placeholderFragTpe)
          plhFragmentsForArg.map(plhFragForArg => {
            placeholderArgTypes ::= (plhFragForArg._2, placeholderFragTpe)
            q"${plhFragForArg._1.id} -> $placeholderArg.asInstanceOf[org.morpheus.Frag[_, _] => _]"
          })
      }
    }).toList

    val placeholderFactoriesTree = q"Map(..$placeholderFactMapEntries)"

    //c.info(c.enclosingPosition, s"Placeholder fragments: ${show(placeholderFactMap)}", true)

    val missingPlaceholders = placeholderFrags.filter(phFragWithTpe => {
      // the declared type
      val phFragTpe = phFragWithTpe._2
      !getFragTypeForDeclType(phFragTpe).isDefined
    })

    if (missingPlaceholders.nonEmpty) {
      c.abort(c.enclosingPosition, s"Some placeholder arguments missing: ${missingPlaceholders.map(_._2).mkString(",")}. Placeholder mapping: $placeholderArgTypes")
    }

    val defStrategy = if (strategy == null)
      None
    else
      Some(strategy)

    composeOrGlean_impl[M](c)(CopyProvider(ciRef, placeholderFactoriesTree, confLev._1, confLev._2, delegation = false, noHiddenFragments),
      checkDeps = false, None, defStrategy, Some(placeholderMapFn), None)
  }

  private def compareFragmentDeclarations(c: whitebox.Context)(fd1: c.Type, fd2: c.Type): Boolean = {
    import c.universe._

    val ret = if (fd1.isInstanceOf[AnnotatedTypeApi] && fd2.isInstanceOf[AnnotatedTypeApi]) {
      val afd1 = fd1.asInstanceOf[AnnotatedTypeApi]
      val afd2 = fd2.asInstanceOf[AnnotatedTypeApi]
      afd1.underlying =:= afd2.underlying && afd1.annotations == afd2.annotations
    } else if (!fd1.isInstanceOf[AnnotatedTypeApi] && !fd2.isInstanceOf[AnnotatedTypeApi]) {
      fd1 =:= fd2
    } else {
      false
    }

    ret

  }

  def convertMorphToPartialRef_impl[M1: c.WeakTypeTag, M2: c.WeakTypeTag](c: whitebox.Context)(morph: c.Expr[MorphMirror[M1]]): c.Expr[~&[M2]] = {
    import c.universe._

    val tgtTpe = implicitly[WeakTypeTag[M2]].tpe.dealias
    val srcTpe = implicitly[WeakTypeTag[M1]]
    val res = q"""
      {
        import org.morpheus._
        val ref: ~&[$tgtTpe] = convertMorphKernelToPartialRef[$srcTpe, $tgtTpe]($morph.kernel)
        ref.copy(sourceStrategy = Some(new LastRatingStrategy($morph.asInstanceOf[MorphMirror[Any]]))).asInstanceOf[~&[$tgtTpe]]
      }
    """
    c.Expr[~&[M2]](res)
  }

  def convertMorphToTotalRef_impl[M1: c.WeakTypeTag, M2: c.WeakTypeTag](c: whitebox.Context)(morph: c.Expr[MorphMirror[M1]]): c.Expr[&[M2]] = {
    import c.universe._

    val tgtTpe = implicitly[WeakTypeTag[M2]].tpe.dealias
    val srcTpe = implicitly[WeakTypeTag[M1]]
    val res = q"""
      {
        import org.morpheus._
        val ref: &[$tgtTpe] = convertMorphKernelToTotalRef[$srcTpe, $tgtTpe]($morph.kernel)
        ref.copy(sourceStrategy = Some(new LastRatingStrategy($morph.asInstanceOf[MorphMirror[Any]]))).asInstanceOf[&[$tgtTpe]]
      }
    """
    c.Expr[&[M2]](res)
  }

  def convertMorphKernelToPartialRef_impl[M1: c.WeakTypeTag, M2: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[MorphKernel[M1]]): c.Expr[~&[M2]] = {
    import c.universe._

    val tgtTpe = implicitly[WeakTypeTag[M2]].tpe.dealias
    val isDereferenced = ci.actualType <:< c.typeOf[WithHiddenFragments]

    val altMappingsExpr = checkMorphKernelAssignmentUsingTypeArgs[M1, M2](c)(ci, checkDepsInSrcModel = !isDereferenced, refConfLevel = Partial, noHiddenFragments = false)

    val res = q"new ~&[$tgtTpe]($ci.asInstanceOf[org.morpheus.MorphKernel[Any]], $altMappingsExpr)"
    c.Expr[~&[M2]](res)
  }

  def convertMorphKernelToPartialRefNoDepsCheck_impl[M1: c.WeakTypeTag, M2: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[MorphKernel[M1]]): c.Expr[~&?[M2]] = {
    import c.universe._

    val tgtTpe = implicitly[WeakTypeTag[M2]].tpe.dealias
    val isDereferenced = ci.actualType <:< c.typeOf[WithHiddenFragments]

    val altMappingsExpr = checkMorphKernelAssignmentUsingTypeArgs[M1, M2](c)(ci, checkDepsInSrcModel = false, refConfLevel = Partial, noHiddenFragments = false)

    val res = q"new ~&?[$tgtTpe]($ci.asInstanceOf[org.morpheus.MorphKernel[Any]], $altMappingsExpr)"
    c.Expr[~&?[M2]](res)
  }

  def convertMorphKernelToTotalRef_impl[M1: c.WeakTypeTag, M2: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[MorphKernel[M1]]): c.Expr[&[M2]] = {
    import c.universe._

    //val srcTpe = implicitly[WeakTypeTag[M1]].tpe.dealias
    val tgtTpe = implicitly[WeakTypeTag[M2]].tpe.dealias

    //c.info(c.enclosingPosition, s"convertMorphKernelToTotalRef: $srcTpe -> $tgtTpe", true)

    val isDereferenced = ci.actualType <:< c.typeOf[WithHiddenFragments]

    // The dependencies needn't be checked as long as the composite instance is created from a composite reference, since
    // we trust the reference to refer a consistent composite.
    // If the reference's composite type contains a dependent fragment reference (non-placeholder), the dereferenced composite instance must contain
    // all required dependencies for such a fragment.
    // If the reference's composite type contains a fragment placeholder, the placeholder's dependencies check is carried out independently
    // within the validation of the placeholder.
    val altMappingsExpr = checkMorphKernelAssignmentUsingTypeArgs[M1, M2](c)(ci, checkDepsInSrcModel = !isDereferenced, refConfLevel = Total, noHiddenFragments = false)

    val res = q"new &[$tgtTpe]($ci.asInstanceOf[org.morpheus.MorphKernel[Any]], $altMappingsExpr)"
    c.Expr[&[M2]](res)
  }

  def convertMorphKernelToTotalRefNoDepsCheck_impl[M1: c.WeakTypeTag, M2: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[MorphKernel[M1]]): c.Expr[&?[M2]] = {
    import c.universe._

    val altMappingsExpr = checkMorphKernelAssignmentUsingTypeArgs[M1, M2](c)(ci, checkDepsInSrcModel = false, refConfLevel = Total, noHiddenFragments = false)

    val tgtTpe = implicitly[WeakTypeTag[M2]].tpe.dealias
    val res = q"new &?[$tgtTpe]($ci.asInstanceOf[org.morpheus.MorphKernel[Any]], $altMappingsExpr)"
    c.Expr[&?[M2]](res)
  }

  def convertMorphKernelToTotalRefNoHidden_impl[M1: c.WeakTypeTag, M2: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[MorphKernel[M1]]): c.Expr[&![M2]] = {
    import c.universe._

    val tgtTpe = implicitly[WeakTypeTag[M2]].tpe.dealias

    val isDereferenced = ci.actualType <:< c.typeOf[WithHiddenFragments]

    val altMappingsExpr = checkMorphKernelAssignmentUsingTypeArgs[M1, M2](c)(ci, checkDepsInSrcModel = !isDereferenced, refConfLevel = Total, noHiddenFragments = true)

    val res = q"new &![$tgtTpe]($ci.asInstanceOf[org.morpheus.MorphKernel[Any]], $altMappingsExpr)"
    c.Expr[&![M2]](res)
  }

  sealed trait ConformanceLevel
  object ConformanceLevel {

    val PARTIAL = "partial"
    val TOTAL = "total"
    //    val EXCLUSIVE = "excl"

    def unapply(refConfLevelStr: String): Option[ConformanceLevel] = refConfLevelStr match {
      case PARTIAL => Some(Partial)
      case TOTAL => Some(Total)
      //        case EXCLUSIVE => Some(Exclusive)
      case _ => None
    }
  }

  case object Partial extends ConformanceLevel
  case object Total extends ConformanceLevel

  private def checkMorphKernelAssignmentUsingTypeArgs[M1: c.WeakTypeTag, M2: c.WeakTypeTag](c: whitebox.Context)(ci: c.Expr[MorphKernel[M1]], checkDepsInSrcModel: Boolean, refConfLevel: ConformanceLevel, noHiddenFragments: Boolean): c.Expr[AltMappings] = {
    import c.universe._

    val srcTpe = implicitly[WeakTypeTag[M1]].tpe.dealias
    val tgtTpe = implicitly[WeakTypeTag[M2]].tpe.dealias

    val withHiddenFragmentsTpe = c.typeOf[WithHiddenFragments]
    val containsHiddenFragments = ci.actualType <:< withHiddenFragmentsTpe

    val confLevSym: Symbol = ci.actualType.member(TypeName("ConformLevel"))
    require(confLevSym != NoSymbol, s"ConformLevel type member not found in kernel type ${ci.actualType}")
    val srcConfLevTpe = confLevSym.asType.toType
    //c.info(c.enclosingPosition, s"Source conf level tpe: ${srcConfLevTpe}, is total: ${srcConfLevTpe <:< implicitly[WeakTypeTag[TotalConformance]].tpe}", true)

    val srcConfLev = srcConfLevTpe match {
    //val srcConfLev = ci.actualType match {
      case t if t <:< implicitly[WeakTypeTag[TotalConformance]].tpe => Total
      case _ => Partial
//      case t if t <:< implicitly[WeakTypeTag[PartialConformance]].tpe => Partial
//      //      case t if t <:< implicitly[WeakTypeTag[ExclusiveConformance]].tpe => Exclusive
//      case _ =>
//        c.abort(c.enclosingPosition, s"Missing conformance level marker on composite instance")
    }

    try {
      checkMorphKernelAssignment(c, s"Checking compatibility of kernel reference type $tgtTpe with source type $srcTpe")(srcTpe, tgtTpe, checkDepsInSrcModel, srcConfLev, refConfLevel, containsHiddenFragments, noHiddenFragments)._1
    }
    catch {
      case dchk: DependencyCheckException =>
        c.abort(c.enclosingPosition, dchk.getMessage)
    }
  }

  /**
   *
   * @param c
   * @param srcTpe
   * @param tgtTpe
   * @param checkDepsInSrcModel
   * @param tgtConfLev
   * @param containsHiddenFragments Used in the placeholder processing only. It indicates that the composite instance
   *                                may contain other fragments than those specified in the composite type.
   *                                In such a case the placeholder is rejected unless it is a simple replacement or a wrapper.
   * @return the AltMappings AST or the list of unsatisfied target alternatives
   */
  private def checkMorphKernelAssignment(c: whitebox.Context, ctxMsg: String)(srcTpe: c.Type, tgtTpe: c.Type,
                                                                    checkDepsInSrcModel: Boolean, srcConfLev: ConformanceLevel,
                                                                    tgtConfLev: ConformanceLevel, containsHiddenFragments: Boolean,
                                                                    noHiddenFragments: Boolean): (c.Expr[AltMappings], AltMappings) = {
    import c.universe._

    val printCounter = new AtomicInteger()

    //var newFragToOrigFrag: Map[Int, Int] = Map.empty
    var origFragToNewFrag: Map[Int, Int] = Map.empty
    //var newAltToOrigAlt: Map[List[Int], List[OrigAlt]] = Map.empty
    var altMap = AltMappings(Map.empty, Map.empty)
    //var origAltToTemplateAlt: Map[List[Int], List[FragInstSource]] = Map.empty

    implicit class PimpFragInstSource(fragSrc: FragInstSource) {
      def toTree: Tree = fragSrc match {
        case PlaceholderSource(FragmentNode(id, placeholder)) => q"org.morpheus.PlaceholderSource(org.morpheus.FragmentNode($id, $placeholder))"
        case OriginalInstanceSource(FragmentNode(id, placeholder)) => q"org.morpheus.OriginalInstanceSource(org.morpheus.FragmentNode($id, $placeholder))"
      }
    }

    implicit class PimpOrigAlt(origAlt: OrigAlt) {
      def toTree: Tree = {
        val fragmentListTree = q"List(..${origAlt.fragments})"
        val fragSrcTrees: List[Tree] = origAlt.template.map(_.toTree)
        val templateTree = q"List(..$fragSrcTrees)"
        q"org.morpheus.OrigAlt($fragmentListTree, $templateTree)"
      }

    }

    def pseudoCodeTree(): Tree = {
      val newAltToOrigAltTree: Iterable[Tree] = altMap.newAltToOrigAlt.map(entry => {
        val tgtAltIds: List[Int] = entry._1
        val origAlts: List[OrigAlt] = entry._2
        val origAltTrees: List[Tree] = origAlts.map(_.toTree)
        val tgtAltTree = q"List(..$tgtAltIds)"
        val origAltListTree = q"List(..$origAltTrees)"
        q"$tgtAltTree -> $origAltListTree"
      })

      val newFragToOrigFragTree: Iterable[Tree] = altMap.newFragToOrigFrag.map(entry => {
        val newFragId: Int = entry._1
        val origFragId: Int = entry._2
        q"$newFragId -> $origFragId"
      })

      q"org.morpheus.AltMappings(Map(..$newFragToOrigFragTree), Map(..$newAltToOrigAltTree))"
    }

    def updatePseudoCodeAST(tgtAlt: (List[FragmentNode], List[c.Type], c.Type), srcAlt: (List[FragmentNode], List[c.Type], c.Type), altTemplate: List[FragInstSource]): Unit = {

      var newAltToOrigAlt = altMap.newAltToOrigAlt
      val tgtAltIds = tgtAlt._1.map(_.id)
      val srcAltIds = srcAlt._1.map(_.id)
      newAltToOrigAlt.get(tgtAltIds) match {
        case None =>
          newAltToOrigAlt += (tgtAltIds -> List(OrigAlt(srcAltIds, altTemplate)))
        case Some(curSrcAlts) =>
          newAltToOrigAlt += (tgtAltIds -> (OrigAlt(srcAltIds, altTemplate) :: curSrcAlts))
      }

      altMap = altMap.copy(newAltToOrigAlt = newAltToOrigAlt)

    }

    // todo: UNCOMMENT!
    //val (srcRoot, srcFragmentTypesMap, srcAltIter) = alternativesIterator(c)(srcTpe, checkDepsInSrcModel, excludePlaceholders = true /*irrelevant, there are no placeholder in src*/, srcConfLev)
    val (srcRoot, srcFragmentTypesMap, srcAltIter) = alternativesIterator(c)(srcTpe, checkDeps = false, excludePlaceholders = true /*irrelevant, there are no placeholder in src*/, srcConfLev)
    val (tgtRoot, tgtFragmentTypesMap, tgtAltIter) = alternativesIterator(c)(tgtTpe, checkDeps = false, excludePlaceholders = true, tgtConfLev)

    // find and associate corresponding fragments in both models
    var newFragToOrigFrag = Map.empty[Int, Int]
    for ((srcFragId, (srcFragTpe, _)) <- srcFragmentTypesMap;
         (tgtFragId, (tgtFragTpe, _)) <- tgtFragmentTypesMap) {
      if (isAbstractFragment(c)(tgtFragTpe)) {
        if (srcFragTpe <:< tgtFragTpe && !isWrapper(c)(srcFragTpe)) {
          newFragToOrigFrag += (tgtFragId -> srcFragId)
          origFragToNewFrag += (srcFragId -> tgtFragId)
        }
      } else {
        if (srcFragTpe =:= tgtFragTpe) {
          newFragToOrigFrag += (tgtFragId -> srcFragId)
          origFragToNewFrag += (srcFragId -> tgtFragId)
        }
      }
    }

    def fragSrcPretty(fragSrc: FragInstSource): String = {
      fragSrc match {
        case OriginalInstanceSource(frag) => toShortName(c)(srcFragmentTypesMap(frag.id)._1)
        case PlaceholderSource(frag) => s"$$[${toShortName(c)(tgtFragmentTypesMap(frag.id)._1)}]"
      }
    }

    def toFragSrc(fn: FragmentNode): Option[FragInstSource] = if (fn.placeholder)
      Some(PlaceholderSource(fn))
    else newFragToOrigFrag.get(fn.id) match {
      case None => None
      case Some(srcFragId) => Some(OriginalInstanceSource(FragmentNode(srcFragId, placeholder = false)))
    }

    def toFragType(fragInst: FragInstSource): c.Type = fragInst match {
      case PlaceholderSource(fn) => tgtFragmentTypesMap(fn.id)._1
      case OriginalInstanceSource(fn) => srcFragmentTypesMap(fn.id)._1
    }

    // Take only such antagonists where both sides have their counterparts in the source model
    val tgtAnts = tgtRoot.createAntagonistsMatrix((fn1, fn2) => tgtFragmentTypesMap(fn1.id)._1 =:= tgtFragmentTypesMap(fn2.id)._1)
    val antagonists: Set[(FragInstSource, FragInstSource)] =
      for ((ant1, ant2) <- tgtAnts; fs1 <- toFragSrc(ant1); fs2 <- toFragSrc(ant2))
        yield (fs1, fs2)

    //    val srcAnts = srcRoot.createAntagonistsMatrix((fn1, fn2) => srcFragmentTypesMap(fn1.id)._1 =:= srcFragmentTypesMap(fn2.id)._1)
//    val antagonists: Set[(FragInstSource, FragInstSource)] =
//      for ((ant1, ant2) <- srcAnts)
//        yield (OriginalInstanceSource(ant1), OriginalInstanceSource(ant2))

    def templateAntagonists(template: List[FragInstSource]): List[(FragInstSource, FragInstSource)] = {
      for (f1 <- template;
           f2 <- template if antagonists.contains((f1, f2))) yield (f1, f2)
    }

    val anyTpe = c.universe.rootMirror.typeOf[Any]

    altMap = altMap.copy(newFragToOrigFrag = newFragToOrigFrag)

    def findFragmentWithSameDependees(placeholderTpe: c.Type, dependees: Set[FragmentNode], srcAlt: List[FragmentNode]): Option[FragmentNode] = {

      // Look for a corresponding fragment having the same dependees as the placeholder.
      // We can use the placeholder only if such a fragment exists.

      // we try all fragments as if they were placeholders

      srcAlt.find(altFrag => {
        if (isCorrespondingSourceAltNode(altFrag, placeholderTpe)) {
          val altFragTpe = srcFragmentTypesMap(altFrag.id)._1
          val altFragDependees = collectPotentialDependees(altFragTpe, srcAlt)
            .filter(_ != altFrag) // remove the fragment from the result
          dependees == altFragDependees
        } else {
          false
        }

      })
    }

    def collectPotentialWrapperDependees(placeholderTpe: c.Type, alt: List[FragmentNode]): Set[FragmentNode] = {
      if (isWrapper(c)(placeholderTpe)) {
        // Wrappers have no dependees by definition
        Set.empty
      } else {

        alt.map(altInstSrc => {
          val altNodeTpe = srcFragmentTypesMap(altInstSrc.id)._1
          if (isFragmentWrapper(c)(altNodeTpe)) {
            // does the wrapper extend the placeholder?
            if (altNodeTpe <:< placeholderTpe)
              Some(altInstSrc)
            else
              None
          } else if (isDimensionWrapper(c)(altNodeTpe)) {
            findDimension(c)(altNodeTpe) match {
              case None => None
              case Some(dim) =>
                // does the placeholder implement the same dimension?
                if (placeholderTpe <:< dim)
                  Some(altInstSrc)
                else
                  None
            }
          } else {
            None
          }
        }).filter(_.isDefined).map(_.get).toSet

      }

    }

    def collectPotentialDependeesByFragDeps(placeholderTpe: c.Type, srcAlt: List[FragmentNode]): Set[FragmentNode] = {
      // the list of fragments from the source alternative whose dependencies can be satisfied by the placeholder
      val dependees: List[FragmentNode] = srcAlt.map(altInstSrc => {

        val altNodeTpe = srcFragmentTypesMap(altInstSrc.id)._1
        getDependencyType(c)(altNodeTpe, excludeHead = true) match {
          case None => None
          case Some(depsTpe) =>

            // try to find a dependency of the alt node that can be satisfied by the placeholder
            val altNodeDeps = alternativesIterator(c)(depsTpe, false, false, Partial)._3.toList
            altNodeDeps.find(altNodeDeps => {
              altNodeDeps._2.exists(altDep => {
                placeholderTpe <:< altDep
              })
            }) match {
              case None => None
              case Some(satisDep) => Some(altInstSrc)
            }
        }

      }
      ).filter(_.isDefined).map(_.get) // filter out the None elements

      dependees.toSet
    }

    def collectPotentialDependees(placeholderTpe: c.Type, srcAlt: List[FragmentNode]): Set[FragmentNode] = {
      collectPotentialDependeesByFragDeps(placeholderTpe, srcAlt) ++ collectPotentialWrapperDependees(placeholderTpe, srcAlt)
    }

    def isSameAltNode(srcAltFrag: FragmentNode, placeholderTpe: c.Type): Boolean = {
      val srcAltTpe = srcFragmentTypesMap(srcAltFrag.id)._1
      srcAltTpe =:= placeholderTpe
    }

    def isCorrespondingSourceAltNode(srcAltFrag: FragmentNode, placeholderTpe: c.Type): Boolean = {
      val srcAltFragTpe = srcFragmentTypesMap(srcAltFrag.id)._1
      if (srcAltFragTpe =:= placeholderTpe) {
        true
      } else {

        if (isFragment(c)(placeholderTpe)) {
          // if the placeholder and the source alt fragment are in the same dimension then they are replaceable
          val d1: Option[c.Type] = findDimension(c)(srcAltFragTpe)
          val d2: Option[c.Type] = findDimension(c)(placeholderTpe)
          (for (x <- d1; y <- d2) yield x =:= y) match {
            case None => false
            case Some(isDimSame) => isDimSame
          }
        } else if (!isWrapper(c)(placeholderTpe)) {
          // a dimension or a plain trait

          // provided that the placeholder is not referenced within the alternative it is possible to use it
          // as a replacement for the compatible fragment
          srcAltFragTpe <:< placeholderTpe
        } else {
          // wrappers are not replaceable unless the placeholder and the src alt fragment are same
          false
        }

      }

    }

    def findPrecedingNodeForWrapper(placeholderTpe: c.Type, srcAlt: List[FragmentNode]): Option[FragmentNode] = {

      def compareWrappedTpeWithPlaceholderWrappedTpe(srcAltNode: FragmentNode) = {
        val tpe: c.Type = srcFragmentTypesMap(srcAltNode.id)._1

        val (wrappedTpeByAlt, wrappedTpeByPlaceholder) = if (isDimensionWrapper(c)(placeholderTpe))
          (findDimension(c)(tpe), findDimension(c)(placeholderTpe))
        else {
          if (isDimensionWrapper(c)(tpe))
            (findDimension(c)(tpe), findDimension(c)(placeholderTpe))
          else
            (findFragment(c)(tpe), findFragment(c)(placeholderTpe))
        }

        (for (wa <- wrappedTpeByAlt; wp <- wrappedTpeByPlaceholder) yield wa =:= wp) match {
          case None => false
          case Some(isWrapperTpeSame) => isWrapperTpeSame
        }
      }

      // find the first from left
      // todo: is it really correct? Shouldn't it be done in the other way round?
      srcAlt.find(compareWrappedTpeWithPlaceholderWrappedTpe)
    }

    def checkIfPlhdViolatesDeps(placeholder: FragmentNode, srcAlt: List[FragmentNode]): Either[(FragmentNode, Option[FragmentNode]), String] = {
      val placeholderTpe = tgtFragmentTypesMap(placeholder.id)._1

      // try to find the corresponding fragment to the placeholder in the source alt
      // btw: it ensures the orthogonality of the fragments
      srcAlt.find(isSameAltNode(_, placeholderTpe)) match {
        case Some(same) =>
          // a simple replacement
          Left(placeholder, Some(same))
        case None =>

          if (isWrapper(c)(placeholderTpe)) {
            // Wrappers cannot have dependees by definition. We must only find the corresponding fragment to be wrapped.

            findPrecedingNodeForWrapper(placeholderTpe, srcAlt) match {
              case None => Right(s"No fragment to be wrapped by placeholder $placeholderTpe") // there is no fragment to be wrapped by the placeholder TODO: provide some error message
              case Some(wrappedNode) =>
                Left(placeholder, Some(wrappedNode)) // in the case of placeholder wrapper the second node is interpreted as the one after which the first node is placed
            }

          } else {
            // In the case the composite instance contains hidden fragments we must abort the process since
            // we cannot prove there is no fragment dependent on the placeholder.
            if (containsHiddenFragments) {
              Right(s"Source model contains hidden fragments, so we cannot prove there is no fragment dependent on the placeholder $placeholderTpe")
            } else {

              // try to detect the interfering fragment by analyzing dependencies
              val potentialDependees: Set[FragmentNode] = collectPotentialDependees(placeholderTpe, srcAlt)
              if (potentialDependees.isEmpty) {
                // there is no interference with the dependency graph

                srcAlt.find(isCorrespondingSourceAltNode(_, placeholderTpe)) match {
                  case None =>
                    Left(placeholder, None)
                  case Some(corresp) =>
                    Left(placeholder, Some(corresp))
                }

              } else {
                // there is some interference, find out if the placeholder can be used
                findFragmentWithSameDependees(placeholderTpe, potentialDependees, srcAlt) match {
                  case None if checkDepsInSrcModel =>
                    // The checkDepsInSrcModel flag indicates some source fragments may be missing (i.e. the source composite is incomplete).
                    // These missing fragment may be in conflict with the placeholder.
                    Right(s"Source model is incomplete. may be in conflict with placeholder $placeholderTpe")
                  case None if !checkDepsInSrcModel =>
                    // So if it is not set then we can assume that the placeholder can be used without interfering with the dependencies in the source.
                    Left(placeholder, None)
                  case Some(interferingFragment) =>
                    val interFragTpe = srcFragmentTypesMap(interferingFragment.id)
                    Left(placeholder, Some(interferingFragment))
                }
              }
            }
          }
      }

    }

    // todo: The garbage collection of non-referenced fragments should not be the default behavior. It could cause
    // todo: problems in situations where there are some indirect references in the source composite (like listeners e.g.).
    // todo: Let's assume this example: val ref: &[A] = compose[A with B with C], where A -> B and C -> B, A fires an event
    // todo: through B and C listens to this event on B. If the garbage collection were in place the C would be removed and
    // todo: its side effects would vanish causing some unexpected consequences.

    def checkTypeAnnotations(tgtFragTypes: List[c.Type], srcFragTypes: List[c.Type]): Boolean = {

      def annotationsMatch(srcTpe: c.Type, tgtAnn: List[Annotation]): Boolean = {
        val srcAnn = getTypeAnnotations(c)(srcTpe).toSet ++ getAnnotations(c)(srcTpe).toSet
        val res = tgtAnn.forall(tgtAn => {
          srcAnn.exists(_.tree.equalsStructure(tgtAn.tree))
        })
        //c.info(c.enclosingPosition, s"Source fragment type $srcTpe annotations: $srcAnn for target anns: $tgtAnn, result=$res", true)
        res
      }

      val res = tgtFragTypes.forall(tgtFragTpe => {
//        if (tgtFragTpe.isInstanceOf[AnnotatedType]) {
//          c.info(c.enclosingPosition, s"Found annotated type: $tgtFragTpe", true)
//        }

        val tgtAnn: List[Annotation] = getTypeAnnotations(c)(tgtFragTpe)
        if (tgtAnn.isEmpty) {
          true
        } else {

          val res = srcFragTypes.exists(srcFragTpe => if (srcFragTpe <:< tgtFragTpe) {
            annotationsMatch(srcFragTpe, tgtAnn)
          } else {
            false
          })

          //c.info(c.enclosingPosition, s"Searching type annotations: $tgtAnn, res: $res", true)
          res
        }


        //        tgtAnn match {
//          case Nil =>
//            true
//          case tgtAnn =>
//            c.info(c.enclosingPosition, s"Checking target annotations $tgtAnn", true)
//            srcFragTypes.exists(srcTpe => {
//              annotationsMatch(srcTpe, tgtAnn)
//            })
//        }
      })

      //c.info(c.enclosingPosition, s"Checking type annotations: src: $srcFragTypes, tgt: $tgtFragTypes, res: $res", true)

      res


    }

    def checkDepsOfPlaceholders(altTemplate: List[FragInstSource]): Unit = {
      val altTemplateTpe = conjunctionLUB(c)(altTemplate.map(toFragType(_)))
      for (altFragSrc <- altTemplate) {

        altFragSrc match {
          case OriginalInstanceSource(_) =>
          case PlaceholderSource(fn) =>
            val plhTpe = toFragType(altFragSrc)
            try {
              val plhConfLevelMode: ConformanceLevel = getConfLevelFromAnnotation(c)(plhTpe)
              plhTpe.typeSymbol.asClass.selfType match {
                case RefinedType(parents, scope) =>
                  val depsTpe = internal.refinedType(parents.tail, scope)
                  checkMorphKernelAssignment(c, s"Checking placeholder $plhTpe dependencies")(altTemplateTpe, depsTpe, false, tgtConfLev, plhConfLevelMode, false, noHiddenFragments = false)._1
                case _ => // no placeholder's dependencies
              }
            }
            catch {
              case dchck: DependencyCheckException =>
                throw new DependencyCheckException(s"Unsatisfied placeholder $plhTpe dependencies\n: $dchck")
            }
        }
      }

    }

    /*
     * @return the alternative template
     */
    def isTargetAltCompatibleWithSourceAlt(tgtAlt: (List[FragmentNode], List[c.Type], c.Type), srcAlt: (List[FragmentNode], List[c.Type], c.Type)): Either[List[FragInstSource], String] = {

      val tgtAltLUB = tgtAlt._3
      val srcAltLUB = srcAlt._3

      val ret = if (srcAltLUB <:< tgtAltLUB) {

        //if (!checkTypeAnnotations(tgtAlt._1.map(f => tgtFragmentTypesMap(f.id)._1), srcAlt._2)) {
        if (!checkTypeAnnotations(tgtAlt._2, srcAlt._2)) {
          Right(s"Some target annotations not found in the source model")
        } else {

          // check if no placeholder violates inner dependencies in the source composite
          val placeholders = tgtAlt._1.filter(_.placeholder)
          // a list of replacements by placeholders. The first fragment is the placeholder,
          // the second is the source alt node being replaced or appended (in case of a wrapper) by the placeholder
          val placeholdersApplication: List[Either[(FragmentNode, Option[FragmentNode]), String]] = placeholders.map(checkIfPlhdViolatesDeps(_, srcAlt._1))
          if (placeholdersApplication.exists(_.isRight)) {
            // some placeholders cannot be applied
            // provide some explanation which placeholders and why they do not match
            Right(s"Some placeholders cannot be applied: ${placeholdersApplication.filter(_.isRight).map(_.right.get)}")
          } else {
            // all placeholders can be applied

            //c.info(c.enclosingPosition, s"placeholdersApplication: $placeholdersApplication", true)

            // orig frag -> placeholder replacement mapping
            val validPlaceholdersApplication: List[(FragmentNode, Option[FragmentNode])] = placeholdersApplication.map(_.left.get)
            val replacements: List[(FragmentNode, FragmentNode)] = validPlaceholdersApplication.
              filter(_._2.isDefined).map(pa => (pa._1, pa._2.get))

            // 'orphan' placeholders
            val notInterferingPlaceholders = validPlaceholdersApplication.
              filter(!_._2.isDefined).map(_._1)
            // 'orphan' placeholders converted to PlaceholderSource
            val notInterferingPlaceholdersAsSources = notInterferingPlaceholders.map(PlaceholderSource)

            var i = 0
            // for all source alt nodes try to find a placeholder(s) to be replaced with
            val remainingAltFragsAsSources = srcAlt._1.flatMap(srcAltNode => {
              // there can be more placeholders for one source node
              val replForSrc: List[FragmentNode] = replacements.filter(_._2.id == srcAltNode.id).map(_._1)
              if (replForSrc.isEmpty) {
                //c.info(c.enclosingPosition, s"No placeholder: srcAltNode: $srcAltNode", true)
                List(OriginalInstanceSource(srcAltNode))
              } else {

                /**
                 * A representation of the source alt node replacement.
                 * Param fragSrc the non-wrapper fragment source. Initially, it is the original fragment src.
                 * Param wrappers the new wrappers of the non-wrapper fragment. Initially it is Nil.
                 */
                case class SrcFragReplacement(fragSrc: FragInstSource = OriginalInstanceSource(srcAltNode), wrappers: List[FragInstSource] = Nil)

                val initialSrcFragRepl = SrcFragReplacement()

                val r = replForSrc.foldLeft(initialSrcFragRepl)((repl, placeholder) => {
                  val placeholderTpe = tgtFragmentTypesMap(placeholder.id)._1
                  val altNodeTpe = srcFragmentTypesMap(srcAltNode.id)._1

                  if (!(altNodeTpe =:= placeholderTpe) && isWrapper(c)(placeholderTpe)) {
                    // append the wrapper only if the altNodeTpe and placeholderTpe are different
                    i += 1
                    repl.copy(wrappers = repl.wrappers ::: List(PlaceholderSource(placeholder)))
                  } else {
                    repl.copy(fragSrc = PlaceholderSource(placeholder))
                  }
                })
                r.fragSrc :: r.wrappers
              }
            })


            // the alternative template
            val altTemplate = notInterferingPlaceholdersAsSources ::: remainingAltFragsAsSources

            val foundAntagonists: List[(FragInstSource, FragInstSource)] = templateAntagonists(altTemplate)
            if (foundAntagonists.nonEmpty) {
              Right(s"Target alternative template ${altTemplate.map(fragSrcPretty(_))} contains antagonists ${foundAntagonists.map(tpl => (fragSrcPretty(tpl._1), fragSrcPretty(tpl._2)))}")
            } else {
              Left(altTemplate)
            }

          }
        }
      } else {
        Right(s"Source LUB $srcAltLUB cannot be assigned to $tgtAltLUB")
      }

      ret match {
        case Right(_) => ret
        case Left(altTemplate) =>
          try {
            checkDepsOfPlaceholders(altTemplate)

            if (noHiddenFragments) {
              // all source fragments must be also in the target alternative template
              val srcFragsInAltTemplate: Set[FragmentNode] = altTemplate.filter(_.isInstanceOf[OriginalInstanceSource]).map(_.fragment).toSet
              val srcAltAsSet: Set[FragmentNode] = srcAlt._1.toSet
              if (srcAltAsSet != srcFragsInAltTemplate) {
                val hiddenFrags = (srcAltAsSet.--(srcFragsInAltTemplate)).map(hf => srcFragmentTypesMap(hf.id)._1)
                Right(s"Some hidden source fragments in the target alternative template: ${hiddenFrags.mkString(",")}")
              } else {
                ret
              }
            } else {
              ret
            }

          } catch {
            case dce: DependencyCheckException =>
              Right(dce.getMessage)
          }
      }

    }

    var targetAltCounter = 0
    var allTargetAlts = List.empty[List[c.Type]]
    var matchingTargetAltCounter = 0
    var matchingNonTrivialTargetAltCounter = 0
    var sourceAltCounter = 0
    var unsatisfiedTargetAlts = Set.empty[(List[c.Type] /*tgt alt*/, List[c.Type] /*src alt*/, String /*reason*/)]
    var independentTargetAltCounter = 0

    val emptySrcAlt: (Nil.type, Nil.type, Type) = (Nil, Nil, anyTpe)

    var altRelation = Set.empty[(List[FragmentNode], List[FragmentNode])]

    while (tgtAltIter.hasNext) {
      val tgtAlt = tgtAltIter.next()
      // Transform fragment nodes to fragment types
      val tgtAltFragTypes = tgtAlt._1.map(tgtFrag => tgtFragmentTypesMap(tgtFrag.id)._1)
      // Used for reporting only now.
      allTargetAlts ::= tgtAltFragTypes

      srcAltIter.reset()
      var matches = false

      sourceAltCounter = 0
      while (srcAltIter.hasNext) {
        val srcAlt = srcAltIter.next()
        // Transform fragment nodes to fragment types
        val srcAltFragTypes = srcAlt._1.map(srcFrag => srcFragmentTypesMap(srcFrag.id)._1)

        sourceAltCounter += 1

        isTargetAltCompatibleWithSourceAlt(tgtAlt, srcAlt) match {
          case Right(reason) =>
            val incompatibleTgtAndSrcAltsWithReason: (List[c.Type], List[c.Type], String) = (tgtAltFragTypes, srcAltFragTypes, reason)
            unsatisfiedTargetAlts += incompatibleTgtAndSrcAltsWithReason
          case Left(altTemplate) =>
            updatePseudoCodeAST(tgtAlt, srcAlt, altTemplate)
            val rel: (List[FragmentNode], List[FragmentNode]) = (tgtAlt._1, srcAlt._1)
            altRelation += rel
            matches = true
        }

      }

      if (!matches) {
        // No source alt matches the target alt.
        // Attempt to save the situation by offering the empty source alternative.
        isTargetAltCompatibleWithSourceAlt(tgtAlt, emptySrcAlt) match {
          case Right(reason) =>
          // the attempt failed
          case Left(altTemplate) =>
            // the attempt was successful
            updatePseudoCodeAST(tgtAlt, emptySrcAlt, altTemplate)
            independentTargetAltCounter += 1
            matches = true
        }
      }

      if (matches) {
        matchingTargetAltCounter += 1
        if (tgtAlt._1.nonEmpty)
          matchingNonTrivialTargetAltCounter += 1
      }
      targetAltCounter += 1
    }

    val isCompatible = (tgtConfLev, srcConfLev) match {
      case (Partial, Partial) => independentTargetAltCounter > 0 || altRelation.map(_._2).size == sourceAltCounter // right-total relation, all source alts must have found their target alts
      case (Partial, Total) => matchingNonTrivialTargetAltCounter > 0 // non-empty relation
      case (Total, Partial) => altRelation.size == targetAltCounter * sourceAltCounter // cartesian product
      case (Total, Total) => matchingTargetAltCounter == targetAltCounter // left-total relation
    }

    def resultTree() = {
      val pcTree: Tree = pseudoCodeTree()
      c.Expr[AltMappings](pcTree)
    }

    if (!isCompatible) {
      srcAltIter.reset()
      tgtAltIter.reset()

      def printUnsatisfiedAltsPair(unstfAltPair: (List[c.Type] /*tgt alt*/, List[c.Type] /*src alt*/, String /*reason*/)): String = {
        val tgtAlt = toShortNames(c)(unstfAltPair._1)
        val srcAlt = toShortNames(c)(unstfAltPair._2)
        val reason = unstfAltPair._3
        s"""
            Target alt: $tgtAlt
            Source alt: $srcAlt
            Reason: $reason
        """
      }

      throw new DependencyCheckException(s"""
           *** $ctxMsg ***

           Reference of $tgtTpe:$tgtConfLev does not conform $srcTpe:$srcConfLev.
           Unsatisfied alternatives:${unsatisfiedTargetAlts.toList.map(printUnsatisfiedAltsPair).mkString("---")}
           Source root: $srcRoot
           Target root: $tgtRoot
           Antagonists: ${antagonists.map(tpl => (fragSrcPretty(tpl._1), fragSrcPretty(tpl._2))).mkString(", ")}
           NewFragToOrigFrag: $newFragToOrigFrag
           Target alternatives:\n\t${allTargetAlts.map(toShortNames(c)(_)).mkString("\n\t")}
         """)

    } else {

//      c.info(c.enclosingPosition, s"AltMapping $altMap", true)

      (resultTree(), altMap)

    }

  }

  private def toShortNames(c: whitebox.Context)(types: List[c.Type]): List[String] = {
    import c.universe._
    types.map(toShortName(c)(_))
  }

  private def toShortName(c: whitebox.Context)(tpe: c.Type): String = {
    import c.universe._
    tpe.typeSymbol.fullName.split("\\.").last
  }

  private def alternativesIterator(c: whitebox.Context)(compositeModelTpe: c.Type, checkDeps: Boolean, excludePlaceholders: Boolean, conformanceLevel: ConformanceLevel):
  (MorphModelNode, Map[Int, (c.Type, Option[c.universe.Type])], ResettableIterator[(List[FragmentNode], List[c.Type], c.Type)]) = {

    val (_, modelRoot, _, _, _, fragmentTypesMap) = buildModel(c)(compositeModelTpe, None, conformanceLevel)
    val altIter = alternativesIterator_(c)(modelRoot, fragmentTypesMap, excludePlaceholders)

    if (checkDeps) {
      checkDependenciesInCompositeType(c)(compositeModelTpe, conformanceLevel)
      altIter.reset()
    }

    (modelRoot, fragmentTypesMap, altIter)
  }


  private def alternativesIterator_(c: whitebox.Context)(modelRoot: MorphModelNode,
                                                         fragmentTypesMap: Map[Int, (c.Type, Option[c.universe.Type])], excludePlaceholders: Boolean):
  ResettableIterator[(List[FragmentNode], List[c.Type], c.Type)] = {

    import c.universe._

    def fragmentType(fn: FragmentNode): c.Type = {
      val fragTpe = fragmentTypesMap(fn.id)._1
      fragTpe
    }

    // Scan all alternatives and try to find one matching the fragment type
    val rootAltNode = modelRoot.toAltNode
    //val coupledCounters = new CoupledCounters(rootAltNode.collectCounters)

    new AltIterator[FragmentNode, (List[FragmentNode], List[c.Type], c.Type)](rootAltNode) {
      override protected def mapAlt(alt: List[FragmentNode]): (List[FragmentNode], List[c.Type], c.Type) = {
        val tpeAlt: List[c.Type] = alt.filter(!excludePlaceholders || !_.placeholder).map(fragmentType(_))
        val conj = conjunctionLUB(c)(tpeAlt)
        (alt, decomposeType(c)(conj), conj)
      }
    }

  }

  //def mirror_impl[S: c.WeakTypeTag](c: whitebox.Context)(self: c.Expr[Any]): c.Expr[Option[S with MorphMirror[\?[S]]]] = {
  def mirror_impl[S: c.WeakTypeTag](c: whitebox.Context)(self: c.Expr[Any]): c.Expr[Any] = {
    import c.universe._

    self.tree match {
      case This(_) =>
      // OK
      case _ =>
        c.abort(c.enclosingPosition, s"The argument in mirror must be 'this'. Found $self.")
    }

    val tp = self.actualType

    val conformanceLevelMarker = implicitly[WeakTypeTag[ConformanceLevelMarker]]

    val mirrorTpe = tq"$tp with MorphMirror[or[$tp, Unit]] with $conformanceLevelMarker"

    val res =
      q"""
          {
            import org.morpheus._
            import org.morpheus.Morpheus._
            $self match {
              case mirror: org.morpheus.MorphMirror[_] =>
                Some(mirror.asInstanceOf[$mirrorTpe]).asInstanceOf[Option[$mirrorTpe]]
              case _ => None
            }
          }
      """

    //val res = q"if (true) None else Some(null.asInstanceOf[$fragmentTpe])"

    //c.Expr[Option[S with MorphMirror[\?[S]]]](res)
    c.Expr(res)

  }

  def select_impl[F: c.WeakTypeTag](c: whitebox.Context)(mutableProxy: c.Expr[Any]): c.Expr[Option[F]] = {
    import c.universe._

    mutableProxy.tree match {
      case This(_) =>
        select_onThis[F](c)(mutableProxy)
      case _ =>
        select_onMirror[F](c)(mutableProxy)
    }

  }

  private def select_onMirror[F: c.WeakTypeTag](c: whitebox.Context)(proxy: c.Expr[Any]): c.Expr[Option[F]] = {
    import c.universe._

    val fragmentTpe = implicitly[WeakTypeTag[F]].tpe

    val immutableMirrorTpe = implicitly[TypeTag[MorphMirror[_]]].tpe
    val mutableMirrorTpe = implicitly[TypeTag[MutableMorphMirror[_]]].tpe

    if (!(proxy.actualType <:< immutableMirrorTpe)) {
      c.abort(c.enclosingPosition, s"Illegal argument type ${proxy.actualType} (${showRaw(proxy.tree)}}). Expected instance of $immutableMirrorTpe")
    }

    val deleg = if (proxy.actualType <:< mutableMirrorTpe) {
      // If the proxy argument is a mutable proxy use the delegate member for the test
      q"$proxy.delegate"
    } else {
      // Use the argument itself if it is an immutable proxy
      q"$proxy"
    }

    val compositeModelTpe = proxy.actualType.dealias match {
      case RefinedType(parents, _) =>
        parents.find(p => p <:< immutableMirrorTpe) match {
          case None =>
            c.abort(c.enclosingPosition, s"Illegal argument type. Expected instance of $immutableMirrorTpe")
          case Some(actualMirror) =>
            actualMirror.typeArgs match {
              case compModelTpe :: Nil =>
                compModelTpe
              case _ =>
                c.abort(c.enclosingPosition, s"Unknown composite mirror: $actualMirror")
            }
        }
      case _ =>
        c.abort(c.enclosingPosition, s"Unsupported argument type ${proxy.actualType}, type class: ${proxy.actualType.getClass}")
    }

    //val altIter = alternativesIterator(c)(compositeModelTpe, checkDeps = false, excludePlaceholders = true, Partial)._3
    val expandedAlts = expandAlternatives(c)(compositeModelTpe)._2
    val matches = expandedAlts.exists(_._3 <:< fragmentTpe)
//    var matches: Boolean = false
//    while (altIter.hasNext && !matches) {
//      val altLUB = altIter.next()._3
//      matches = altLUB <:< fragmentTpe
//    }

    val res = if (matches) {
      q"""
          {
            val deleg = $deleg
            deleg match {
              case f: $fragmentTpe => Some(deleg.asInstanceOf[$fragmentTpe]).asInstanceOf[Option[$fragmentTpe]]
              case _ => None
            }
          }
      """
    } else {
      c.abort(c.enclosingPosition, s"Composite type $compositeModelTpe is incompatible with the requested type $fragmentTpe")
    }

    c.Expr[Option[F]](res)
  }

  private def select_onThis[F: c.WeakTypeTag](c: whitebox.Context)(self: c.Expr[Any]): c.Expr[Option[F]] = {
    import c.universe._

    val fragmentTpe = implicitly[WeakTypeTag[F]].tpe

    // todo: check if the self is a fragment

    //val mutableMirrorTpe = implicitly[TypeTag[MutableMorphMirror[_, _]]].tpe

    // Use the type of 'this' as the composite type. It allows checking whether the F argument is valid since
    // F can refer only to the fragment type itself or its dependencies.
    val compositeModelTpe = self.actualType

    val altIter = alternativesIterator(c)(compositeModelTpe, checkDeps = false, excludePlaceholders = true, Partial)._3
    var matches: Boolean = false
    while (altIter.hasNext && !matches) {
      val altLUB = altIter.next()._3
      matches = altLUB <:< fragmentTpe
    }

    val res = if (matches) {
      //q"Some($mutableProxy.delegate.asInstanceOf[$fragmentTpe]).asInstanceOf[Option[$fragmentTpe]]"
      q"""
          {
            val morphOpt = $self match {
              case mirror: org.morpheus.MorphMirror[_] =>
                mirror.owningMutableProxy match {
                  case None => Some($self)
                  case Some(proxy) => Some(proxy.delegate)
                }
              case _ => None
            }

            morphOpt match {
              case None => None
              case Some(morph) => morph match {
                case f: $fragmentTpe => Some(f)
                case _ => None
              }
            }
          }
      """
    } else {
      c.abort(c.enclosingPosition, s"Composite type $compositeModelTpe is incompatible with the requested type $fragmentTpe")
    }
    //val res = q"if (true) None else Some(null.asInstanceOf[$fragmentTpe])"

    c.Expr[Option[F]](res)
  }

  def inspect_impl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: whitebox.Context)(mutableProxy: c.Expr[T])(fork: c.Expr[PartialFunction[Any, Any]]): c.Expr[Any] = {
    import c.universe._

    val argTpe = implicitly[WeakTypeTag[T]].tpe

    val res = q"""
        {
          $fork.apply($mutableProxy.delegate)
        }
     """

    c.Expr(res)
  }


  def decomposeType(c: whitebox.Context)(compositeTpe: c.Type): List[c.Type] = {
    import c.universe._

    val anyRefTpe = c.universe.rootMirror.typeOf[AnyRef]
    val anyTpe = c.universe.rootMirror.typeOf[Any]

    def decompose(tpe: c.Type): List[c.Type] = {
      tpe match {
        case RefinedType(parents, _) => parents.flatMap(decompose(_))
        case _ => List(tpe)
      }
    }

    val partTypes = decompose(compositeTpe)
    partTypes.filter(pt => pt != anyRefTpe && pt != anyTpe)
  }

  def conjunctionLUB(c: whitebox.Context)(partTypes: List[c.Type]): c.Type = {
    import c.universe._

    val anyTpe = c.universe.rootMirror.typeOf[Any]
    if (partTypes.isEmpty) {
      anyTpe
    } else {
      val anyRefTpe = c.universe.rootMirror.typeOf[AnyRef]
      val partTypesNoAnyRef = partTypes.filter(_ != anyRefTpe)
      if (partTypesNoAnyRef.isEmpty) {
        anyTpe
      } else {
        val conjTree = partTypesNoAnyRef.tail.foldLeft(tq"${partTypesNoAnyRef.head}")((tr, partTpe) => {
          tq"$tr with $partTpe"
        })
        val lubTpe = c.typecheck(conjTree, silent = true).tpe
        lubTpe
      }
    }
  }

  def disjunctionLUB(c: whitebox.Context)(partTypes: List[c.Type]): c.Type = {
    import c.universe._

    val unitTpe = c.universe.rootMirror.typeOf[AnyRef]

    val lubArgListTrees: List[Tree] = partTypes.map(partTpe => {
      q"identity[$partTpe](null)"
    })
    var lubTpe = c.typecheck(q"org.morpheus.LUBHelper.lub(..$lubArgListTrees)", silent = true).tpe
    if ("java.lang.Object" == lubTpe.typeSymbol.fullName) {
      // Replace AnyRef for Unit since Unit type plays the role of the unit element
      lubTpe = unitTpe
    }

    lubTpe
  }

  def getAnnotations(c: whitebox.Context)(tpe: c.Type): List[c.universe.Annotation] = {
    import c.universe._

    tpe.typeSymbol.annotations
  }

  def getAnnotation[A: WeakTypeTag](c: whitebox.Context)(tpe: c.Type) = {
    import c.universe._

    val annotTag = implicitly[WeakTypeTag[A]]
    tpe.typeSymbol.annotations.find(ann => {
      ann.tree.tpe =:= annotTag.tpe
    })
  }

  def getTypeAnnotations(c: whitebox.Context)(tpe: c.Type): List[c.universe.Annotation] = {
    import c.universe._

    tpe match {
      case aTpe: AnnotatedType =>
        aTpe.annotations
      case _ => Nil
    }

  }

  def getTypeAnnotation[A: WeakTypeTag](c: whitebox.Context)(tpe: c.Type) = {
    import c.universe._

    tpe match {
      case aTpe: AnnotatedType =>
        val annotTag = implicitly[WeakTypeTag[A]]
        aTpe.annotations.find(ann => {
          ann.tree.tpe =:= annotTag.tpe
        })
      case _ => None
    }
  }

  def isDimension(c: whitebox.Context)(tpe: c.Type): Boolean = {
    import c.universe._

    getAnnotation[dimension](c)(tpe).isDefined && !getAnnotation[wrapper](c)(tpe).isDefined
  }

  def findDimension(c: whitebox.Context)(tpe: c.Type): Option[c.Type] = {
    import c.universe._

    if (getTypeAnnotation[dimension](c)(tpe).isDefined) {
      // the support for the annotated placeholder types
      Some(tpe.asInstanceOf[AnnotatedType].underlying)
    } else {
      val bcIter = tpe.baseClasses.tail.toIterator // exclude the head which is the type itself
      var foundDim: Option[c.Type] = None
      while (bcIter.hasNext && !foundDim.isDefined) {
        val base = bcIter.next()
        val baseTpe = base.asType.toType
        if (isDimension(c)(baseTpe)) {
          foundDim = Some(baseTpe)
        } else {
          foundDim = findDimension(c)(baseTpe)
        }
      }

      foundDim
    }

  }

  def findFragment(c: whitebox.Context)(tpe: c.Type): Option[c.Type] = {
    import c.universe._

    if (isFragment(c)(tpe)) {
      Some(tpe)
    } else {
      val bcIter = tpe.baseClasses.tail.toIterator
      var foundFrag: Option[c.Type] = None
      while (bcIter.hasNext && !foundFrag.isDefined) {
        val base = bcIter.next()
        val baseTpe = base.asType.toType
        if (isFragment(c)(baseTpe)) {
          foundFrag = Some(baseTpe)
        } else {
          foundFrag = findFragment(c)(baseTpe)
        }
      }
      foundFrag
    }

  }

  def isDimensionWrapper(c: whitebox.Context)(tpe: c.Type): Boolean = {
    import c.universe._

    (getAnnotation[dimension](c)(tpe).isDefined || getTypeAnnotation[dimension](c)(tpe).isDefined) &&
      (getAnnotation[wrapper](c)(tpe).isDefined || getTypeAnnotation[wrapper](c)(tpe).isDefined)
  }

  def isFragmentWrapper(c: whitebox.Context)(tpe: c.Type): Boolean = {
    import c.universe._

    (getAnnotation[fragment](c)(tpe).isDefined || getTypeAnnotation[fragment](c)(tpe).isDefined) &&
      (getAnnotation[wrapper](c)(tpe).isDefined || getTypeAnnotation[wrapper](c)(tpe).isDefined)
  }

  def isWrapper(c: whitebox.Context)(tpe: c.Type): Boolean = {
    import c.universe._

    getAnnotation[wrapper](c)(tpe).isDefined || getTypeAnnotation[wrapper](c)(tpe).isDefined
  }

  def isFragment(c: whitebox.Context)(tpe: c.Type): Boolean = {
    import c.universe._

    (getAnnotation[fragment](c)(tpe).isDefined || getTypeAnnotation[fragment](c)(tpe).isDefined) &&
      !(getAnnotation[wrapper](c)(tpe).isDefined || getTypeAnnotation[wrapper](c)(tpe).isDefined)
  }

  def isAbstractFragment(c: whitebox.Context)(tpe: c.Type): Boolean = {
    import c.universe._

    val bareTpe: c.Type = tpe match {
      case aTpe: AnnotatedType => aTpe.underlying
      case _ => tpe
    }

    bareTpe.typeSymbol.isAbstract && !isFragment(c)(bareTpe) && !isWrapper(c)(bareTpe)
  }

  def getDependencyType(c: whitebox.Context)(fragType: c.Type, excludeHead: Boolean): Option[c.Type] = {
    import c.universe._

    val fragSelfTpe = fragType.typeSymbol.asClass.selfType
    if (excludeHead)
      fragSelfTpe match {
        case RefinedType(parents, scope) =>
          Some(internal.refinedType(parents.tail, scope))
        case _ => None
      }
    else
      Some(fragSelfTpe)
  }

  def checkDependenciesInCompositeType(c: whitebox.Context)(compTpe: c.Type, conformanceLevel: ConformanceLevel): Map[Int, c.Expr[String]] = {
    import c.universe._

    def checkUniqueFragmentTypes(altLub: c.Type, fragTypes: List[c.Type]): Unit = {
      if (fragTypes.size > 1) {
        val headTpe = fragTypes.head
        if (fragTypes.tail.contains(headTpe)) {
          c.abort(c.enclosingPosition, s"Fragment type $headTpe occurs more than once in alternative $altLub")
        } else {
          checkUniqueFragmentTypes(altLub, fragTypes.tail)
        }
      }
    }

    val (compModelTpe, modelRoot, fragmentNodes, lub, lubComponentTypes, fragmentTypesMap) = buildModel(c)(compTpe, None, conformanceLevel)

    var fragToDepsMaps = Map.empty[Int, c.Expr[String]]

    for (fragNode <- fragmentNodes;
         fragTpe = fragmentTypesMap(fragNode.id)._1) {

      try {
        val depsTpe = fragTpe.typeSymbol.asClass.selfType
        val optDepsTpe = c.typecheck(tq"org.morpheus.Morpheus.or[Unit, $depsTpe]", mode = c.TYPEmode).tpe

        val refConfLevel: ConformanceLevel = getConfLevelFromAnnotation(c)(fragTpe)
        val depsMapsAsStr = checkMorphKernelAssignment(c, s"Checking fragment $fragTpe dependencies")(compTpe, optDepsTpe, checkDepsInSrcModel = false, conformanceLevel,
          refConfLevel, containsHiddenFragments = false /* irrelevant, no placeholders in the self-type */, noHiddenFragments = false)._2.serialize

        fragToDepsMaps += (fragNode.id -> c.Expr[String](q"$depsMapsAsStr"))

      } catch {
        case dchk: DependencyCheckException =>
          c.abort(c.enclosingPosition, dchk.getMessage)
      }

    }

    // Check whether for each wrapper there exists a wrappable fragment in all alternatives where the wrapper occurs

    val altIter = alternativesIterator(c)(compTpe, false, false, Partial)._3
    while (altIter.hasNext) {
      val alt = altIter.next()

      checkUniqueFragmentTypes(alt._3, alt._2)

      for (wrapperTpe <- alt._2 if isWrapper(c)(wrapperTpe)) {
        val wrappedFragment = if (isDimensionWrapper(c)(wrapperTpe)) {
          // a dimension wrapper
          findDimension(c)(wrapperTpe)
        } else {
          // a fragment wrapper
          findFragment(c)(wrapperTpe)
        }

        require(wrappedFragment.isDefined, s"No fragment found for wrapper $wrapperTpe")

        if (!alt._2.exists(fragTpe => {
          // We try to verify that F <:< W <:< A[X], where F is the fragment in the alternative, W is the wrapped fragment
          // and A[X] is a generic parent of the two. The test for the ExistentialType is a kind of a hack attempting to
          // detect the situation when F and W have the same generic parent (erased) A but with different type argument X. In such
          // a case the lowest upper bound of F and W is an existential type (i.e. a bounded type).
          fragTpe.erasure <:< wrappedFragment.get.erasure &&
            !c.universe.lub(List(fragTpe, wrapperTpe)).isInstanceOf[ExistentialType]
//          if (isFragment(c)(fragTpe)) {
//            fragTpe.erasure <:< wrappedFragment.get.erasure &&
//              !c.universe.lub(List(fragTpe, wrapperTpe)).isInstanceOf[ExistentialType]
//          } else if (isAbstractFragment(c)(fragTpe)) {
//
//          } else {
//            false
//          }

        })) {
          c.abort(c.enclosingPosition, s"No fragment implementing ${wrappedFragment.get} found for wrapper $wrapperTpe in alternative ${alt._2}")
        }

      }
    }


    fragToDepsMaps


  }

//  def checkDependenciesInAlternatives(c: whitebox.Context)(altIter: Iterator[(List[FragmentNode], List[c.Type], c.Type)], conformanceLevel: ConformanceLevel): Unit = {
//    import c.universe._
//
//    val alts = altIter.toList
//
//    for (alt <- alts; fragTpe <- alt._2) {
//      try {
//        val altModelTpe = alt._3
//        val depsTpe = fragTpe.typeSymbol.asClass.selfType
//
//        val refConfLevel: ConformanceLevel = getConfLevelFromAnnotation(c)(fragTpe)
//        checkMorphKernelAssignment(c)(altModelTpe, depsTpe, checkDepsInSrcModel = false, conformanceLevel,
//          refConfLevel, containsHiddenFragments = false /* irrelevant, no placeholders in the self-type */)
//
//      } catch {
//        case dchk: DependencyCheckException =>
//          c.abort(c.enclosingPosition, dchk.getMessage)
//      }
//    }

//
//      // The fragment's self-type represents its dependencies.
//      // The self-type can be seen as a composite reference, which is to be assigned by an instance
//      // of the composite model under construction.
//      // The dependency check is equivalent to check the reference assignment where the source model is
//      // the composite model under construction and the target model is the self-type of the fragment.
//
//      try {
//        getDependencyType(c)(fragTpe, excludeHead = true) match {
//          case Some(depsTpe) =>
//            val refConfLevel: ConformanceLevel = getConfLevelFromAnnotation(c)(fragTpe)
//            Some(checkMorphKernelAssignment(c)(compModelTp, depsTpe, checkDepsInSrcModel = false, conformanceLevel,
//              refConfLevel, containsHiddenFragments = false /* irrelevant, no placeholders in the self-type */))
//          case None => None
//        }
//      }
//      //val depsTpe = fragTpe.typeSymbol.asClass.selfType
//      catch {
//        case dchk: DependencyCheckException =>
//          c.abort(c.enclosingPosition, dchk.getMessage)
//      }
//
//
//  }

  def buildModel(c: whitebox.Context)(compRawTp: c.Type, placeholderTpeTransf: Option[PartialFunction[c.Type, c.Type]], conformanceLevel: ConformanceLevel):
  (c.Type, MorphModelNode, List[FragmentNode], c.Type, List[c.Type], Map[Int, (c.Type, Option[c.universe.Type])]) = try {

    import c.universe._

    def normalizeModelType(tpe: Type): Type = {
      tpe match {
        case RefinedType(parents, decls) => internal.refinedType(parents.map(t => normalizeModelType(t)), decls)
        case t: Type => t
      }
    }

    val compModelTp: Type = normalizeModelType(compRawTp.dealias)

    type FragDesc = (Type, Option[Type])

    val anyRefTpe = c.universe.rootMirror.typeOf[AnyRef]

    val fragCounter: AtomicInteger = new AtomicInteger()
    var fragmentTypes: Map[Int, FragDesc] = Map.empty

    // DisjNode extractor
    object disjNodeType {
      val disjTp = implicitly[WeakTypeTag[or[_, _]]].tpe.typeConstructor

      def unapply(tp: Type): Option[(Type, Type)] = if (tp.typeConstructor =:= disjTp) {
        tp match {
          case tpRef: TypeRef =>
            Some((tpRef.args.head, tpRef.args(1)))
          case _ =>
            c.abort(c.enclosingPosition, s"Unsupported type in disjunction: $tp:${tp.getClass}")
        }
      } else None
    }

    object placeHolderNodeType {
      val plHldTp = implicitly[WeakTypeTag[$[_]]].tpe.typeConstructor

      def unapply(tp: Type): Option[Type] = if (tp.typeConstructor =:= plHldTp) {
        val tpRef: TypeRef = tp.asInstanceOf[TypeRef]
        Some(tpRef.args.head)
      } else None
    }

    // UnitNode extractor
    object unitNodeType {
      val unitTp = implicitly[WeakTypeTag[Unit]].tpe

      def unapply(tp: Type): Option[UnitNode.type] = if (tp.typeConstructor =:= unitTp) {
        Some(UnitNode)
      } else None
    }

    def checkEntity(node: FragmentNode, previousFragments: List[FragmentNode]) {
      val (fragTpe, cfgTpeOpt) = fragmentTypes(node.id)
      if (!fragTpe.decls.exists(sym => {
        sym.name == TermName("<init>") && sym.asMethod.paramLists.head.isEmpty
      })) {
        c.error(c.enclosingPosition, s"Entity $fragTpe missing default constructor")
      }
    }

    def verifyModel(node: MorphModelNode): (MorphModelNode, Type, List[Type]) = {
      val (lubTpe, lubTpeComponents) = determineLUB(node)
      //c.echo(c.enclosingPosition, s"LUB: $lubTpe")

      def checkFragments(node: MorphModelNode, checkedFragments: List[FragmentNode]): List[FragmentNode] = {

        def checkChildren(children: List[MorphModelNode]) = {
          var checked = checkedFragments
          for (ch <- children)
            checked = checkFragments(ch, checked)
          checked
        }

        node match {
          case ConjNode(children) =>
            checkChildren(children)
          case DisjNode(children) =>
            checkChildren(children)
          case UnitNode => checkedFragments
          case fn@FragmentNode(_, _) =>
            val (fragTpe, cfgTpeOpt) = fragmentTypes(fn.id)

            if (fn.id == 0 && !fragTpe.typeSymbol.isAbstract) {
              // fragment with id=0 may be an entity, i.e. a concrete class with the default constructor
              checkEntity(fn, checkedFragments)
            }

            fn :: checkedFragments
        }
      }

      checkFragments(node, Nil)

      (node, lubTpe, lubTpeComponents)
    }

    def createFragmentNode(frgTpe: Type, isPlaceholder: Boolean): FragmentNode = {
      val fragId: Int = fragCounter.getAndIncrement
      fragmentTypes += (fragId -> fragDesc(frgTpe, fragId))

//      if (frgTpe.isInstanceOf[AnnotatedType]) {
//        c.info(c.enclosingPosition, s"Found annotated type: $frgTpe", true)
//      }

      FragmentNode(fragId, isPlaceholder)
    }

    def traverseCompTp(tp: Type): MorphModelNode = tp.dealias match {
      case RefinedType(parents, _) =>
        val fragNodes = for (p <- parents) yield traverseCompTp(p)
        ConjNode(fragNodes)
      case disjNodeType(lhs, rhs) =>
        DisjNode(List(traverseCompTp(lhs), traverseCompTp(rhs)))
      case unitNodeType(u) => u
      case plHld@placeHolderNodeType(plHldTpe) =>
        val actPlHldTpe = placeholderTpeTransf match {
          case None => plHldTpe
          case Some(phTpeTrans) => if (phTpeTrans.isDefinedAt(plHldTpe)) phTpeTrans(plHldTpe) else plHldTpe
        }

        createFragmentNode(actPlHldTpe, true)
      case tr@TypeRef(_, sym, _) =>

        val symCls: Symbols#ClassSymbol = sym.asClass.asInstanceOf[Symbols#ClassSymbol]
        if (symCls.isRefinementClass) {
          val parents = symCls.tpe.parents // todo: is there any better way to decompose a type reference?
          val fragNodes = for (p <- parents) yield traverseCompTp(p.asInstanceOf[c.Type])
          ConjNode(fragNodes)
        } else {
          createFragmentNode(tr, false)
        }
      case atp: AnnotatedType =>
        createFragmentNode(atp, false)
      case t => sys.error(s"Unexpected type: $t: ${t.getClass}")
    }

    def fragDesc(fragTpe: Type, fragId: Int): FragDesc = {

      val fragTraitName: String = fragTpe.typeSymbol.fullName
      //val fragClassTpName: String = if (isDimension(c)(fragTpe) || (fragId == 0 && !fragTpe.typeSymbol.isAbstract)) {
      val fragClassTpName: String = if (isAbstractFragment(c)(fragTpe) || (fragId == 0 && !fragTpe.typeSymbol.isAbstract)) {
        // TODO: verify this condition! Can be there an entity with dependencies or configuration?
        // composite entity fragment
        fragTraitName
      } else {
        s"$fragTraitName$$fragment"
      }
      val fragClassTp = c.mirror.staticClass(fragClassTpName)

      val cfgClsOpt: Option[Type] = extractConfigType(c)(fragClassTp)

      (fragTpe, cfgClsOpt)
    }

    def fragmentType(fn: FragmentNode): c.Type = {
      fragmentTypes(fn.id)._1
    }

    def determineLUB(node: MorphModelNode): (Type, List[Type]) = {

      def determineLUB_(node: MorphModelNode): c.Type = node match {
        case ConjNode(children) =>
          conjunctionLUB(c)(children.map(c => determineLUB_(c)))
        case DisjNode(children) =>
          if (children.contains(Unit)) {
            anyRefTpe
          } else {
            disjunctionLUB(c)(children.map(determineLUB_(_)))
          }
        case fn@FragmentNode(_, _) =>
          val fTpe = fragmentType(fn)
          fTpe
        case UnitNode =>
          anyRefTpe
      }

      val lub = determineLUB_(node)
      val lubComponents = decomposeType(c)(lub)

      //c.info(c.enclosingPosition, s"LUB $lub, components: $lubComponents", true)

      if (lub =:= anyRefTpe) {
        //println(s"Degenerated LUB")
        (anyRefTpe, List(anyRefTpe))
      } else {
        (lub, lubComponents)
      }
    }

    def collectFragmentNodes(node: MorphModelNode): List[FragmentNode] = node match {
      case ConjNode(children) =>
        children.flatMap(ch => collectFragmentNodes(ch))
      case DisjNode(children) =>
        children.flatMap(ch => collectFragmentNodes(ch))
      case fn@FragmentNode(_, _) => List(fn)
      case UnitNode => Nil
    }

    val flattenedModel = traverseCompTp(compModelTp.asInstanceOf[c.Type]).flatten
    val (modelRoot, modelLUB, modelLUBComponents): (MorphModelNode, c.Type, List[c.Type]) =
      verifyModel(flattenedModel)

    val fragmentNodes = collectFragmentNodes(modelRoot).reverse

    val res = (compModelTp, modelRoot, fragmentNodes, modelLUB, modelLUBComponents, fragmentTypes)

    //c.info(c.enclosingPosition, s"Fragment types for model $compRawTp:\n $fragmentTypes", true)

    res
  } catch {
    case e: Exception =>
      throw new Exception(s"Cannot parse morph type $compRawTp", e)
  }

  private def getConfLevelFromAnnotation(c: whitebox.Context)(fragTpe: c.Type): ConformanceLevel = {
    import c.universe._

    // todo: improve the annotation handling

    getAnnotation[fragment](c)(fragTpe) match {
      case None => Partial

      case Some(fragAnnot) =>

        fragAnnot.tree match {
          case Apply(_, List(Literal(Constant(confLevelAttr)))) =>
            confLevelAttr match {
              case ConformanceLevel(confLevel) => confLevel
              case _ =>
                c.abort(c.enclosingPosition, s"Invalid value in fragment annotation: $confLevelAttr")
            }
          case _ => Partial
        }
    }
  }

  def parse_impl[M: c.WeakTypeTag](c: whitebox.Context)(checkDeps: c.Expr[Boolean]): c.Expr[MorphModel[M]] = {
    import c.universe._

    val Literal(Constant(check)) = checkDeps.tree
    val compTpe = implicitly[WeakTypeTag[M]].tpe
    parseMorphModelFromType(c)(compTpe, check.asInstanceOf[Boolean], removePlaceholders = false, None, Total)._1.asInstanceOf[c.Expr[MorphModel[M]]]
  }

  def parseWithRemovePlaceholders_impl[M: c.WeakTypeTag](c: whitebox.Context)(checkDeps: c.Expr[Boolean], removePlaceholders: c.Expr[Boolean]): c.Expr[Any] = {
    import c.universe._

    val Literal(Constant(check)) = checkDeps.tree
    val Literal(Constant(removePlh)) = removePlaceholders.tree
    val compTpe = implicitly[WeakTypeTag[M]].tpe

    parseMorphModelFromType(c)(compTpe, check.asInstanceOf[Boolean], removePlh.asInstanceOf[Boolean], None, Total)._1.asInstanceOf[c.Expr[Any]]
  }

  def build_impl[M: c.WeakTypeTag](c: whitebox.Context)(compositeModel: c.Expr[MorphModel[M]], checkDeps: c.Expr[Boolean],
                                                        fragmentProvider: c.Expr[FragmentProvider],
                                                        defaultStrategy: c.Expr[MorphingStrategy[M]],
                                                        conformanceLevel: c.Expr[org.morpheus.Morpheus.ConformanceLevel]): c.Expr[Any] = {
    import c.universe._

    val Literal(Constant(check)) = checkDeps.tree
    val Select(sel0, TermName(providerIdent)) = fragmentProvider.tree

    val tp = implicitly[WeakTypeTag[M]]

    val provider = providerIdent match {
      case "FactoryProvider" => FactoryProvider
      case "SingletonProvider" => SingletonProvider
      //      case "ConfiguratorProvider" => ConfiguratorProvider
      case "InstanceProvider" => InstanceProvider
    }

    composeOrGlean_impl[M](c)(provider, checkDeps = check.asInstanceOf[Boolean], Some(compositeModel), Some(defaultStrategy), None, Some(conformanceLevel))
  }

  def compose_impl[M: c.WeakTypeTag](c: whitebox.Context): c.Expr[Any] = {
    composeOrGlean_impl[M](c)(FactoryProvider, checkDeps = true, None, None, None, None)
  }

  def composeWithModel_impl[M: c.WeakTypeTag](c: whitebox.Context)(compositeModel: c.Expr[MorphModel[M]], defaultStrategy: c.Expr[MorphingStrategy[M]]): c.Expr[Any] = {
    composeOrGlean_impl[M](c)(FactoryProvider, checkDeps = true, Some(compositeModel), Some(defaultStrategy), None, None)
  }

  def singleton_impl[M: c.WeakTypeTag](c: whitebox.Context): c.Expr[Any] = {
    composeOrGlean_impl[M](c)(SingletonProvider, checkDeps = true, None, None, None, None)
  }

  def singletonWithModel_impl[M: c.WeakTypeTag](c: whitebox.Context)(compositeModel: c.Expr[MorphModel[M]], defaultStrategy: c.Expr[MorphingStrategy[M]]): c.Expr[Any] = {
    composeOrGlean_impl[M](c)(SingletonProvider, checkDeps = true, Some(compositeModel), Some(defaultStrategy), None, None)
  }

  def glean_impl[M: c.WeakTypeTag](c: whitebox.Context): c.Expr[Any] = {
    composeOrGlean_impl[M](c)(InstanceProvider, checkDeps = true, None, None, None, None)
  }

  def gleanWithModel_impl[M: c.WeakTypeTag](c: whitebox.Context)(compositeModel: c.Expr[MorphModel[M]], defaultStrategy: c.Expr[MorphingStrategy[M]]): c.Expr[Any] = {
    composeOrGlean_impl[M](c)(InstanceProvider, checkDeps = true, Some(compositeModel), Some(defaultStrategy), None, None)
  }

  def composePartial_impl[M: c.WeakTypeTag](c: whitebox.Context): c.Expr[Any] = {
    composeOrGlean_impl[M](c)(FactoryProvider, checkDeps = false, None, None, None, None)
  }

  def singletonPartial_impl[M: c.WeakTypeTag](c: whitebox.Context): c.Expr[Any] = {
    composeOrGlean_impl[M](c)(SingletonProvider, checkDeps = false, None, None, None, None)
  }

  def gleanPartial_impl[M: c.WeakTypeTag](c: whitebox.Context): c.Expr[Any] = {
    composeOrGlean_impl[M](c)(InstanceProvider, checkDeps = false, None, None, None, None)
  }

//  def fork_impl[M1: c.WeakTypeTag, M2: c.WeakTypeTag](c: whitebox.Context)(ci1: c.Expr[MorphKernelBase[M1]], ci2: c.Expr[MorphKernelBase[M2]]): c.Expr[Any] = {
//    import c.universe._
//
//    val comp1Tpe = implicitly[WeakTypeTag[M1]].tpe.dealias
//    val comp2Tpe = implicitly[WeakTypeTag[M2]].tpe.dealias
//
//    if (comp1Tpe =:= comp2Tpe) {
//      createMorphKernel(c)(comp1Tpe, ForkProvider(ci1, ci2), false, None, None, None, None)
//    } else {
//      c.abort(c.enclosingPosition, s"Composite types do not match:\n$comp1Tpe\n!=\n$comp2Tpe")
//    }
//
//  }

  def transformToNoPlaceholders(c: whitebox.Context)(rootNode: MorphModelNode,
                                                     fragmentTypesMap: Map[Int, (c.Type, Option[c.Type])],
                                                     placeholderTpeTransf: Option[PartialFunction[c.Type, c.Type]]): c.Tree = {
    import c.universe._

    val unitTpe = implicitly[WeakTypeTag[Unit]].tpe

    def transformNode(node: MorphModelNode): Tree = {
      node match {
        case ConjNode(children) =>
          val headTpe = transformNode(children.head)
          val tpeTree = children.tail.foldLeft(tq"$headTpe")((res, ch) => {
            val childTpe = transformNode(ch)
            tq"$res with $childTpe"
          })
          tpeTree
        case DisjNode(children) =>
          val headTpe = transformNode(children.head)
          val tpeTree = children.tail.foldLeft(tq"$headTpe")((res, ch) => {
            val childTpe = transformNode(ch)
            tq"org.morpheus.Morpheus.or[$res, $childTpe]"
          })
          tpeTree
        case FragmentNode(id, plh) =>
          val fragDeclTpe = fragmentTypesMap(id)._1
          // try to transform the placeholder's declared type to the actual type by means of placeholderTpeTransf
          val fragTpe = if (!plh) {
            fragDeclTpe
          } else placeholderTpeTransf match {
            case None => fragDeclTpe
            case Some(phTpeTrans) => if (phTpeTrans.isDefinedAt(fragDeclTpe)) phTpeTrans(fragDeclTpe) else fragDeclTpe
          }
          tq"$fragTpe"
        case UnitNode =>
          tq"$unitTpe"
      }
    }

    transformNode(rootNode)
  }


  def parseMorphModelFromType(c: whitebox.Context)(compTpe: c.Type, checkDeps: Boolean, removePlaceholders: Boolean,
                                                       placeholderTpeTransf: Option[PartialFunction[c.Type, c.Type]],
                                                       conformanceLevel: ConformanceLevel): (c.Expr[MorphModel[_]], c.Type) = {
    import c.universe._


    // TODO: This is an ugly workaround.
    // TODO: The model must be parsed twice because of some bug in Scala compiler. Sometimes a type contains no annotations
    // TODO: even if there are some. It seems that such a type is not fully initialized.
    buildModel(c)(compTpe, None, conformanceLevel)

    val (compModelTpe, modelRoot, fragmentNodes, lub, lubComponentTypes, fragmentTypesMap) = buildModel(c)(compTpe, placeholderTpeTransf, conformanceLevel)

    val (appliedCompModelTpe, actualModelTpe) = if (removePlaceholders) {
      val transformedModel = transformToNoPlaceholders(c)(modelRoot, fragmentTypesMap.map(e => (e._1, (e._2._1, e._2._2))), placeholderTpeTransf)
      val transformedModelTpe = c.typecheck(transformedModel, mode = c.TYPEmode).tpe
      (tq"org.morpheus.MorphModel[$transformedModel]", transformedModelTpe)
    } else
      (tq"org.morpheus.MorphModel[$compTpe]", compTpe)

    val fragToDepsMaps: Map[Int, c.Expr[String]] = if (checkDeps) {
      //checkDependenciesInCompositeType(c)(compTpe, conformanceLevel)
      checkDependenciesInCompositeType(c)(actualModelTpe, conformanceLevel)
    } else {
      Map.empty
    }

    def convertModelToTree(node: MorphModelNode): Tree = node match {
      case ConjNode(children) =>
        val childrenTrees: List[Tree] = for (c <- children) yield convertModelToTree(c)
        q"""
            {
               org.morpheus.ConjNode(List(..$childrenTrees))
            }
        """
      case DisjNode(children) =>
        val childrenTrees: List[Tree] = for (c <- children) yield convertModelToTree(c)
        q"""
            {
               org.morpheus.DisjNode(List(..$childrenTrees))
            }
        """
      case UnitNode => q"org.morpheus.UnitNode"
      case FragmentNode(fragId, placeholder) =>
        q"org.morpheus.FragmentNode($fragId, $placeholder)"
    }

    def fragmentAltMapTree(fn: FragmentNode): Tree = {
      fragToDepsMaps.get(fn.id) match {
        case None => q"None"
        case Some(depsMaps) => q"Some($depsMaps)"
      }
    }

    def fragmentTree(fn: FragmentNode): Tree = {
      val (fragTpe, cfgClsOpt) = fragmentTypesMap(fn.id)

      cfgClsOpt match {
        case Some(cf) =>
          q"Frag[$fragTpe, $cf](${fn.id}, implicitly[reflect.runtime.universe.WeakTypeTag[$fragTpe]], implicitly[reflect.runtime.universe.WeakTypeTag[$cf]], am(${fn.id}))"
        case None =>
          q"Frag[$fragTpe, Unit](${fn.id}, implicitly[reflect.runtime.universe.WeakTypeTag[$fragTpe]], implicitly[reflect.runtime.universe.WeakTypeTag[Unit]], am(${fn.id}))"
      }
    }

    val hlistTree: Tree = fragmentNodes.foldLeft[Tree](q"HNil")((tree, fn) => {
      val fragId = Literal(Constant(fn.id))
      q"${fragmentTree(fn)}::$tree"
    })


    val fragAltMapTrees: List[Tree] = fragmentNodes.map(fragmentAltMapTree(_))
    val fragAltMapTreeList: Tree = q"List(..$fragAltMapTrees)"

    val fragDescs: List[Tree] = fragmentNodes.map(fragmentTree(_)).reverse
    val fragDescListTree: Tree = q"List(..$fragDescs)"

    val modelRootTree = convertModelToTree(modelRoot)
    val lubComponents: List[Tree] = for (lubComp <- lubComponentTypes) yield q"classOf[$lubComp]"

    //c.info(c.enclosingPosition, s"Actual morph type: $actualModelTpe", true)
    //val appliedCompModelTpe = c.typecheck(appliedCompModelTpeNoCheck, mode = c.TYPEmode)

    val confLevelTpe = conformanceLevel match {
      case Total => implicitly[WeakTypeTag[TotalConformance]].tpe
      case Partial => implicitly[WeakTypeTag[PartialConformance]].tpe
      //      case Exclusive => implicitly[WeakTypeTag[ExclusiveConformance]].tpe
    }

    val compositeModelTree = q"""
        {
          val am = $fragAltMapTreeList
          new $appliedCompModelTpe($modelRootTree) {
            import org.morpheus._
            import shapeless.{record, syntax, HList, Poly1, HNil}
            import record._
            import syntax.singleton._

            type LUB = $lub
            type ConformLevel = $confLevelTpe
            val lubComponents: Array[Class[_]] = Array(..$lubComponents)

            val fragmentDescriptors = $hlistTree
            val fragmentDescriptorsList = $fragDescListTree
          }
        }
      """

    //c.info(c.enclosingPosition, s"Composite model: ${show(compositeModelTree)}", true)
    (c.Expr[MorphModel[_]](compositeModelTree), actualModelTpe)
  }

  def composeOrGlean_impl[M: c.WeakTypeTag](c: whitebox.Context)(fragmentProvider: FragmentProvider, checkDeps: Boolean,
                                                                 compositeModelExprOpt: Option[c.Expr[MorphModel[M]]],
                                                                 defaultStrategyOpt: Option[c.Expr[MorphingStrategy[_]]],
                                                                 placeholderTpeTransf: Option[PartialFunction[c.Type, c.Type]],
                                                                 conformanceLevel: Option[c.Expr[org.morpheus.Morpheus.ConformanceLevel]]): c.Expr[Any] = {
    import c.universe._

    val compTpe = implicitly[WeakTypeTag[M]].tpe
    createMorphKernel(c)(compTpe, fragmentProvider, checkDeps, compositeModelExprOpt, defaultStrategyOpt, placeholderTpeTransf, conformanceLevel)
  }


  def createMorphKernel(c: whitebox.Context)(compTpe: c.Type, fragmentProvider: FragmentProvider, checkDeps: Boolean,
                                                   compositeModelExprOpt: Option[c.Expr[MorphModel[_]]],
                                                   defaultStrategyOpt: Option[c.Expr[MorphingStrategy[_]]],
                                                   placeholderTpeTransf: Option[PartialFunction[c.Type, c.Type]],
                                                   conformanceLevelExpr: Option[c.Expr[org.morpheus.Morpheus.ConformanceLevel]]): c.Expr[Any] = {
    import c.universe._

    // the conformance level of the composite instance under construction

    val (conformanceLevel, conformanceLevelMarker) = conformanceLevelExpr match {
      case None => fragmentProvider match {
        case CopyProvider(_, _, confLev, confLevTpe, _, _) =>
          (confLev, confLevTpe.asInstanceOf[c.Tree])
        case _ => (Total, tq"org.morpheus.TotalConformance")
      }

      case Some(confLevelExpr) =>
        val Select(_, TermName(confLevelName)) = confLevelExpr.tree
        val cl = confLevelName match {
          case "Total" => (Total, tq"org.morpheus.TotalConformance")
          case "Partial" => (Partial, tq"org.morpheus.PartialConformance")
          case _ => c.abort(c.enclosingPosition, s"Invalid conformance level $confLevelName")
        }
        cl
    }


    val (_, _, fragmentNodes, _, _, fragmentTypesMap) = buildModel(c)(compTpe, placeholderTpeTransf, conformanceLevel)

    def fragmentProxyImplicit(fn: FragmentNode): Tree = {
      val fragTpe = fragmentTypesMap(fn.id)._1
      val proxyImplValName = TermName(c.freshName("proxy"))
      q"implicit def $proxyImplValName = fragments.select[FragmentHolder[$fragTpe]].proxy"
    }

    def fragmentSymbol(fn: FragmentNode): Tree = {
      val fragTpe = fragmentTypesMap(fn.id)._1
      Literal(Constant(fragTpe.typeSymbol.fullName.split("\\.").last))
      //q"$sym"
      //q"Frag[$fragTpe, Unit](${fn.id}, implicitly[reflect.runtime.universe.WeakTypeTag[$fragTpe]], implicitly[reflect.runtime.universe.WeakTypeTag[Unit]])"
    }

    val defConstr = DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))

    val (compModelExpr, actualModelTpe) = compositeModelExprOpt match {
      case Some(modelExpr) =>
        (modelExpr, compTpe)
      case None =>
        parseMorphModelFromType(c)(compTpe, checkDeps, removePlaceholders = true, placeholderTpeTransf, conformanceLevel)
    }
    val compModelTree = compModelExpr.tree

    def defaultParentInstance = q"None"
    def noHiddenFragmentsMarker = tq"org.morpheus.WithoutHiddenFragments"
    def mayContainHiddenFragmentsMarker = tq"org.morpheus.WithHiddenFragments"
    def defaultRootNode = q"compositeModel.rootNode"
    def defaultFragmentList = q"fragments.toList.asInstanceOf[List[FragmentHolder[_]]]"
    def defaultDefaultStrategy = q"org.morpheus.DefaultCompositeStrategy[compositeModel.Model](compositeModel)"
    def defaultAltComposer = q"new org.morpheus.DefaultAlternativeComposer[compositeModel.Model]()"

    val (configImplicits, rootNode, fragmentList, strategy, altComposer, hiddenFragmentsMarker, parentInstance) = fragmentProvider match {
      case FactoryProvider =>
        (q"org.morpheus.FactoryProviderImplicits", defaultRootNode, defaultFragmentList, defaultDefaultStrategy, defaultAltComposer, noHiddenFragmentsMarker, defaultParentInstance)
      case SingletonProvider =>
        (q"org.morpheus.SingletonProviderImplicits", defaultRootNode, defaultFragmentList, defaultDefaultStrategy, defaultAltComposer, noHiddenFragmentsMarker, defaultParentInstance)
      case InstanceProvider =>
        (q"org.morpheus.InstanceProviderImplicits", defaultRootNode, defaultFragmentList, defaultDefaultStrategy, defaultAltComposer, noHiddenFragmentsMarker, defaultParentInstance)
      case CopyProvider(src, placeholderFacts, _, _, delegation, noHiddenFragments) if !delegation =>
        val srcTree = src.asInstanceOf[c.Expr[MorphKernelRef[_, _]]].tree
        val placeholderFactMapTree = placeholderFacts.asInstanceOf[Tree]
        val hiddenFragMarker = if (noHiddenFragments) noHiddenFragmentsMarker else mayContainHiddenFragmentsMarker
        (q"new org.morpheus.CopyProviderImplicits($srcTree.instance, $placeholderFactMapTree, $defaultRootNode)",
          defaultRootNode, defaultFragmentList,
          q"org.morpheus.BridgeStrategy($srcTree.asInstanceOf[org.morpheus.MorphKernelRef[compositeModel.Model, _]])",
          q"org.morpheus.BridgeAlternativeComposer($srcTree.asInstanceOf[org.morpheus.MorphKernelRef[compositeModel.Model, _]])",
          hiddenFragMarker, q"Some($srcTree.instance)")
      case CopyProvider(src, _, _, _, delegation, _) if delegation =>
        val srcTree = src.asInstanceOf[c.Expr[MorphKernelBase[_]]].tree
        (q"new org.morpheus.CopyProviderImplicits($srcTree, Map.empty, $defaultRootNode)",
          defaultRootNode, defaultFragmentList, defaultDefaultStrategy, defaultAltComposer, noHiddenFragmentsMarker, q"Some($srcTree)")
//      case ForkProvider(src1, src2) =>
//        val src1Tree = src1.asInstanceOf[c.Expr[MorphKernelBase[_]]].tree
//        val src2Tree = src2.asInstanceOf[c.Expr[MorphKernelBase[_]]].tree
//        (q"new org.morpheus.ForkProviderImplicits()", defaultRootNode, defaultFragmentList,
//          defaultDefaultStrategy,
//          q"org.morpheus.ForkAlternativeComposer($src1Tree.asInstanceOf[org.morpheus.MorphKernelBase[compositeModel.Model]], $src2Tree.asInstanceOf[org.morpheus.MorphKernelBase[compositeModel.Model]])",
//          mayContainHiddenFragmentsMarker, q"Some(new org.morpheus.ForkMorphKernelBase($src1Tree, $src2Tree))")
    }

    val defaultStrategy = defaultStrategyOpt match {
      case None => strategy
      case Some(defStrat) =>
        val requiredStrategyTpe = c.typecheck(tq"org.morpheus.MorphingStrategy[$actualModelTpe]", mode = c.TYPEmode).tpe
        if (!(defStrat.actualType <:< requiredStrategyTpe)) {
          c.abort(c.enclosingPosition, s"Default strategy type\n${defStrat.actualType}\n does not conform the required type\n$requiredStrategyTpe")
        }
        defStrat.tree
    }

    val proxyImplicitsTree: Tree = {
      val implVals: List[Tree] = fragmentNodes.map(fn => fragmentProxyImplicit(fn))
      ModuleDef(Modifiers(), TermName("proxyImplicits"), Template(List(Select(Ident(TermName("scala")), TypeName("AnyRef"))), noSelfType, defConstr :: implVals))
    }

    val compositeInstanceTree = q"""
        {
            val compositeModel = $compModelTree
            new org.morpheus.MorphKernel[compositeModel.Model]($rootNode) with $hiddenFragmentsMarker with $conformanceLevelMarker {
              import org.morpheus._
              import shapeless.{record, syntax, HList, Poly1, HNil}
              import record._
              import syntax.singleton._
              val configImplicitsProvider = $configImplicits
              import configImplicitsProvider._

              type LUB = compositeModel.LUB
              type ConformLevel = $conformanceLevelMarker
              val parent = $parentInstance
              val lubComponents: Array[Class[_]] = compositeModel.lubComponents

              val fragmentDescriptors = compositeModel.fragmentDescriptors

              val fragments = fragmentDescriptors.map(ConfigImplicits)

              $proxyImplicitsTree
              import proxyImplicits._

              val proxies = proxyImplicits

              val fragmentList = $fragmentList

              val defaultStrategy = $defaultStrategy
              val altComposer = $altComposer
              val model = compositeModel
            }
        }
    """

    //c.info(c.enclosingPosition, s"Composite instance: ${show(compositeInstanceTree)}", true)

    c.Expr(compositeInstanceTree)
  }

  def extractConfigType[F: c.WeakTypeTag](c: whitebox.Context)(fragmentClazz: c.universe.ClassSymbol): Option[c.Type] = {

    import scala.reflect.api.Universe

    val ext = new FragmentClassAnalyzer {
      override val univ: Universe = c.universe
    }

    ext.extractConfigType(fragmentClazz.asInstanceOf[ext.univ.ClassSymbol]).asInstanceOf[Option[c.Type]]
  }

}



