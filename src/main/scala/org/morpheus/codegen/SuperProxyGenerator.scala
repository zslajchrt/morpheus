package org.morpheus.codegen

/**
* Created by zslajchrt on 20/01/15.
*/

import scala.tools._
import scala.tools.nsc.Global
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.transform.{Transform, TypingTransformers}


/**
 * This class is Scala 2.11 version of the following class:
 * [[https://raw.githubusercontent.com/kevinwright/Autoproxy-Lite/master/plugin/src/main/scala/autoproxy/plugin/GenerateSynthetics.scala']]
 * developed by Kevin Wright.
 *
 * @param global
 */
class SuperProxyGenerator(val global: Global) extends PluginComponent
with Transform
with TypingTransformers
with TreeDSL
{
  import global._
  //import definitions._

  import global.Tree

  //val runsAfter = List[String]("generatesynthetics")
  val runsAfter = List[String]("typer")
  val phaseName = "autoproxy"

  def newTransformer(unit: CompilationUnit) = new SuperProxyTransformer(unit)

  class SuperProxyTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    private def cloneMethod(prototype: Symbol, owner: Symbol) = {
      val newSym = prototype.cloneSymbol(owner)
      newSym setFlag SYNTHETIC
      newSym resetFlag ABSTRACT
      newSym resetFlag DEFERRED
      if (prototype.isStable) newSym setFlag STABLE
      owner.info.decls enter newSym
    }

//    private def cloneMethod2(prototype: Symbol, owner: Symbol) = {
//      val methodName = prototype.name
//      val flags = SYNTHETIC | (if (prototype.isStable) STABLE else 0)
//      val method = owner.newMethod(NoPosition, methodName) setFlag flags
//      method setInfo prototype.info
//      owner.info.decls.enter(method).asInstanceOf[TermSymbol]
//    }

    private def mkDelegate(owner: Symbol, tgtMember: Symbol, tgtMethod: Symbol, pos: Position) = {
      val delegate = cloneMethod(tgtMethod, owner)
      //val delegate = cloneMethod2(tgtMethod, owner)
      delegate setPos tgtMember.pos.focus

      log("owner = " + This(owner))

      val tgtGetter: Symbol = if(tgtMember.hasGetter) tgtMember.getterIn(owner) else tgtMember
      log("target getter = " + tgtGetter)

      //val selectTarget = This(owner) DOT tgtGetter DOT tgtMethod
      val selectTarget = Select(Select(This(owner), tgtGetter), tgtMethod)
      //      log("SelectTarget = " + nodeToString(selectTarget))

      val rhs: Tree =
        delegate.info match {
          case MethodType(params, _) => Apply(selectTarget, params.map(Ident(_)))
          case _ => selectTarget
        }

      //global.analyzer.UnTyper.traverse(rhs)
      //      log("rhs=" + nodeToString(rhs))

      //val delegateDef = localTyper.typed {DEF(delegate) === rhs}
      val delegateDef = try {
        localTyper.typed {DefDef(delegate, rhs)}
      } catch {
        case t: Throwable =>
          sys.error(s"Cannot generate delegate for $tgtMember, $tgtMethod: $t")
          //throw t
      }
      //      log("delegate = " + nodeToString(delegateDef))

      delegateDef
    }

    private def publicMembersOf(sym: Symbol) =
      sym.tpe.members.filter(_.isPublic).filter(!_.isConstructor)

    private def publicMethodsOf(sym: Symbol) =
      publicMembersOf(sym).filter(_.isMethod)


    def generateDelegates(templ: Template, symbolToProxy: Symbol): List[Tree] = {
      def equiv(a: Symbol) = (b: Symbol) => {
        (a.nameString == b.nameString) && (a.tpe =:= b.tpe)
      }

      val cls = symbolToProxy.owner //the class owning the symbol

      log("proxying symbol: " + symbolToProxy)
      log("owning class: " + cls)

      //need to find methods that are ONLY inherited from the type being proxied.
      //if available through any other route, then don't create the delegate.

      //first, locate all concrete public methods inherited from a superclass other than the proxy source
      val parents = cls.info.parents filter {symbolToProxy.tpe != }
      val nonProxyBaseClasses = parents.flatMap(_.baseClasses).distinct
      val nonProxyInheritedMethods = nonProxyBaseClasses.flatMap(publicMethodsOf).distinct
      val inheritedExclusions = nonProxyInheritedMethods.filterNot(_.isIncompleteIn(cls))
      log("inherited exclusions: " + inheritedExclusions.mkString(", "))

      // now locate all methods on the receiving class, and separate those which are inherited
      val definedMethods = publicMethodsOf(cls)
      val inheritedMethods = definedMethods.filter(_.owner != cls)
      val localMethods = definedMethods.filter(_.owner == cls)
      val localMethodAncestors = localMethods.flatMap(_.allOverriddenSymbols)
      val symbols: Iterable[Symbol] = localMethods ++ localMethodAncestors
      val localExclusions = symbols.toList.distinct
      log("local exclusions: " + localExclusions.mkString(", "))

      //determine all methods that should be excluded from consideration for proxying
      val exclusions = (inheritedExclusions ++ localExclusions).distinct
      log("all exclusions: " + exclusions.mkString(", "))
      val nameTpeStrs = exclusions map (sym => sym.name.toString + ": " + sym.tpe.toString)
      log("all exclusions: " + nameTpeStrs.mkString(", "))

      //now locate all methods available via the proxy source, and remove exclusions
      val candidates = publicMembersOf(symbolToProxy)
      log("candidates: " + candidates.mkString(", "))
      val requiredMethods = candidates filterNot (c => exclusions exists equiv(c))
      log("required methods (candidates - exclusions): " + requiredMethods.mkString(", "))

      val needsOverride = requiredMethods.filterNot(_.isIncompleteIn(cls))
      log("needs override: " + needsOverride.mkString(", "))
      //val abstractMethods = definedMethods.filter(_.isIncompleteIn(cls))
      //val missingMethods =
      //  publicMembersOf(symbolToProxy).filter(mem => !definedMethods.contains(mem))



      val synthetics = requiredMethods map { mkDelegate(cls, symbolToProxy, _, symbolToProxy.pos.focus) }
      synthetics.toList
    }

    override def transform(tree: Tree): Tree = {
//      inform(s"settings.log.isSetByUser : ${settings.log.isSetByUser}")
//      inform(s"settings.log containsPhase globalPhase : ${settings.log containsPhase globalPhase}")
//      inform(s"settings.log containsPhase phase : ${settings.log containsPhase phase}")

      //    (
      //      (settings.log containsPhase globalPhase) || (settings.log containsPhase phase)

      def isAccessor(tree: Tree) = tree match {
        case m: ValDef => true
        case _ => false
      }

      def shouldAutoProxySym(sym: Symbol) = {
        //inform("testing symbol: " + sym)
        if (sym != null) {
          val testSym = if (sym.isModule) sym.moduleClass else sym
          testSym.annotations foreach { ann => log("annotation:" + ann) }
          val gotOne = testSym.annotations exists {_.toString startsWith "org.morpheus.autoproxy"}
          //if(gotOne) inform("got a live one here!")
          gotOne
        } else false
      }

      def shouldAutoProxy(tree: Tree) = {
        //isAccessor(tree) &&
        shouldAutoProxySym(tree.symbol)
      }

      tree match {
        case PackageDef(_, _) =>
          val p = super.transform(tree)
          //inform(s"Package after autoproxy: ${show(p)}")
          p
        case cd @ ClassDef(mods, name, tparams, impl) =>
          //inform(s"Autoproxy candidate: $name with members ${impl.body.map(m => {s"$m is annotated ${shouldAutoProxy(m)}"})}")
          val delegs = for (member <- impl.body if shouldAutoProxy(member)) yield {
            //inform("found annotated member: " + member)
            generateDelegates(impl, member.symbol)
          }

          if (delegs.nonEmpty) {
            val newImpl = treeCopy.Template(impl, impl.parents, impl.self, delegs.flatten ::: impl.body)
            val proxyCls = treeCopy.ClassDef(tree, mods &~ Flag.ABSTRACT, name, tparams, newImpl)

            super.transform(proxyCls)
          } else {
            super.transform(tree)
          }

        case _ => super.transform(tree)
      }

    }
  }
}
