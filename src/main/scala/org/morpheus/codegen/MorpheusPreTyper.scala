package org.morpheus.codegen

import scala.tools.nsc.Global
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.util.DynamicVariable

/**
 * This transformer generates the "cap class" (fragment) for a fragment trait. In the case the cap class has
 * the configuration constructor parameter, it is further transformed in [[SuperProxyGenerator]] to map
 * the configuration members onto the abstract fragment members.
 * *
 * Created by zslajchrt on 19/01/15.
 *
 */
class MorpheusPreTyper(val global: Global) extends PluginComponent
with Transform
with TreeDSL {

  import global._

  override val runsBefore: List[String] = List[String]("namer")
  val runsAfter = List[String]("parser")
  val phaseName = "morpheus-pre-typer"

  def newTransformer(unit: CompilationUnit) = new MorpheusPreTyperTransformer(unit)

  class MorpheusPreTyperTransformer(unit: CompilationUnit) extends Transformer {

    def isSymbolAnnotated(sym: Symbol, annotation: String) = {
      if (sym != null) {
        val testSym = if (sym.isModule) sym.moduleClass else sym
        testSym.annotations foreach { ann => log("annotation:" + ann)}
        val gotOne = testSym.annotations exists {
          _.toString startsWith annotation
        }
        if (gotOne) log("got a live one here!")
        gotOne
      } else false
    }

    def generateClass(newClassName: TypeName, clsMods: Modifiers, parents: List[Tree], body: List[Tree], selfType: ValDef, tparams : scala.List[TypeDef]): ClassDef = {
      val templ = Template(parents, selfType, body)
      ClassDef(clsMods, newClassName, tparams, templ)
    }

    class ContextPackage(val pkg: PackageDef) {
      var newClasses: List[ClassDef] = Nil
    }

    class ContextClass(val cls: ClassDef) {
    }

    class ContextAnnotation {
      var annotation: String = _
    }

    val contextPackage: DynamicVariable[ContextPackage] = new DynamicVariable[ContextPackage](null)
    val contextClass: DynamicVariable[ContextClass] = new DynamicVariable[ContextClass](null)
    val contextAnnotation: DynamicVariable[ContextAnnotation] = new DynamicVariable[ContextAnnotation](null)

    val autoProxyAnnot =
      Apply(Select(New(Select(Select(Ident(TermName("org")), TermName("morpheus")), TypeName("autoproxy"))),
        termNames.CONSTRUCTOR), List())

    override def transform(tree: Tree): Tree = {

      def generateDimClass() {
        val ctxCls: ClassDef = contextClass.value.cls
        val newClassName = TypeName(s"${ctxCls.name}$$dimension")

        val parentSelfType: ValDef = ctxCls.impl.self
        val newClsSelfType = treeCopy.ValDef(parentSelfType, parentSelfType.mods, parentSelfType.name, parentSelfType.tpt, parentSelfType.rhs)

        val superBaseGeneric = Select(Select(Ident(TermName("org")), TermName("morpheus")), TypeName("SuperBase"))
        val appliedDim = if (ctxCls.tparams.isEmpty)
          Ident(ctxCls.name)
        else
          AppliedTypeTree(Ident(ctxCls.name), ctxCls.tparams.map(tp => Ident(tp.name)))
        val superBaseForDim = AppliedTypeTree(superBaseGeneric, List(appliedDim))

        val superAccessorMods = Modifiers(Flag.SYNTHETIC | Flag.PRIVATE, typeNames.EMPTY, List(autoProxyAnnot))
        val superAccessor: DefDef = DefDef(superAccessorMods, TermName("_$_"), List(), List(), TypeTree(), Select(This(newClassName), TermName("$super$")))

        val classBody = List(
          superAccessor,
          DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(),
            Block(List(Apply(Select(Super(This(newClassName), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(()))))
        )


        val clsMods: Modifiers = Modifiers(Flag.SYNTHETIC)
        val newCls = generateClass(newClassName, clsMods,
          List(superBaseForDim, appliedDim), classBody, newClsSelfType, ctxCls.tparams)

//        if (ctxCls.tparams.nonEmpty) {
//          inform(s"Generated generic dim class:\n${showRaw(newCls)}")
//        }

        contextPackage.value.newClasses ::= newCls
      }

      def extractDepsFromSelfType(clsDef: MorpheusPreTyper.this.global.ClassDef): List[MorpheusPreTyper.this.global.Tree] = {
        val ValDef(_, _, selfTypeTree, _) = clsDef.impl.self

        val depsFromSelf: List[Tree] = selfTypeTree match {
          case ct@CompoundTypeTree(annotTempl) =>
            annotTempl.parents

          case id@Ident(dimTpName) =>
            List(id)
          //case null => Nil
          case _ =>
            //abort(s"Unsupported self-type in fragment: $selfTypeTree in fragment ${clsDef.name}")
            Nil
          //          case tt: TypeTree =>
          //            tt.original match {
          //              case ct@CompoundTypeTree(annotTempl) =>
          //                annotTempl.parents
          //
          //              case id@Ident(dimTpName) =>
          //                List(id)
          //              case null => Nil
          //              case _ => abort(s"Unsupported self-type in fragment: ${tt.original} in fragment ${clsDef.name}")
          //            }
        }

        depsFromSelf
      }

      def generateFragmentClassAnnotation(depsTypesTrees: List[Tree], configTypeTree: Tree = EmptyTree): Tree = {

        val depClsOfTrees = depsTypesTrees.map(depTree => {
          TypeApply(Ident(TermName("classOf")),
            List(depTree))
        })
        val depsAttr = List(AssignOrNamedArg(Ident(TermName("deps")), Apply(Ident(TermName("Array")),
          depClsOfTrees
        )))

        val configAttr = configTypeTree match {
          case EmptyTree => Nil
          case _ =>
            val attrValue = TypeApply(Ident(TermName("classOf")), List(configTypeTree))
            List(AssignOrNamedArg(Ident(TermName("config")), attrValue))
        }

        val annotConstr = Select(New(Select(Select(Ident(TermName("org")), TermName("morpheus")), TypeName("fragmentClass"))), termNames.CONSTRUCTOR)
        val attrs: List[AssignOrNamedArg] = depsAttr ::: configAttr
        val annotTree = Apply(annotConstr, attrs)

        annotTree
      }

      def generateFragmentClass(clsDef: ClassDef, isDim: Boolean, isWrapper: Boolean, isFragment: Boolean) {
        val ctxCls: ClassDef = contextClass.value.cls
        //val postfix = if (isWrapper) "wrapper" else "fragment"
        val postfix = "fragment"
        val newClassName = TypeName(s"${ctxCls.name}$$$postfix")

        val depsFromSelf: List[MorpheusPreTyper.this.global.Tree] = extractDepsFromSelfType(clsDef)

        //reporter.info(clsDef.pos, s"Fragment ${ctxCls.name} parents ${ctxCls.impl.parents.map(p => showRaw(p))}", true)

        // Try to find the config trait among the direct ancestors
        val (fragClassAnnot, classBody) = ctxCls.impl.parents.find {
          case AppliedTypeTree(Ident(TypeName("dlg")), List(Ident(TypeName(_)))) =>
            true
          case Ident(TypeName(parentName)) =>
            parentName == ctxCls.name + "Config"
          case _ => false
        } match {
          case Some(cfgParentTree) =>

            val configParent = cfgParentTree match {
              case AppliedTypeTree(_, List(delegateParent)) => delegateParent
              case _ => cfgParentTree
            }

            //inform(s"Found config parent: $configParent")

            val fragClsAnnot = generateFragmentClassAnnotation(depsFromSelf, configParent)

            // Annotate the synthetic constructor parameter $config$ by @autoproxy
            val annotParamMods = Modifiers(Flag.PARAMACCESSOR, typeNames.EMPTY, List(autoProxyAnnot))

            (fragClsAnnot, List(
              ValDef(annotParamMods, TermName("$config$"), configParent, EmptyTree),
              DefDef(Modifiers(Flag.SYNTHETIC, typeNames.EMPTY, List(fragClsAnnot)), termNames.CONSTRUCTOR, List(), List(List(ValDef(Modifiers(Flag.PARAM | Flag.PARAMACCESSOR), TermName("$config$"), configParent, EmptyTree))), TypeTree(),
                Block(List(Apply(Select(Super(This(newClassName), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))), EmptyTree))

          case None =>
            //inform(s"Config parent no found among : ${ctxCls.impl.parents.map(p => s"$p, class: ${p.getClass}")}")

            val fragClsAnnot = generateFragmentClassAnnotation(depsFromSelf, EmptyTree)

            (fragClsAnnot, List(
              DefDef(Modifiers(Flag.SYNTHETIC, typeNames.EMPTY, List(fragClsAnnot)), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(),
                Block(List(Apply(Select(Super(This(newClassName), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(()))))
            ))
        }

        val parentSelfType: ValDef = ctxCls.impl.self
        val newClsSelfType = treeCopy.ValDef(parentSelfType, parentSelfType.mods, parentSelfType.name, parentSelfType.tpt, parentSelfType.rhs)

        // TODO: New parent are not fully qualified. See https://github.com/zslajchrt/morpheus/issues/1
        val newParents: List[Tree] = if (isDim && isWrapper) {
          ctxCls.impl.parents match {
            case dimTrait :: others => dimTrait match {
              case Ident(TypeName(dimTraitName)) => List(Ident(TypeName(s"$dimTraitName$$dimension")), Ident(ctxCls.name))
              case AppliedTypeTree(Ident(TypeName(dimTraitName)), args) => List(AppliedTypeTree(Ident(TypeName(s"$dimTraitName$$dimension")), args), Ident(ctxCls.name))
              case _ => abort(s"Invalid dimension trait ${showRaw(dimTrait)} found in the list of parents of dimension wrapper ${ctxCls.name}")
            }
            case Nil => abort(s"No dimension trait found in the list of parents of dimension wrapper ${ctxCls.name}")
          }
        } else {
          List(Ident(ctxCls.name))
        }
        //inform(s"Fragment's new parents: ${newParents.map(showRaw(_))}")

        val clsMods: Modifiers = if (isFragment && isWrapper) {
          // We have to mark the "cap" class of a fragment wrapper as abstract since
          // the fragment may extend a configuration trait, which is unknown at his stage and thus
          // it si impossible to implement the config trait in the cap class of the wrapper.
          Modifiers(Flag.SYNTHETIC | Flag.ABSTRACT, typeNames.EMPTY, List(fragClassAnnot))
        } else {
          Modifiers(Flag.SYNTHETIC, typeNames.EMPTY, List(fragClassAnnot))
        }
        val newCls = generateClass(newClassName, clsMods, newParents, classBody, newClsSelfType, Nil)

        contextPackage.value.newClasses ::= newCls
      }

      tree match {

        case pkg@PackageDef(_, _) =>

          contextPackage.withValue(new ContextPackage(pkg)) {
            super.transform(pkg) match {
              case transPkg@PackageDef(n, stats) => if (contextPackage.value.newClasses.nonEmpty) {
                val newClsTrees: List[Tree] = contextPackage.value.newClasses
                val newPkg = treeCopy.PackageDef(transPkg, n, stats ::: newClsTrees)

                //inform(s"New package ${show(newPkg)}")

                newPkg
              } else {
                transPkg
              }

              case _ => sys.error("Should not be here")
            }
          }

        case cls@ClassDef(mods, _, _, _) =>

          contextClass.withValue(new ContextClass(cls)) {

            //inform(s"Context class: ${showRaw(cls)}")

            // Collect annotations
            val annots: List[String] = mods.annotations.map(annTree => {
              contextAnnotation.withValue(new ContextAnnotation) {
                super.transform(annTree)
                contextAnnotation.value.annotation
              }
            })

            val transfCls = super.transform(cls)

            val isDim = annots.contains("dimension")
            val isWrapper = annots.contains("wrapper")
            val isFragment = annots.contains("fragment")
            val isIgnored = annots.contains("ignore")

            if (!isIgnored && isDim && !isWrapper) {

              generateDimClass()

            } else if (!isIgnored && (isFragment || (isDim && isWrapper))) {

              generateFragmentClass(cls, isDim, isWrapper, isFragment)

            }

            transfCls

          }

        case ann@New(Ident(TypeName(annotName))) if contextAnnotation.value ne null =>
          contextAnnotation.value.annotation = annotName
          super.transform(ann)

        case ann@New(Select(_, TypeName(annotName))) if contextAnnotation.value ne null =>
          contextAnnotation.value.annotation = annotName
          super.transform(ann)

        case _ => super.transform(tree)
      }


    }
  }

}

