package org.morpheus.codegen

import java.io.{FileWriter, BufferedReader, File}

import scala.io.Source
import scala.tools.nsc.Global
import scala.tools.nsc.ast.TreeDSL
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.{Transform, TypingTransformers}
import scala.util.DynamicVariable

/**
 * Created by zslajchrt on 12/11/15.
 */
class AltMapDumper(val global: Global) extends PluginComponent
with Transform
with TypingTransformers
with TreeDSL {

  import global._

  //import definitions._

  import global.Tree

  override val runsAfter = List[String]("typer")
  //val runsAfter = List[String]("genJVM")
  override val terminal: Boolean = true
  val phaseName = "altmapdumper"

  def newTransformer(unit: CompilationUnit) = new AltMapDumperTransformer(unit)

  class AltMapDumperTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    class ContextPackage(val pkg: PackageDef) {
      lazy val packagePath = show(pkg.pid).replace(".", File.separator)

      def createAltMapOutDir = {
        val outDir = new File(global.settings.outputDirs.outputDirFor(unit.source.file).file, packagePath)
        outDir.mkdirs()
        outDir
      }

    }

    val contextPackage: DynamicVariable[ContextPackage] = new DynamicVariable[ContextPackage](null)


    override def transform(tree: Tree): Tree = {

      tree match {
        case pkg@PackageDef(pid, _) =>
          contextPackage.withValue(new ContextPackage(pkg)) {
            super.transform(tree)
          }

        case am@Apply(Select(Select(Select(Ident(TermName("org")), TermName("morpheus")), TermName("AltMappings")), TermName("apply")), List(Literal(Constant(altMapStr)))) =>
          if (altMapStr.toString.startsWith("file:")) {
            val altMapResourceName: String = s"${unit.source.file.name.dropRight(".scala".length)}_line_${am.pos.line}.am"
            val altMapFile = new File(contextPackage.value.createAltMapOutDir, altMapResourceName)

            writeAltMap(altMapStr, altMapFile)

            val altMapResourceFile = new File(contextPackage.value.packagePath, altMapResourceName)
            val newAm = localTyper.typed(am.copy(args = List(Literal(Constant(s"file:${altMapResourceFile.getPath}")))))
            super.transform(newAm)
          } else {
            super.transform(tree)
          }

        case _ => super.transform(tree)
      }
    }

    def writeAltMap(altMapStr: Any, altMapFile: File): Unit = {
      val altMapSrcPath: String = altMapStr.toString.drop("file:".length)
      val source = Source.fromFile(altMapSrcPath)
      val lines = try source.mkString finally source.close()
      val amWriter = new FileWriter(altMapFile)
      try amWriter.write(lines) finally amWriter.close()
    }
  }

}
