package org.morpheus

import java.io.PrintWriter

import org.objectweb.asm.util.TraceClassVisitor
import org.objectweb.asm.{ClassReader, ClassWriter}

/**
 * Created by zslajchrt on 01/03/15.
 */
object ClassPrinter {

  def printClass(cr: ClassReader)  {
    val cw: ClassWriter = new ClassWriter(0)
    val printWriter: PrintWriter = new PrintWriter(System.out)
    val cv: TraceClassVisitor = new TraceClassVisitor(cw, printWriter)
    cr.accept(cv, 0)
  }

}
