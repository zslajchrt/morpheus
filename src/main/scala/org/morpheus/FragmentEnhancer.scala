package org.morpheus

import java.io.IOException
import java.lang.ClassNotFoundException
import java.lang.reflect.Method
import java.util

import net.sf.cglib.proxy.{Enhancer, MethodInterceptor, MethodProxy}
import org.objectweb.asm.tree._
import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes}

/**
 *
 * Created by zslajchrt on 17/01/15.
 *
 */

class FragmentEnhancer(fragInstrClass: Class[_]) extends Enhancer {
  // initialization
  setSuperclass(fragInstrClass)
  setInterfaces(new Array[Class[_]](0))
  setCallback(new FragmentProxyHandler(fragInstrClass))
}

object FragmentEnhancer {

  @throws(classOf[Exception])
  def createFragmentProxy(compositeContext: FragmentInitContext, fragClass: Class[_], fragArgTypes: Array[Class[_]], fragArgs: Array[AnyRef]): AnyRef = {
    val fragInstrClass: Class[_] = instrumentFragmentClass(fragClass, removeSuperConstInvocations = false, replaceInvokeStaticForCtxSuper = false)
    val enhancer: Enhancer = new FragmentEnhancer(fragInstrClass)
    compositeContext.createProxy(enhancer, fragArgTypes, fragArgs)
  }

  @throws(classOf[Exception])
  def createFragmentWrapperProxy(compositeContext: FragmentInitContext, fragClass: Class[_], fragArgTypes: Array[Class[_]], fragArgs: Array[AnyRef], isDimWrapper: Boolean): AnyRef = {
    val fragInstrClass: Class[_] = instrumentFragmentClass(fragClass, removeSuperConstInvocations = true, // todo: removeSuperConstInvocations value should be !isDimWrapper, just like replaceInvokeStaticForCtxSuper
      replaceInvokeStaticForCtxSuper = !isDimWrapper)
    val enhancer: Enhancer = new FragmentEnhancer(fragInstrClass)
    compositeContext.createProxy(enhancer, fragArgTypes, fragArgs)
  }

  @throws(classOf[IOException])
  private def instrumentFragmentClass(fragmentClass: Class[_], removeSuperConstInvocations: Boolean, replaceInvokeStaticForCtxSuper: Boolean): Class[_] = {
    FragmentClassLoader.loadClass(fragmentClass, removeSuperConstInvocations, replaceInvokeStaticForCtxSuper)
  }

  private def instrumentFragmentClass_(fragmentClass: Class[_], removeSuperConstInvocations: Boolean, replaceInvokeStaticForCtxSuper: Boolean): Array[Byte] = {

    //todo: Remove all inherited fields in a fragment wrapper
    //todo: Invalidate all methods not overridden by a fragment wrapper since they are ignored by the filter proxy handler

    val fragmentTraitName = fragmentClass.getName.dropRight("$fragment".length).replace('.', '/')

    def isCallTo$class(insnNode: AbstractInsnNode): Boolean = {
      if (insnNode.getOpcode == Opcodes.INVOKESTATIC) {
        val minst = insnNode.asInstanceOf[MethodInsnNode]
        if (minst.owner.endsWith("$class")) {
          val ownerWithExtCutOff = minst.owner.dropRight("$class".length)
          ownerWithExtCutOff == fragmentTraitName
        } else {
          false
        }
      } else {
        false
      }
    }

    val cn: ClassNode = new ClassNode
    val cr: ClassReader = new ClassReader(fragmentClass.getName)

    cr.accept(cn, 0)
    val methodIter: util.Iterator[_] = cn.methods.iterator
    while (methodIter.hasNext) {
      val methodNode: MethodNode = methodIter.next.asInstanceOf[MethodNode]

      def findPrecedingALOAD0(insnNode: AbstractInsnNode): VarInsnNode = {
        // Find the preceding ALOAD 0
        var i = insnNode
        while (i != null && !(i.getOpcode == Opcodes.ALOAD && i.asInstanceOf[VarInsnNode].`var` == 0)) {
          i = i.getPrevious
        }
        if (i == null) {
          sys.error(s"A preceding ALOAD 0 not found in method ${methodNode.name} in class ${fragmentClass.getName}")
        }
        i.asInstanceOf[VarInsnNode]
      }

      val isConstructor = methodNode.name == "<init>"
      val isSuperAccessor = methodNode.name.contains("$$super$")

      val instructionsIter: util.ListIterator[_] = methodNode.instructions.iterator
      var constrCalls = List.empty[MethodInsnNode]
      while (instructionsIter.hasNext) {
        val insnNode: AbstractInsnNode = instructionsIter.next.asInstanceOf[AbstractInsnNode]

        //if (!isConstructor && isCallTo$class(insnNode)) {
        if (isCallTo$class(insnNode)) {
          // Insert the call to the method org.morpheus.CompositeUtils.context(this) which return the composite context before the current instruction.
          // Note: The retrieved context implements all composite interfaces.

          // Find the preceding ALOAD 0
          val aload0 = findPrecedingALOAD0(insnNode)
          val contextRetr = new MethodInsnNode(Opcodes.INVOKESTATIC, "org/morpheus/CompositeUtils", "context", "(Ljava/lang/Object;)Ljava/lang/Object;")
          val cast = new TypeInsnNode(Opcodes.CHECKCAST, fragmentTraitName)

          methodNode.instructions.insert(aload0, contextRetr)
          methodNode.instructions.insert(contextRetr, cast)

        }
        //        if (insnNode.getOpcode == Opcodes.CHECKCAST) {
        //          val tcInsn: TypeInsnNode = insnNode.asInstanceOf[TypeInsnNode]
        //          if (entityClassName == tcInsn.desc) {
        //            // insert a call to the method org.cloudio.morpheus.CompositeUtils.context(this) which return the composite context.
        //            // This context implements the current entity interface.
        //            methodNode.instructions.insertBefore(tcInsn, new MethodInsnNode(Opcodes.INVOKESTATIC, "org/cloudio/morpheus/codegen/CompositeUtils", "context", "(Ljava/lang/Object;)Ljava/lang/Object;"))
        //          }
        //        }

        if ((insnNode.getOpcode == Opcodes.INVOKESPECIAL || insnNode.getOpcode == Opcodes.INVOKESTATIC) && replaceInvokeStaticForCtxSuper && isSuperAccessor) {
          // Now we are in the context of a super accessor (...$$super$...)

          // We have to distinguish between entity and fragment super accessor as there is distinct protocol for invoking
          // the super instance. The distinction is made by the type of the method instruction responsible for
          // the super invocation: INVOKESPECIAL => entity, INVOKESTATIC => fragment

          val methodCall = insnNode.asInstanceOf[MethodInsnNode]

          val aload0 = findPrecedingALOAD0(insnNode)

          val superInterface: String = if (insnNode.getOpcode == Opcodes.INVOKESTATIC) {
            // determine the type of the super fragment from the fragment "$class" class
            methodCall.owner.dropRight("$class".length)
          } else {
            cn.superName
          }

          // this instruction retrieves the context super instance
          val superRetr: MethodInsnNode = new MethodInsnNode(Opcodes.INVOKESTATIC, "org/morpheus/CompositeUtils", "$super$", "(Ljava/lang/Object;)Ljava/lang/Object;")
          // we have to cast the contextual super instance (obtained by INVOKESTATIC) to the super class type
          val cast: TypeInsnNode = new TypeInsnNode(Opcodes.CHECKCAST, superInterface)
          methodNode.instructions.insert(aload0, superRetr)
          methodNode.instructions.insert(superRetr, cast)

          // change INVOKESPECIAL/INVOKESUPER to INVOKEVIRTUAL
          val replacementCall = if (insnNode.getOpcode == Opcodes.INVOKESTATIC) {
            // Take the arguments descriptor from the 'super' method because methodCall.desc because the INVOKEINTERFACE
            // instruction does not have the first argument for the instance as the INVOKESTATIC instruction does.
            new MethodInsnNode(Opcodes.INVOKEINTERFACE, superInterface, methodCall.name, methodNode.desc)
          } else {
            new MethodInsnNode(Opcodes.INVOKEVIRTUAL, methodCall.owner, methodCall.name, methodCall.desc)
          }
          methodNode.instructions.insertBefore(insnNode, replacementCall)
          methodNode.instructions.remove(insnNode)

        }

        // When instantiating a "fragment wrapper" we need to remove all $init$ invocations initializing super types. The reason is that
        // we need to avoid initializing the wrapped fragment, which the wrapper extends. One wrapper instance
        // may wrap more fragments under various contexts.
        if (removeSuperConstInvocations && isConstructor && insnNode.getOpcode == Opcodes.INVOKESTATIC) {
          val staticCall = insnNode.asInstanceOf[MethodInsnNode]
          if (staticCall.name == "$init$") {
            constrCalls ::= staticCall
          }
        }

      }

      if (removeSuperConstInvocations && isConstructor && constrCalls.nonEmpty) {
        // skip the first constructor invocation since it belongs to the current fragment class
        constrCalls.tail.foreach(c => {
          // neutralize super-trait init calls
          methodNode.instructions.insertBefore(c, new InsnNode(Opcodes.POP))
          methodNode.instructions.remove(c)
        })
      }

    }

    val cw: ClassWriter = new ClassWriter(0)
    cn.accept(cw)
    cw.toByteArray

  }

  object FragmentClassLoader extends ClassLoader() {

    //@throws(classOf[ClassNotFoundException])
    def loadClass(fragmentClass: Class[_], removeSuperConstInvocations: Boolean, replaceInvokeStaticForCtxSuper: Boolean): Class[_] = this.synchronized {
      val newClassName: String = fragmentClass.getName

      findLoadedClass(newClassName) match {
        case cls: Class[_] => cls
        case null =>
          val newClsBytes = instrumentFragmentClass_(fragmentClass, removeSuperConstInvocations, replaceInvokeStaticForCtxSuper)

          val newCls: Class[_] = defineClass(newClassName, newClsBytes)

          var printClass = false
          if (printClass) {
            val cr = new ClassReader(newClsBytes)
            ClassPrinter.printClass(cr)
          }

          newCls
      }
    }

    def defineClass(name: String, b: Array[Byte]): Class[_] = {
      defineClass(name, b, 0, b.length)
    }
  }

}


class FragmentProxyHandler(fragInstrClass: Class[_]) extends MethodInterceptor {

  override def intercept(fragmentProxy: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
    // it can check the presence of a composite context
    proxy.invokeSuper(fragmentProxy, args)
  }
}
