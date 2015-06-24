package org.morpheus

import java.lang.reflect.Method

import net.sf.cglib.proxy._
import shapeless.ops.hlist.Modifier

import scala.util.DynamicVariable

/**
 * Created by zslajchrt on 14/01/15.
 *
 */
object MorphContext {

  val stack = new DynamicVariable[MorphContext](null)

  def context(fragment: AnyRef): AnyRef = stack.value.context(fragment)

}

trait MorphContext {

  protected def context(fragment: AnyRef): AnyRef

  def inContext[R](action: => R): R = {
    MorphContext.stack.withValue(this) {
      action
    }
  }

}

/**
 *
 *
 * @param contextDimensions The context dimensions. The first dimension must be the entity class
 * @param contextFragments Implementations of the context dimensions
 */
abstract class CompleteMorphContext[M, L, ConformLev <: ConformanceLevelMarker](contextDimensions: Array[List[Class[_]]], contextFragments: Array[_],
                                                                                val compositeInstance: MorphKernel[M] {type LUB = L; type ConformLevel = ConformLev},
                                                                                alternative: List[FragmentHolder[_]],
                                                                                alts: Alternatives[M],
                                                                                usedStrategy: MorphingStrategy[M]) extends MorphContext {

  val owningProxy: Option[compositeInstance.MutableLUB]

  // check the dimensions and the corresponding instances
  require(contextDimensions.length == contextFragments.length, s"Number if context dimensions and context fragments do not match: ${contextDimensions.length}!=${contextFragments.length}")
  contextDimensions zip contextFragments find (d => !d._1.forall(_.isInstance(d._2))) match {
    case None => // OK
    case Some((dimCls, dimFrag)) =>
      sys.error(s"Context fragment $dimFrag does not conform to dimension $dimCls")
  }
  private val contextDimWithFragments: Array[(List[Class[_]], _)] = contextDimensions zip contextFragments map (df => (df._1, df._2))

  val (entCls, allInterfaces) = if (contextDimensions.nonEmpty) {
    val entCls: Class[_] = contextDimensions.head.last
    if (entCls.isInterface) {
      (classOf[AnyRef], contextDimensions.flatMap(fragClasses => fragClasses) ++ Array(classOf[MorphMirror[M]]))
    } else {
      (entCls, contextDimensions(0).dropRight(1).toArray ++ contextDimensions.tail.flatMap(fragClasses => fragClasses) ++ Array(classOf[MorphMirror[M]]))
    }
  } else {
    (classOf[AnyRef], Array.empty[Class[_]])
  }

  private[this] var contextProxy: AnyRef = context(null)

  def proxy = contextProxy

  val compositeMirror = new MorphMirror[compositeInstance.Model] {

    override type ConfLev = compositeInstance.ConformLevel
    override type LUB = compositeInstance.LUB

    override def remorph() = {
      Morpher.morph[compositeInstance.Model](compositeInstance, usedStrategy)(None)
    }

    override def remorph(altStrategy: MorphingStrategy[compositeInstance.Model]) = {
      Morpher.morph[compositeInstance.Model](kernel, altStrategy)(None)
    }

    override val kernel = compositeInstance

    override def myAlternative: List[FragmentHolder[_]] = alternative

    override def alternatives: Alternatives[M] = alts

    /**
     * @return the strategy used to rank the alternatives
     */
    override def strategy: MorphingStrategy[M] = usedStrategy

    override def owningMutableProxy = owningProxy
  }

  override def context(fragment: AnyRef): AnyRef = if (contextProxy == null) {
    val enhancer = new Enhancer
    enhancer.setSuperclass(entCls)
    enhancer.setInterfaces(allInterfaces)
    val helper: Helper = new Helper()
    enhancer.setCallbackFilter(helper)
    enhancer.setCallbacks(helper.getCallbacks)
    contextProxy = enhancer.create()
    contextProxy
  } else {
    contextProxy
  }

  class Helper() extends CallbackHelper(entCls, allInterfaces) {
    override def getCallback(method: Method): AnyRef = {
      val declaringClass = method.getDeclaringClass

      if (declaringClass == classOf[MorphMirror[M]])
        new MethodInterceptor {
          override def intercept(obj: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
            inContext {
              proxy.invoke(compositeMirror, args)
            }
          }
        }
      else {
        val invokedCtxDimensionWithFragmentOpt = if (declaringClass == classOf[Object])
        // the methods from Object delegate to the entity
          Some(contextDimWithFragments(0))
        else
          contextDimWithFragments.find(df => df._1.exists(intf => declaringClass.isAssignableFrom(intf)))

        invokedCtxDimensionWithFragmentOpt match {
          case Some((_, outerFrag)) =>
            new MethodInterceptor {
              override def intercept(obj: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
                inContext {
                  proxy.invoke(outerFrag, args)
                }
              }
            }
          case None => sys.error(s"No context fragment matches method $method")
        }
      }
    }
  }

}

/**
 *
 * @param fragmentTrait the class of the fragment under initialization
 */
class FragmentInitContext(fragmentTrait: Class[_]) extends MorphContext {

  //  val contextDimensions: Array[Class[_]] = if (ctxDims.isEmpty)
  //    Array(classOf[AnyRef])
  //  else
  //    ctxDims

  val (entCls, allInterfaces) = {
    if (fragmentTrait.isInterface) {
      (classOf[AnyRef], Array[Class[_]](fragmentTrait))
    } else {
      (fragmentTrait, Array.empty[Class[_]])
    }
  }

  private[this] var contextProxy: AnyRef = null

  def proxy = contextProxy

  def createProxy(enhancer: Enhancer, argumentTypes: Array[Class[_]], arguments: Array[AnyRef]) = {
    inContext {
      enhancer.create(argumentTypes, arguments)
    }
  }

  /**
   * The callback invoked from the constructor to obtain the context proxy
   * @param fragment the fragment instance being initialized
   * @return the context proxy implementing the fragment's trait and exposing unimplemented dependency dimensions
   */
  override def context(fragment: AnyRef): AnyRef = if (contextProxy == null) {
    val enhancer = new Enhancer
    enhancer.setSuperclass(entCls)
    enhancer.setInterfaces(allInterfaces)
    val helper: Helper = new Helper(fragment)
    enhancer.setCallbackFilter(helper)
    enhancer.setCallbacks(helper.getCallbacks)
    contextProxy = enhancer.create()
    contextProxy
  } else {
    contextProxy
  }

  class Helper(fragment: AnyRef) extends CallbackHelper(entCls, allInterfaces) {
    override def getCallback(method: Method): AnyRef = {
      val declaringClass = method.getDeclaringClass
      if (declaringClass.isAssignableFrom(fragmentTrait)) {
        // handle the fragment's methods only
        new MethodInterceptor {
          override def intercept(obj: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
            inContext {
              proxy.invoke(fragment, args)
            }
          }
        }
      } else {
        // the other invocations end up with error
        new MethodInterceptor {
          override def intercept(obj: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
            sys.error(s"Cannot invoke method $method on a context dimension during initialization of fragment ${fragment.getClass.getSuperclass}")
          }
        }
      }

    }
  }

}

//abstract class MutableMorphContext[M, L, ConformLev <: ConformanceLevelMarker, ImmutableLUB <: L with MorphMirror[M, L], MutableLUB <: MutableMorphMirror[M, L]](
abstract class MutableMorphContext[M, L, ConformLev <: ConformanceLevelMarker](
                                                                                val owningKernel: MorphKernel[M] {type LUB = L; type ConformLevel = ConformLev},
                                                                                lubComponents: Array[Class[_]],
                                                                                initialStrategy: MorphingStrategy[M]) extends MorphContext {

  @volatile private[this] var delegate: owningKernel.ImmutableLUB = _

  def morph(proxy: owningKernel.MutableLUB, strategy: MorphingStrategy[M]): owningKernel.ImmutableLUB

  val compositeMirror = new MutableMorphMirror[owningKernel.Model] {

    override type ConfLev = ConformLev
    override type LUB = L

    override def remorph() = {
      MutableMorphContext.this.delegate = morph(proxy, MutableMorphContext.this.delegate.strategy)
      MutableMorphContext.this.delegate
    }

    override def remorph(altStrategy: MorphingStrategy[M]) = {
      MutableMorphContext.this.delegate = morph(proxy, altStrategy)
      MutableMorphContext.this.delegate
    }

    def delegate: kernel.ImmutableLUB = MutableMorphContext.this.delegate.asInstanceOf[kernel.ImmutableLUB]

    /**
     * This method is delegated to the wrapped delegate.
     * @return
     */
    override val kernel = owningKernel

    /**
     * This method is delegated to the wrapped delegate.
     */
    override def myAlternative: List[FragmentHolder[_]] = ???

    /**
     * This method is delegated to the wrapped delegate.
     * @return The alternatives from which the winning myAlternative was chosen.
     */
    override def alternatives: Alternatives[M] = ???

    /**
     * This method is delegated to the wrapped delegate.
     * @return the strategy used to rank the alternatives
     */
    override def strategy: MorphingStrategy[M] = ???

    /**
     * This method is delegated to the wrapped delegate.
     */
    override def owningMutableProxy = ???


  }

  val (entCls, allInterfaces) = {
    val firstDim = lubComponents(0)
    if (firstDim.isInterface) {
      (classOf[AnyRef], lubComponents ++ Array(classOf[MutableMorphMirror[M]]))
    } else {
      (firstDim, lubComponents.tail ++ Array(classOf[MutableMorphMirror[M]]))
    }
  }

  lazy val proxy: owningKernel.MutableLUB = {
    val enhancer = new Enhancer
    enhancer.setSuperclass(entCls)
    enhancer.setInterfaces(allInterfaces)
    val helper: Helper = new Helper()
    enhancer.setCallbackFilter(helper)
    enhancer.setCallbacks(helper.getCallbacks)
    val px = enhancer.create().asInstanceOf[owningKernel.MutableLUB]

    delegate = morph(px, initialStrategy)

    px
  }

  protected def context(fragment: AnyRef): AnyRef = {
    proxy
  }

  class Helper() extends CallbackHelper(entCls, allInterfaces) {
    override def getCallback(method: Method): AnyRef = {
      val declaringClass = method.getDeclaringClass
      if (declaringClass == classOf[MutableMorphMirror[_]] || "remorph" == method.getName) // todo: do it better
        new MethodInterceptor {
          override def intercept(obj: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
            inContext {
              proxy.invoke(compositeMirror, args)
            }
          }
        }
      else
        new MethodInterceptor {
          override def intercept(obj: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
            inContext {
              val res = proxy.invoke(delegate, args)
              if (res eq delegate.asInstanceOf[AnyRef]) {
                // todo: Document this behavior!!!
                // The proxy invocation returned 'this'. Change it to this 'this'.
                MutableMorphContext.this.proxy
              } else {
                res
              }
            }
          }
        }
    }
  }

}