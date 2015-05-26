package org.morpheus

import java.lang.reflect.Method

import net.sf.cglib.proxy._

import scala.util.DynamicVariable

/**
 * Created by zslajchrt on 14/01/15.
 *
 */
object CompositeContext {

  val stack = new DynamicVariable[CompositeContext](null)

  def context(fragment: AnyRef): AnyRef = stack.value.context(fragment)

}

trait CompositeContext {

  protected def context(fragment: AnyRef): AnyRef

  def inContext[R](action: => R): R = {
    CompositeContext.stack.withValue(this) {
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
class CompleteCompositeContext[M, LUB, ConformLev <: ConformanceLevelMarker](contextDimensions: Array[List[Class[_]]], contextFragments: Array[_],
                                       compositeInstance: CompositeInstance[M] with ConformLev, alternative: List[FragmentHolder[_]],
                                       alts: Alternatives[M], usedStrategy: MorpherStrategy[M],
                                       owningProxy: Option[LUB with MutableCompositeMirror[M, LUB]]) extends CompositeContext {

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
      (classOf[AnyRef], contextDimensions.flatMap(fragClasses => fragClasses) ++ Array(classOf[CompositeMirror[M, LUB]]))
    } else {
      (entCls, contextDimensions(0).dropRight(1).toArray ++ contextDimensions.tail.flatMap(fragClasses => fragClasses) ++ Array(classOf[CompositeMirror[M, LUB]]))
    }
  } else {
    (classOf[AnyRef], Array.empty[Class[_]])
  }

  private[this] var contextProxy: AnyRef = context(null)

  def proxy = contextProxy

  val compositeMirror = new CompositeMirror[M, LUB] {

    override type ConfLev = ConformLev

    override def toCompositeInstance = compositeInstance

    override def myAlternative: List[FragmentHolder[_]] = alternative

    override def alternatives: Alternatives[M] = alts

    /**
     * @return the strategy used to rank the alternatives
     */
    override def strategy: MorpherStrategy[M] = usedStrategy

    override def owningMutableProxy: Option[LUB with MutableCompositeMirror[M, LUB]] = owningProxy
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

      if (declaringClass == classOf[CompositeMirror[M, LUB]])
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
class FragmentInitContext(fragmentTrait: Class[_]) extends CompositeContext {

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

abstract class MutableCompositeContext[M, LUB, ConformLev <: ConformanceLevelMarker, ImutableLUB <: LUB with CompositeMirror[M, LUB], MutableLUB <: MutableCompositeMirror[M, LUB]](
                                                            lubComponents: Array[Class[_]],
                                                            initialStrategy: MorpherStrategy[M]) extends CompositeContext {

  @volatile private[this] var delegate: ImutableLUB = _

  def morph(proxy: MutableLUB, strategy: MorpherStrategy[M]): ImutableLUB

  val compositeMirror = new MutableCompositeMirror[M, LUB] {

    override type ConfLev = ConformLev

    def remorph() {
      MutableCompositeContext.this.delegate = morph(proxy, MutableCompositeContext.this.delegate.strategy)
    }

    def remorph(altStrategy: MorpherStrategy[M]) {
      MutableCompositeContext.this.delegate = morph(proxy, altStrategy)
    }

    def delegate: LUB = MutableCompositeContext.this.delegate

    /**
     * This method is delegated to the wrapped delegate.
     * @return
     */
    override def toCompositeInstance: CompositeInstance[M] with ConfLev = ???

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
    override def strategy: MorpherStrategy[M] = ???

    /**
     * This method is delegated to the wrapped delegate.
     */
    override def owningMutableProxy: Option[LUB with MutableCompositeMirror[M, LUB]] = ???


  }

  val (entCls, allInterfaces) = {
    val firstDim = lubComponents(0)
    if (firstDim.isInterface) {
      (classOf[AnyRef], lubComponents ++ Array(classOf[MutableCompositeMirror[M, LUB]]))
    } else {
      (firstDim, lubComponents.tail ++ Array(classOf[MutableCompositeMirror[M, LUB]]))
    }
  }

  lazy val proxy: MutableLUB = {
    val enhancer = new Enhancer
    enhancer.setSuperclass(entCls)
    enhancer.setInterfaces(allInterfaces)
    val helper: Helper = new Helper()
    enhancer.setCallbackFilter(helper)
    enhancer.setCallbacks(helper.getCallbacks)
    val px = enhancer.create().asInstanceOf[MutableLUB]

    delegate = morph(px, initialStrategy)

    px
  }

  protected def context(fragment: AnyRef): AnyRef = {
    proxy
  }

  class Helper() extends CallbackHelper(entCls, allInterfaces) {
    override def getCallback(method: Method): AnyRef = {
      val declaringClass = method.getDeclaringClass
      if (declaringClass == classOf[MutableCompositeMirror[M, LUB]] || declaringClass == classOf[Mutator[M]]) // todo: do it better
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
                MutableCompositeContext.this.proxy
              } else {
                res
              }
            }
          }
        }
    }
  }

}