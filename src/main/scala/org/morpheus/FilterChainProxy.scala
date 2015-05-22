package org.morpheus

import java.lang.reflect.{Modifier, Method}
import java.util
import net.sf.cglib.core.ReflectUtils
import net.sf.cglib.proxy.{CallbackHelper, Enhancer, MethodInterceptor, MethodProxy}
import scala.annotation.tailrec
import scala.util.DynamicVariable
import scala.reflect.runtime.universe._
import scala.collection.JavaConversions._

/**
 * Created by zslajchrt on 17/01/15.
 *
 */
object FilterChainProxy {


  def apply(targetFragment: AnyRef, filterChainInterface: Array[Class[_]], filterFragmentInst: AnyRef, filterFragment: Frag[_, _], top: => Any) = {

    def findMethodDeclaredInFilterFragment(m: Method): Option[Method] = {

      def findDeclaredMethodRecursive(where: Class[_]): Option[Method] = {
        findDeclaredMethod(where) match {
          case mOpt@Some(foundMet) if !Modifier.isPrivate(foundMet.getModifiers) => mOpt
          case None =>
            val superCls = where.getSuperclass
            if (superCls != null) {
              // Try super class
              findDeclaredMethod(superCls)
            } else {
              // Try interfaces
              val intfs = where.getInterfaces
              if (intfs != null && intfs.length > 0) {
                val iter = intfs.iterator
                var foundMet: Option[Method] = None
                while (iter.hasNext && foundMet == None) {
                  val intf = iter.next()
                  foundMet = findDeclaredMethodRecursive(intf)
                }

                foundMet
              } else {
                None
              }
            }
        }
      }

      def findDeclaredMethod(where: Class[_]): Option[Method] = {
        try {
          val methodInFilter = where.getDeclaredMethod(m.getName, m.getParameterTypes: _*)
          Some(methodInFilter)
        }
        catch {
          case e: Throwable =>
            None
        }
      }

      if (m.getDeclaringClass == filterFragment.fragmentClass || m.getDeclaringClass == filterFragment.configClass) {
        Some(m)
      } else {
        findDeclaredMethod(filterFragment.fragmentClass) match {
          case None =>
            findDeclaredMethodRecursive(filterFragment.configClass)
          case m@Some(_) => m
        }
      }

    }

    def findMethodInInstance(m: Method, instance: AnyRef): Option[Method] = {
      val allMethods = new util.ArrayList()
      ReflectUtils.addAllMethods(instance.getClass, allMethods)
      findMethod(m, allMethods.toList)
    }

    def findMethod(m: Method, methods: List[Method]): Option[Method] = {
      methods.find(filterMethod => filterMethod.getName == m.getName &&
        filterMethod.getParameterTypes.deep == m.getParameterTypes.deep)
    }

    val enhancer = new Enhancer

    val (superClass, interfaces) = if (!filterChainInterface.head.isInterface)
    // the head is an entity class, thus it must be used as the superclass
      (filterChainInterface.head, filterChainInterface.tail)
    else
      (classOf[Object], filterChainInterface)

    object Helper extends CallbackHelper(superClass, interfaces) {

      override def getCallback(method: Method): AnyRef = {
        val methodInFilter = findMethodDeclaredInFilterFragment(method)
        if (methodInFilter.isDefined) {
          //val filterMet = findMethodInInstance(method, filterFragmentInst)
          if (filterFragmentInst.isInstanceOf[Super[_]]) {
            // this method will be handled by the 'abstract-override' filter
            new ContextSuperFilterChainProxyHandler(targetFragment, filterFragmentInst.asInstanceOf[Super[AnyRef]], methodInFilter.get, top)
          } else {
            // this method will be handled by the 'override' filter
            new FilterChainProxyHandler(targetFragment, filterFragmentInst, methodInFilter.get, top)
          }
        } else {
          val targetMet = findMethodInInstance(method, targetFragment)
          // this method will be handled by the target fragment
          targetMet match {
            case Some(tm) =>
              new FilterTargetProxyHandler(targetFragment, tm, top)
            case None =>
              sys.error(s"No method $method in target fragment ${targetFragment.getClass.getSuperclass}")
          }
        }
      }
    }

    enhancer.setSuperclass(superClass)
    enhancer.setInterfaces(interfaces)
    enhancer.setCallbackFilter(Helper)
    enhancer.setCallbacks(Helper.getCallbacks)
    enhancer.create()

  }

}

/**
 * Denotes abstract-override filters.
 * @tparam T
 */
trait Super[T] {
  def $$super$: DynamicVariable[T]
}

abstract class SuperBase[T] extends Super[T] {
  override val $$super$: DynamicVariable[T] = new DynamicVariable[T](null.asInstanceOf[T])

  def $super$: T = $$super$.value
}


/**
 * This is the handler for the fragment (concrete-override) filter.
 * @param targetFragment
 * @param filterFragment
 * @param filterMethod
 */
class FilterChainProxyHandler(targetFragment: AnyRef, filterFragment: AnyRef, filterMethod: Method, top: => Any) extends MethodInterceptor {
  override def intercept(fragmentProxy: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
    //filterMethod.invoke(filterFragment, args: _*)
    require(top != null)

    TopContext.stack.withValue(top) {
      SuperContext.stack.withValue(targetFragment) {
        proxy.invoke(filterFragment, args)
      }
    }
  }
}

class FilterTargetProxyHandler(targetFragment: AnyRef, targetMethod: Method, top: => Any) extends MethodInterceptor {
  override def intercept(fragmentProxy: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
    require(top != null)

    TopContext.stack.withValue(top) {
      targetMethod.invoke(targetFragment, args: _*)
    }
  }
}


/**
 * This handler wraps the dimension (abstract-override) filter and target fragments together. It just delegates all calls to the filter fragment
 * after informing the filter proxy handler about its 'super' fragment through the synthetic Super interface.
 *
 * Note: The filter does not hold the reference to the super fragment permanently. Contrariwise, the filter
 * fragment handler reads the current super fragment reference from a thread local field.
 *
 * @param targetFragment
 * @param filterFragment
 * @param filterMethod
 */
class ContextSuperFilterChainProxyHandler(targetFragment: AnyRef, filterFragment: Super[AnyRef], filterMethod: Method, top: => Any) extends MethodInterceptor {
  override def intercept(fragmentProxy: scala.Any, method: Method, args: Array[AnyRef], proxy: MethodProxy): AnyRef = {
    require(top != null)

    TopContext.stack.withValue(top) {
      filterFragment.$$super$.withValue(targetFragment) {
        filterMethod.invoke(filterFragment, args: _*)
      }
    }
  }
}

object SuperContext {

  val stack = new DynamicVariable[Any](null)

  def $super$(fragment: AnyRef): Any = stack.value

}

object TopContext {

  val stack = new DynamicVariable[Any](null)

  def top(fragment: Any): Any = stack.value match {
    case null => fragment
    case top => top
  }

}
