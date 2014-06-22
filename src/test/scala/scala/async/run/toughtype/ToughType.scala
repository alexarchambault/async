/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package toughtype

import language.{reflectiveCalls, postfixOps}
import scala.concurrent._
import scala.concurrent.duration._
import scala.async.Async._
import org.junit.{Assert, Test}
import scala.async.internal.AsyncId


object ToughTypeObject {

  import ExecutionContext.Implicits.global

  class Inner

  def m2 = async[(List[_], ToughTypeObject.Inner)] {
    val y = await(future[List[_]](Nil))
    val z = await(future[Inner](new Inner))
    (y, z)
  }
}

class ToughTypeSpec {

  @Test def `propogates tough types`() {
    val fut = ToughTypeObject.m2
    val res: (List[_], scala.async.run.toughtype.ToughTypeObject.Inner) = Await.result(fut, 2 seconds)
    res._1 mustBe (Nil)
  }

  @Test def patternMatchingPartialFunction() {
    import AsyncId.{awaitId, asyncId}
    asyncId {
      awaitId(1)
      val a = awaitId(1)
      val f = { case x => x + a }: PartialFunction[Int, Int]
      awaitId(f(2))
    } mustBe 3
  }

  @Test def patternMatchingPartialFunctionNested() {
    import AsyncId.{awaitId, asyncId}
    asyncId {
      awaitId(1)
      val neg1 = -1
      val a = awaitId(1)
      val f = { case x => ({case x => neg1 * x}: PartialFunction[Int, Int])(x + a) }: PartialFunction[Int, Int]
      awaitId(f(2))
    } mustBe -3
  }

  @Test def patternMatchingFunction() {
    import AsyncId.{awaitId, asyncId}
    asyncId {
      awaitId(1)
      val a = awaitId(1)
      val f = { case x => x + a }: Function[Int, Int]
      awaitId(f(2))
    } mustBe 3
  }

  @Test def existentialBindIssue19() {
    import AsyncId.{awaitId, asyncId}
    def m7(a: Any) = asyncId {
      a match {
        case s: Seq[_] =>
          val x = s.size
          var ss = s
          ss = s
          awaitId(x)
      }
    }
    m7(Nil) mustBe 0
  }

  @Test def existentialBind2Issue19() {
    import scala.async.Async._, scala.concurrent.ExecutionContext.Implicits.global
    def conjure[T]: T = null.asInstanceOf[T]

    def m3 = async {
      val p: List[Option[_]] = conjure[List[Option[_]]]
      await(future(1))
    }

    def m4 = async {
      await(future[List[_]](Nil))
    }
  }

  @Test def singletonTypeIssue17() {
    import AsyncId.{asyncId, awaitId}
    class A { class B }
    asyncId {
      val a = new A
      def foo(b: a.B) = 0
      awaitId(foo(new a.B))
    }
  }

  @Test def existentialMatch() {
    import AsyncId.{asyncId, awaitId}
    trait Container[+A]
    case class ContainerImpl[A](value: A) extends Container[A]
    def foo: Container[_] = asyncId {
      val a: Any = List(1)
      a match {
        case buf: Seq[_] =>
          val foo = awaitId(5)
          val e0 = buf(0)
          ContainerImpl(e0)
      }
    }
    foo
  }

  @Test def existentialIfElse0() {
    import AsyncId.{asyncId, awaitId}
    trait Container[+A]
    case class ContainerImpl[A](value: A) extends Container[A]
    def foo: Container[_] = asyncId {
      val a: Any = List(1)
      if (true) {
        val buf: Seq[_] = List(1)
        val foo = awaitId(5)
        val e0 = buf(0)
        ContainerImpl(e0)
      } else ???
    }
    foo
  }

  // This test was failing when lifting `def r` with:
  // symbol value m#10864 does not exist in r$1
  //
  // We generated:
  //
  //   private[this] def r$1#5727[A#5728 >: Nothing#157 <: Any#156](m#5731: Foo#2349[A#5728]): Unit#208 = Bippy#2352.this.bar#5532({
  //     m#5730;
  //     ()
  //   });
  //
  // Notice the incorrect reference to `m`.
  //
  // We compensated in `Lifter` by copying `ValDef` parameter symbols directly across.
  //
  // Turns out the behaviour stems from `thisMethodType` in `Namers`, which treats type parameter skolem symbols.
  @Test def nestedMethodWithInconsistencyTreeAndInfoParamSymbols() {
    import language.{reflectiveCalls, postfixOps}
    import scala.concurrent.{Future, ExecutionContext, future, Await}
    import scala.concurrent.duration._
    import scala.async.Async.{async, await}
    import scala.async.internal.AsyncId

    class Foo[A]

    object Bippy {

      import ExecutionContext.Implicits.global

      def bar(f: => Unit): Unit = f

      def quux: Future[String] = ???

      def foo = async {
        def r[A](m: Foo[A])(n: A) = {
          bar {
            locally(m)
            locally(n)
            identity[A] _
          }
        }

        await(quux)

        r(new Foo[String])("")
      }
    }
    Bippy
  }

  @Test
  def ticket63(): Unit = {
    import scala.async.Async._
    import scala.concurrent.{ ExecutionContext, Future }

    object SomeExecutionContext extends ExecutionContext {
      def reportFailure(t: Throwable): Unit = ???
      def execute(runnable: Runnable): Unit = ???
    }

    trait FunDep[W, S, R] {
      def method(w: W, s: S): Future[R]
    }

    object FunDep {
      implicit def `Something to do with List`[W, S, R](implicit funDep: FunDep[W, S, R]) =
        new FunDep[W, List[S], W] {
          def method(w: W, l: List[S]) = async {
            val it = l.iterator
            while (it.hasNext) {
              await(funDep.method(w, it.next()))
            }
            w
          }(SomeExecutionContext)
        }
    }
  }
}

trait A

trait B

trait L[A2, B2 <: A2] {
  def bar(a: Any, b: Any) = 0
}
