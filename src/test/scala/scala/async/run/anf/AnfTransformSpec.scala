/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package anf

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test
import scala.async.internal.AsyncId


class AnfTestClass {

  import ExecutionContext.Implicits.global

  def base(x: Int): Future[Int] = future {
    x + 2
  }

  def m(y: Int): Future[Int] = async {
    val blerg = base(y)
    await(blerg)
  }

  def m2(y: Int): Future[Int] = async {
    val f = base(y)
    val f2 = base(y + 1)
    await(f) + await(f2)
  }

  def m3(y: Int): Future[Int] = async {
    val f = base(y)
    var z = 0
    if (y > 0) {
      z = await(f) + 2
    } else {
      z = await(f) - 2
    }
    z
  }

  def m4(y: Int): Future[Int] = async {
    val f = base(y)
    val z = if (y > 0) {
      await(f) + 2
    } else {
      await(f) - 2
    }
    z + 1
  }

  def futureUnitIfElse(y: Int): Future[Unit] = async {
    val f = base(y)
    if (y > 0) {
      State.result = await(f) + 2
    } else {
      State.result = await(f) - 2
    }
  }
}

object State {
  @volatile var result: Int = 0
}

class AnfTransformSpec {

  @Test
  def `simple ANF transform`() {
    val o = new AnfTestClass
    val fut = o.m(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (12)
  }

  @Test
  def `simple ANF transform 2`() {
    val o = new AnfTestClass
    val fut = o.m2(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (25)
  }

  @Test
  def `simple ANF transform 3`() {
    val o = new AnfTestClass
    val fut = o.m3(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }

  @Test
  def `ANF transform of assigning the result of an if-else`() {
    val o = new AnfTestClass
    val fut = o.m4(10)
    val res = Await.result(fut, 2 seconds)
    res mustBe (15)
  }

  @Test
  def `Unit-typed if-else in tail position`() {
    val o = new AnfTestClass
    val fut = o.futureUnitIfElse(10)
    Await.result(fut, 2 seconds)
    State.result mustBe (14)
  }

  @Test
  def `inlining block does not produce duplicate definition`() {
    AsyncId.asyncId {
      val f = 12
      val x = AsyncId.awaitId(f)

      {
        type X = Int
        val x: X = 42
        println(x)
      }
      type X = Int
      x: X
    }
  }

  @Test
  def `inlining block in tail position does not produce duplicate definition`() {
    AsyncId.asyncId {
      val f = 12
      val x = AsyncId.awaitId(f)

      {
        val x = 42
        x
      }
    } mustBe (42)
  }

  @Test
  def `match as expression 1`() {
    import ExecutionContext.Implicits.global
    val result = AsyncId.asyncId {
      val x = "" match {
        case _ => AsyncId.awaitId(1) + 1
      }
      x
    }
    result mustBe (2)
  }

  @Test
  def `match as expression 2`() {
    import ExecutionContext.Implicits.global
    val result = AsyncId.asyncId {
      val x = "" match {
        case "" if false => AsyncId.awaitId(1) + 1
        case _           => 2 + AsyncId.awaitId(1)
      }
      val y = x
      "" match {
        case _ => AsyncId.awaitId(y) + 100
      }
    }
    result mustBe (103)
  }

  @Test
  def nestedAwaitAsBareExpression() {
    import ExecutionContext.Implicits.global
    import AsyncId.{asyncId, awaitId}
    val result = asyncId {
      awaitId(awaitId("").isEmpty)
    }
    result mustBe (true)
  }

  @Test
  def nestedAwaitInBlock() {
    import ExecutionContext.Implicits.global
    import AsyncId.{asyncId, awaitId}
    val result = asyncId {
      ()
      awaitId(awaitId("").isEmpty)
    }
    result mustBe (true)
  }

  @Test
  def nestedAwaitInIf() {
    import ExecutionContext.Implicits.global
    import AsyncId.{asyncId, awaitId}
    val result = asyncId {
      if ("".isEmpty)
        awaitId(awaitId("").isEmpty)
      else 0
    }
    result mustBe (true)
  }

  @Test
  def byNameExpressionsArentLifted() {
    import AsyncId.{asyncId, awaitId}
    def foo(ignored: => Any, b: Int) = b
    val result = asyncId {
      foo(???, awaitId(1))
    }
    result mustBe (1)
  }

  @Test
  def evaluationOrderRespected() {
    import AsyncId.{asyncId, awaitId}
    def foo(a: Int, b: Int) = (a, b)
    val result = asyncId {
      var i = 0
      def next() = {
        i += 1;
        i
      }
      foo(next(), awaitId(next()))
    }
    result mustBe ((1, 2))
  }

  @Test
  def awaitInNonPrimaryParamSection1() {
    import AsyncId.{asyncId, awaitId}
    def foo(a0: Int)(b0: Int) = s"a0 = $a0, b0 = $b0"
    val res = asyncId {
      var i = 0
      def get = {i += 1; i}
      foo(get)(awaitId(get))
    }
    res mustBe "a0 = 1, b0 = 2"
  }

  @Test
  def awaitInNonPrimaryParamSection2() {
    import AsyncId.{asyncId, awaitId}
    def foo[T](a0: Int)(b0: Int*) = s"a0 = $a0, b0 = ${b0.head}"
    val res = asyncId {
      var i = 0
      def get = asyncId {i += 1; i}
      foo[Int](awaitId(get))(awaitId(get) :: awaitId(asyncId(Nil)) : _*)
    }
    res mustBe "a0 = 1, b0 = 2"
  }

  @Test
  def awaitInNonPrimaryParamSectionWithLazy1() {
    import AsyncId.{asyncId, awaitId}
    def foo[T](a: => Int)(b: Int) = b
    val res = asyncId {
      def get = asyncId {0}
      foo[Int](???)(awaitId(get))
    }
    res mustBe 0
  }

  @Test
  def awaitInNonPrimaryParamSectionWithLazy2() {
    import AsyncId.{asyncId, awaitId}
    def foo[T](a: Int)(b: => Int) = a
    val res = asyncId {
      def get = asyncId {0}
      foo[Int](awaitId(get))(???)
    }
    res mustBe 0
  }

  @Test
  def awaitWithLazy() {
    import AsyncId.{asyncId, awaitId}
    def foo[T](a: Int, b: => Int) = a
    val res = asyncId {
      def get = asyncId {0}
      foo[Int](awaitId(get), ???)
    }
    res mustBe 0
  }

  @Test
  def awaitOkInReciever() {
    import AsyncId.{asyncId, awaitId}
    class Foo { def bar(a: Int)(b: Int) = a + b }
    asyncId {
      awaitId(asyncId(new Foo)).bar(1)(2)
    }
  }

  @Test
  def namedArgumentsRespectEvaluationOrder() {
    import AsyncId.{asyncId, awaitId}
    def foo(a: Int, b: Int) = (a, b)
    val result = asyncId {
      var i = 0
      def next() = {
        i += 1;
        i
      }
      foo(b = next(), a = awaitId(next()))
    }
    result mustBe ((2, 1))
  }

  @Test
  def namedAndDefaultArgumentsRespectEvaluationOrder() {
    import AsyncId.{asyncId, awaitId}
    var i = 0
    def next() = {
      i += 1;
      i
    }
    def foo(a: Int = next(), b: Int = next()) = (a, b)
    asyncId {
      foo(b = awaitId(next()))
    } mustBe ((2, 1))
    i = 0
    asyncId {
      foo(a = awaitId(next()))
    } mustBe ((1, 2))
  }

  @Test
  def repeatedParams1() {
    import AsyncId.{asyncId, awaitId}
    var i = 0
    def foo(a: Int, b: Int*) = b.toList
    def id(i: Int) = i
    asyncId {
      foo(awaitId(0), id(1), id(2), id(3), awaitId(4))
    } mustBe (List(1, 2, 3, 4))
  }

  @Test
  def repeatedParams2() {
    import AsyncId.{asyncId, awaitId}
    var i = 0
    def foo(a: Int, b: Int*) = b.toList
    def id(i: Int) = i
    asyncId {
      foo(awaitId(0), List(id(1), id(2), id(3)): _*)
    } mustBe (List(1, 2, 3))
  }

  @Test
  def awaitInThrow() {
    import _root_.scala.async.internal.AsyncId.{asyncId, awaitId}
    intercept[Exception](
      asyncId {
        throw new Exception("msg: " + awaitId(0))
      }
    ).getMessage mustBe "msg: 0"
  }

  @Test
  def awaitInTyped() {
    import _root_.scala.async.internal.AsyncId.{asyncId, awaitId}
    asyncId {
      (("msg: " + awaitId(0)): String).toString
    } mustBe "msg: 0"
  }


  @Test
  def awaitInAssign() {
    import _root_.scala.async.internal.AsyncId.{asyncId, awaitId}
    asyncId {
      var x = 0
      x = awaitId(1)
      x
    } mustBe 1
  }

  @Test
  def caseBodyMustBeTypedAsUnit() {
    import _root_.scala.async.internal.AsyncId.{asyncId, awaitId}
    val Up = 1
    val Down = 2
    val sign = asyncId {
      awaitId(1) match {
        case Up   => 1.0
        case Down => -1.0
      }
    }
    sign mustBe 1.0
  }

  @Test
  def awaitInImplicitApply() {
    val tb = mkToolbox(s"-cp ${toolboxClasspath}")
    val tree = tb.typeCheck(tb.parse {
      """
        | import language.implicitConversions
        | import _root_.scala.async.internal.AsyncId.{asyncId, awaitId}
        | implicit def view(a: Int): String = ""
        | asyncId {
        |   awaitId(0).length
        | }
      """.stripMargin
    })
    val applyImplicitView = tree.collect { case x if x.getClass.getName.endsWith("ApplyImplicitView") => x }
    applyImplicitView.map(_.toString) mustStartWith List("view(a$macro$")
  }
}
