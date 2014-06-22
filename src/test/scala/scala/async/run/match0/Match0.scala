/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package match0

import language.{reflectiveCalls, postfixOps}
import scala.concurrent.{Future, ExecutionContext, future, Await}
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.junit.Test
import scala.async.internal.AsyncId


class TestMatchClass {

  import ExecutionContext.Implicits.global

  def m1(x: Int): Future[Int] = future {
    x + 2
  }

  def m2(y: Int): Future[Int] = async {
    val f = m1(y)
    var z = 0
    y match {
      case 10 =>
        val x1 = await(f)
        z = x1 + 2
      case 20 =>
        val x2 = await(f)
        z = x2 - 2
    }
    z
  }

  def m3(y: Int): Future[Int] = async {
    val f = m1(y)
    var z = 0
    y match {
      case 0 =>
        val x2 = await(f)
        z = x2 - 2
      case 1 =>
        val x1 = await(f)
        z = x1 + 2
    }
    z
  }
}


class MatchSpec {

  @Test def `support await in a simple match expression`() {
    val o = new TestMatchClass
    val fut = o.m2(10) // matches first case
    val res = Await.result(fut, 2 seconds)
    res mustBe (14)
  }

  @Test def `support await in a simple match expression 2`() {
    val o = new TestMatchClass
    val fut = o.m3(1) // matches second case
    val res = Await.result(fut, 2 seconds)
    res mustBe (5)
  }

  @Test def `support await in a match expression with binds`() {
    val result = AsyncId.asyncId {
      val x = 1
      Option(x) match {
        case op @ Some(x) =>
          assert(op == Some(1))
          x + AsyncId.awaitId(x)
        case None => AsyncId.awaitId(0)
      }
    }
    result mustBe (2)
  }

  @Test def `support await referring to pattern matching vals`() {
    import AsyncId.{asyncId, awaitId}
    val result = asyncId {
      val x = 1
      val opt = Some("")
      awaitId(0)
      val o @ Some(y) = opt

      {
        val o @ Some(y) = Some(".")
      }

      awaitId(0)
      awaitId((o, y.isEmpty))
    }
    result mustBe ((Some(""), true))
  }

  @Test def `await in scrutinee`() {
    import AsyncId.{asyncId, awaitId}
    val result = asyncId {
      awaitId(if ("".isEmpty) awaitId(1) else ???) match {
        case x if x < 0 => ???
        case y: Int => y * awaitId(3)
      }
    }
    result mustBe (3)
  }

  @Test def duplicateBindName() {
    import AsyncId.{asyncId, awaitId}
    def m4(m: Any) = asyncId {
      m match {
        case buf: String =>
          awaitId(0)
        case buf: Double =>
          awaitId(2)
      }
    }
    m4("") mustBe 0
  }

  @Test def bugCastBoxedUnitToStringMatch() {
    import scala.async.internal.AsyncId.{asyncId, awaitId}
    def foo = asyncId {
      val p2 = awaitId(5)
      "foo" match {
        case p3: String =>
          p2.toString
      }
    }
    foo mustBe "5"
  }

  @Test def bugCastBoxedUnitToStringIf() {
    import scala.async.internal.AsyncId.{asyncId, awaitId}
    def foo = asyncId {
      val p2 = awaitId(5)
      if (true) p2.toString else p2.toString
    }
    foo mustBe "5"
  }
}
