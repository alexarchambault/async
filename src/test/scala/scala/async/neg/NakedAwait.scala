/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package neg

import org.junit.Test

class NakedAwait {
  @Test
  def `await only allowed in async neg`() {
    expectError("`await` must be enclosed in an `async` block") {
      """
        | import _root_.scala.async.Async._
        | await[Any](null)
      """.stripMargin
    }
  }

  @Test
  def `await not allowed in by-name argument`() {
    expectError("await must not be used under a by-name argument.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | def foo(a: Int)(b: => Int) = 0
        | asyncId { foo(0)(awaitId(0)) }
      """.stripMargin
    }
  }

  @Test
  def `await not allowed in boolean short circuit argument 1`() {
    expectError("await must not be used under a by-name argument.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { true && awaitId(false) }
      """.stripMargin
    }
  }

  @Test
  def `await not allowed in boolean short circuit argument 2`() {
    expectError("await must not be used under a by-name argument.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { true || awaitId(false) }
      """.stripMargin
    }
  }

  @Test
  def nestedObject() {
    expectError("await must not be used under a nested object.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { object Nested { awaitId(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedTrait() {
    expectError("await must not be used under a nested trait.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { trait Nested { awaitId(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedClass() {
    expectError("await must not be used under a nested class.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { class Nested { awaitId(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedFunction() {
    expectError("await must not be used under a nested function.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { () => { awaitId(false) } }
      """.stripMargin
    }
  }

  @Test
  def nestedPatMatFunction() {
    expectError("await must not be used under a nested class.") { // TODO more specific error message
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { { case x => { awaitId(false) } } : PartialFunction[Any, Any] }
      """.stripMargin
    }
  }

  @Test
  def tryBody() {
    expectError("await must not be used under a try/catch.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { try { awaitId(false) } catch { case _ => } }
      """.stripMargin
    }
  }

  @Test
  def catchBody() {
    expectError("await must not be used under a try/catch.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { try { () } catch { case _ => awaitId(false) } }
      """.stripMargin
    }
  }

  @Test
  def finallyBody() {
    expectError("await must not be used under a try/catch.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { try { () } finally { awaitId(false) } }
      """.stripMargin
    }
  }

  @Test
  def guard() {
    expectError("await must not be used under a pattern guard.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { 1 match { case _ if awaitId(true) => } }
      """.stripMargin
    }
  }

  @Test
  def nestedMethod() {
    expectError("await must not be used under a nested method.") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | asyncId { def foo = awaitId(false) }
      """.stripMargin
    }
  }

  @Test
  def returnIllegal() {
    expectError("return is illegal") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | def foo(): Any = asyncId { return false }
        | ()
        |
        |""".stripMargin
    }
  }

  @Test
  def lazyValIllegal() {
    expectError("await must not be used under a lazy val initializer") {
      """
        | import _root_.scala.async.internal.AsyncId._
        | def foo(): Any = asyncId { val x = { lazy val y = awaitId(0); y } }
        | ()
        |
        |""".stripMargin
    }
  }
}
