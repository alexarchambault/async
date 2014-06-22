/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package neg

import org.junit.Test
import scala.async.internal.AsyncId

class LocalClasses0Spec {
  @Test
  def localClassCrashIssue16() {
    import AsyncId.{asyncId, awaitId}
    asyncId {
      class B { def f = 1 }
      awaitId(new B()).f
    } mustBe 1
  }

  @Test
  def nestedCaseClassAndModuleAllowed() {
    import AsyncId.{awaitId, asyncId}
    asyncId {
      trait Base { def base = 0}
      awaitId(0)
      case class Person(name: String) extends Base
      val fut = asyncId { "bob" }
      val x = Person(awaitId(fut))
      x.base
      x.name
    } mustBe "bob"
  }
}
