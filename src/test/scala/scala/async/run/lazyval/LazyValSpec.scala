/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package lazyval

import org.junit.Test
import scala.async.internal.AsyncId._

class LazyValSpec {
  @Test
  def lazyValAllowed() {
    val result = asyncId {
      var x = 0
      lazy val y = { x += 1; 42 }
      assert(x == 0, x)
      val z = awaitId(1)
      val result = y + x
      assert(x == 1, x)
      identity(y)
      assert(x == 1, x)
      result
    }
    result mustBe 43
  }
}

