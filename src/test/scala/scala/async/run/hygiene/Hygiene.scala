/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package hygiene

import org.junit.Test
import scala.async.internal.AsyncId

class HygieneSpec {

  import AsyncId.{asyncId, awaitId}

  @Test
  def `is hygenic`() {
    val state = 23
    val result: Any = "result"
    def resume(): Any = "resume"
    val res = asyncId {
      val f1 = state + 2
      val x  = awaitId(f1)
      val y  = awaitId(result)
      val z  = awaitId(resume())
      (x, y, z)
    }
    res mustBe ((25, "result", "resume"))
  }

  @Test
  def `external var as result of await`() {
    var ext = 0
    asyncId {
      ext = awaitId(12)
    }
    ext mustBe (12)
  }

  @Test
  def `external var as result of await 2`() {
    var ext = 0
    val inp = 10
    asyncId {
      if (inp > 0)
        ext = awaitId(12)
      else
        ext = awaitId(10)
    }
    ext mustBe (12)
  }

  @Test
  def `external var as result of await 3`() {
    var ext = 0
    val inp = 10
    asyncId {
      val x = if (inp > 0)
        awaitId(12)
      else
        awaitId(10)
      ext = x + awaitId(2)
    }
    ext mustBe (14)
  }

  @Test
  def `is hygenic nested`() {
    val state = 23
    val result: Any = "result"
    def resume(): Any = "resume"
    import AsyncId.{awaitId, asyncId}
    val res = asyncId {
      val f1 = asyncId { state + 2 }
      val x  = awaitId(f1)
      val y  = awaitId(asyncId { result })
      val z  = awaitId(asyncId(awaitId(asyncId { resume() })))
      (x, y, z)
    }
    res._1 mustBe (25)
    res._2 mustBe ("result")
    res._3 mustBe ("resume")
  }
}
