/*
 * Copyright (C) 2012-2014-2013 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async
package run
package live

import org.junit.Test

import internal.AsyncTestLV
import AsyncTestLV._

case class Cell[T](v: T)

class Meter(val len: Long) extends AnyVal

case class MCell[T](var v: T)


class LiveVariablesSpec {
  AsyncTestLV.clear()

  @Test
  def `zero out fields of reference type`() {
    val f = asyncIdLV { Cell(1) }

    def m1(x: Cell[Int]): Cell[Int] =
      asyncIdLV { Cell(x.v + 1) }

    def m2(x: Cell[Int]): String =
      asyncIdLV { x.v.toString }

    def m3() = asyncIdLV {
      val a: Cell[Int] = awaitIdLV(f)      // await$1$1
      // a == Cell(1)
      val b: Cell[Int] = awaitIdLV(m1(a))  // await$2$1
      // b == Cell(2)
      assert(AsyncTestLV.log.exists(_._2 == Cell(1)), AsyncTestLV.log)
      val res = awaitIdLV(m2(b))           // await$3$1
      assert(AsyncTestLV.log.exists(_._2 == Cell(2)))
      res
    }

    assert(m3() == "2")
  }

  @Test
  def `zero out fields of type Any`() {
    val f = asyncIdLV { Cell(1) }

    def m1(x: Cell[Int]): Cell[Int] =
      asyncIdLV { Cell(x.v + 1) }

    def m2(x: Any): String =
      asyncIdLV { x.toString }

    def m3() = asyncIdLV {
      val a: Cell[Int] = awaitIdLV(f)      // await$4$1
      // a == Cell(1)
      val b: Any = awaitIdLV(m1(a))        // await$5$1
      // b == Cell(2)
      assert(AsyncTestLV.log.exists(_._2 == Cell(1)))
      val res = awaitIdLV(m2(b))           // await$6$1
      assert(AsyncTestLV.log.exists(_._2 == Cell(2)))
      res
    }

    assert(m3() == "Cell(2)")
  }

  @Test
  def `do not zero out fields of primitive type`() {
    val f = asyncIdLV { 1 }

    def m1(x: Int): Cell[Int] =
      asyncIdLV { Cell(x + 1) }

    def m2(x: Any): String =
      asyncIdLV { x.toString }

    def m3() = asyncIdLV {
      val a: Int = awaitIdLV(f)            // await$7$1
      // a == 1
      val b: Any = awaitIdLV(m1(a))        // await$8$1
      // b == Cell(2)
      // assert(!AsyncTestLV.log.exists(p => p._1 == "await$7$1"))
      val res = awaitIdLV(m2(b))           // await$9$1
      assert(AsyncTestLV.log.exists(_._2 == Cell(2)))
      res
    }

    assert(m3() == "Cell(2)")
  }

  @Test
  def `zero out fields of value class type`() {
    val f = asyncIdLV { Cell(1) }

    def m1(x: Cell[Int]): Meter =
      asyncIdLV { new Meter(x.v + 1) }

    def m2(x: Any): String =
      asyncIdLV { x.toString }

    def m3() = asyncIdLV {
      val a: Cell[Int] = awaitIdLV(f)      // await$10$1
      // a == Cell(1)
      val b: Meter = awaitIdLV(m1(a))      // await$11$1
      // b == Meter(2)
      assert(AsyncTestLV.log.exists(_._2 == Cell(1)))
      val res = awaitIdLV(m2(b.len))       // await$12$1
      assert(AsyncTestLV.log.exists(_._2.asInstanceOf[Meter].len == 2L))
      res
    }

    assert(m3() == "2")
  }

  @Test
  def `zero out fields after use in loop`() {
    val f = asyncIdLV { MCell(1) }

    def m1(x: MCell[Int], y: Int): Int =
      asyncIdLV { x.v + y }

    def m3() = asyncIdLV {
      // state #1
      val a: MCell[Int] = awaitIdLV(f)     // await$13$1
      // state #2
      var y = MCell(0)

      while (a.v < 10) {
        // state #4
        a.v = a.v + 1
        y = MCell(awaitIdLV(a).v + 1)      // await$14$1
        // state #7
      }

      // state #3
      // assert(AsyncTestLV.log.exists(entry => entry._1 == "await$14$1"))

      val b = awaitIdLV(m1(a, y.v))        // await$15$1
      // state #8
      assert(AsyncTestLV.log.exists(_._2 == MCell(10)), AsyncTestLV.log)
      assert(AsyncTestLV.log.exists(_._2 == MCell(11)))
      b
    }

    assert(m3() == 21, m3())
  }

  @Test
  def `don't zero captured fields captured lambda`() {
    val f = asyncIdLV {
      val x = "x"
      val y = "y"
      awaitIdLV(0)
      y.reverse
      val f = () => assert(x != null)
      awaitIdLV(0)
      f
    }
    AsyncTestLV.assertNotNulledOut("x")
    AsyncTestLV.assertNulledOut("y")
    f()
  }

  @Test
  def `don't zero captured fields captured by-name`() {
    def func0[A](a: => A): () => A =  () => a
    val f = asyncIdLV {
      val x = "x"
      val y = "y"
      awaitIdLV(0)
      y.reverse
      val f = func0(assert(x != null))
      awaitIdLV(0)
      f
    }
    AsyncTestLV.assertNotNulledOut("x")
    AsyncTestLV.assertNulledOut("y")
    f()
  }

  @Test
  def `don't zero captured fields nested class`() {
    def func0[A](a: => A): () => A = () => a
    val f = asyncIdLV {
      val x = "x"
      val y = "y"
      awaitIdLV(0)
      y.reverse
      val f = new Function0[Unit] {
        def apply = assert(x != null)
      }
      awaitIdLV(0)
      f
    }
    AsyncTestLV.assertNotNulledOut("x")
    AsyncTestLV.assertNulledOut("y")
    f()
  }

  @Test
  def `don't zero captured fields nested object`() {
    def func0[A](a: => A): () => A = () => a
    val f = asyncIdLV {
      val x = "x"
      val y = "y"
      awaitIdLV(0)
      y.reverse
      object f extends Function0[Unit] {
        def apply = assert(x != null)
      }
      awaitIdLV(0)
      f
    }
    AsyncTestLV.assertNotNulledOut("x")
    AsyncTestLV.assertNulledOut("y")
    f()
  }

  @Test
  def `don't zero captured fields nested def`() {
    val f = asyncIdLV {
      val x = "x"
      val y = "y"
      awaitIdLV(0)
      y.reverse
      def xx = x
      val f = xx _
      awaitIdLV(0)
      f
    }
    AsyncTestLV.assertNotNulledOut("x")
    AsyncTestLV.assertNulledOut("y")
    f()
  }

  @Test
  def `capture bug`() {
    sealed trait Base
    case class B1() extends Base
    case class B2() extends Base
    val outer = List[(Base, Int)]((B1(), 8))

    def getMore(b: Base) = 4

    def baz = asyncIdLV {
      outer.head match {
        case (a @ B1(), r) => {
          val ents = awaitIdLV(getMore(a))

          { () =>
            println(a)
            assert(a ne null)
          }
        }
        case (b @ B2(), x) =>
          () => ???
      }
    }
    baz()
  }
}
