/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.async.internal

import scala.reflect.internal.annotations.compileTimeOnly
import scala.reflect.macros.Context
import scala.reflect.api.Universe

/**
 * A base class for the `async` macro. Subclasses must provide:
 *
 * - Concrete types for a given future system
 * - Tree manipulations to create and complete the equivalent of Future and Promise
 * in that system.
 * - The `async` macro declaration itself, and a forwarder for the macro implementation.
 * (The latter is temporarily needed to workaround bug SI-6650 in the macro system)
 *
 * The default implementation, [[scala.async.Async]], binds the macro to `scala.concurrent._`.
 */
abstract class AsyncBase {
  self =>

  type FS <: FutureSystem
  val futureSystem: FS

  def awaitName: String = "await"

  def asyncImpl[T: c.WeakTypeTag](c: Context)
                                 (body: c.Expr[T])
                                 (execContext: c.Expr[futureSystem.ExecContext]): c.Expr[futureSystem.Fut[T]] = {
    import c.universe._, c.internal._, decorators._
    val asyncMacro = AsyncMacro(c, self)

    val code = asyncMacro.asyncTransform[T](body.tree, execContext.tree)(c.weakTypeTag[T])
    AsyncUtils.vprintln(s"async state machine transform expands to:\n ${code}")

    // Mark range positions for synthetic code as transparent to allow some wiggle room for overlapping ranges
    for (t <- code) t.setPos(t.pos.makeTransparent)
    c.Expr[futureSystem.Fut[T]](code)
  }

  protected[async] def awaitMethod(u: Universe)(asyncMacroSymbol: u.Symbol): u.Symbol = {
    import u._
    asyncMacroSymbol.owner.typeSignature.member(newTermName(awaitName))
  }

  protected[async] def nullOut(u: Universe)(name: u.Expr[String], v: u.Expr[Any]): u.Expr[Unit] =
    u.reify { () }
}
