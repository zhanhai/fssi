package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait BaseProgram[F[_]] {
  val model: components.Model[F]

  def _if[A](cond: Boolean, right: => A)(left: => SP[F, A]): SP[F, A] = {
    if(cond) right.pureSP[F]
    else left
  }

  def _if[A](cond: SP[F, Boolean], right: => SP[F, A])(left: => SP[F, A]): SP[F, A] = {
    for {
      c <- cond
      r <- if(c) right else left
    } yield r
  }

  def __if(cond: Boolean)(sp: => SP[F, Unit]): SP[F, Unit] = _if(cond, ())(sp)

  implicit final class SPOptionOps[A](sp: SP[F, Option[A]]) {
    def get(ex: Exception): SP[F, A] = for {
      opt <- sp
      a <- model.err.either(
        Either.cond(opt.isDefined, opt.get, ex)
      )
    } yield a

    def getOrElse(default: => A): SP[F, A] = for {
      opt <- sp
      r <- if(opt.isDefined) opt.get.pureSP[F] else default.pureSP[F]
    } yield r
  }

  implicit def toSPOptionOps[A](p: P[F, Option[A]]): SPOptionOps[A] =
    new SPOptionOps(p: SP[F, Option[A]])

  implicit def __pureSP[A](a: A): SP[F, A] = a.pureSP[F]
}
