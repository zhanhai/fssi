package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait BaseProgram[F[_]] {
  protected val model: components.Model[F]

  protected def _if[A](cond: Boolean, right: => A)(left: => SP[F, A]): SP[F, A] = {
    if (cond) right.pureSP[F]
    else left
  }

  protected def _if[A](cond: SP[F, Boolean], right: => SP[F, A])(left: => SP[F, A]): SP[F, A] = {
    for {
      c <- cond
      r <- if (c) right else left
    } yield r
  }

  protected def __if(cond: Boolean)(sp: => SP[F, Unit]): SP[F, Unit]        = _if(cond, ())(sp)
  protected def __ifThen(cond: Boolean)(_then: => SP[F, Unit]): SP[F, Unit] = __if(!cond)(_then)

  protected def updateAndGetSlices(nodeId: NodeID, quorumSet: QuorumSet): SP[F, Option[Slices]] = {
    import model.slicesStore._
    for {
      _         <- updateQuorumSet(nodeId, quorumSet)
      slicesOpt <- getSlices(nodeId)
    } yield slicesOpt
  }

  protected def federatedAccepted(votedNodes: => SP[F, Set[NodeID]],
                                  acceptedNodes: => SP[F, Set[NodeID]],
                                  slices: Slices): SP[F, Boolean] = {
    import model.nodeService._
    for {
      accepted    <- acceptedNodes
      byVBlocking <- isVBlocking(accepted, slices)
      result <- _if(byVBlocking, true) {
        for {
          votes    <- votedNodes
          byQuorum <- isQuorum(votes ++ accepted, slices)
        } yield byQuorum
      }
    } yield result
  }

  protected def federatedRatified(acceptedNodes: => SP[F, Set[NodeID]],
                                  slices: Slices): SP[F, Boolean] = {
    import model.nodeService._
    for {
      accepted       <- acceptedNodes
      quorumAccepted <- isQuorum(accepted, slices)
    } yield quorumAccepted
  }

  protected implicit final class SPOptionOps[A](sp: SP[F, Option[A]]) {
    def get(ex: Exception): SP[F, A] =
      for {
        opt <- sp
        a <- model.err.either(
          Either.cond(opt.isDefined, opt.get, ex)
        )
      } yield a

    def getOrElse(default: => A): SP[F, A] =
      for {
        opt <- sp
        r   <- if (opt.isDefined) opt.get.pureSP[F] else default.pureSP[F]
      } yield r
  }

  protected implicit def toSPOptionOps[A](p: P[F, Option[A]]): SPOptionOps[A] =
    new SPOptionOps(p: SP[F, Option[A]])

  protected implicit def __pureSP[A](a: A): SP[F, A] = a.pureSP[F]

  protected implicit final class SCAssertOps[A](sp: SP[F, A]) {
    def assert(p: A => Boolean, ex: Exception): SP[F, A] =
      for {
        a <- sp
        _ <- model.err.either(Either.cond(p(a), (), ex))
      } yield a
  }

  protected implicit def toSCAssertOps[A](p: P[F, A]): SCAssertOps[A] =
    new SCAssertOps(p: SP[F, A])

  protected implicit final class SPEitherOps[A, E <: Throwable](x: SP[F, Either[E, A]]) {
    def right: SP[F, A] =
      for {
        ax <- x
        a  <- model.err.either(ax)
      } yield a
  }
  protected implicit def toSPEitherOps[A, E <: Throwable](
      x: P[F, Either[E, A]]): SPEitherOps[A, E] =
    new SPEitherOps(x: SP[F, Either[E, A]])

}
