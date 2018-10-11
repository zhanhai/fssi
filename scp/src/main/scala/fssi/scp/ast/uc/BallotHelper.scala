package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait BallotHelper[F[_]] extends BaseProgram[F] {

  /** update local state
    */
  def updateLocalState[A <: Value](newBallot: Ballot[A], nodeId: NodeID, slotIndex: BigInt): SP[F, Boolean] = ???

  def emitCurrentStateStatement(nodeId: NodeID, slotIndex: BigInt): SP[F, Unit] = ???

  def checkHeardFromQuorum(nodeId: NodeID, slotIndex: BigInt): SP[F, Unit] = ???
}
