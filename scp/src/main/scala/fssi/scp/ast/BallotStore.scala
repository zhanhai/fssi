package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

/** save ballot protocol processing status
  */
@sp trait BallotStore[F[_]] {
  // ballot state maintainence
  // current phase, 𝜑
  def getCurrentPhase(nodeId: NodeID, slotIndex: BigInt): P[F, Ballot.Phase]                         // 𝜑
  def getCurrentBallot[A <: Value](nodeId: NodeID, slotIndex: BigInt): P[F, Option[Ballot[A]]]       // b
  def getPreparedBallot[A <: Value](nodeId: NodeID, slotIndex: BigInt): P[F, Option[Ballot[A]]]      // p
  def getPreparedPrimeBallot[A <: Value](nodeId: NodeID, slotIndex: BigInt): P[F, Option[Ballot[A]]] // p'
  def getLowestBallot[A <: Value](nodeId: NodeID, slotIndex: BigInt): P[F, Option[Ballot[A]]]        // c, commit
  def getHighestBallot[A <: Value](nodeId: NodeID, slotIndex: BigInt): P[F, Option[Ballot[A]]]       // h
  def getNextValueBallot[A <: Value](nodeId: NodeID, slotIndex: BigInt): P[F, Option[Ballot[A]]]     // z mValueOverride
  def getLatestBallotMessages(): P[F, Map[NodeID, Message]]                                          // M

  def isInExternalizePhase(nodeId: NodeID, slotIndex: BigInt): P[F, Boolean] =
    getCurrentPhase(nodeId, slotIndex).map(x =>
      (x != Ballot.preparePhase) && (x != Ballot.confirmPhase))

}
