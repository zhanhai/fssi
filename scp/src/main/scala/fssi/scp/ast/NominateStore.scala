package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

/** save nominate protocol processing status
  */
@sp trait NominateStore[F[_]] {

  def getCurrentRound(nodeId: NodeID, slotIndex: BigInt): P[F, Int]

  def gotoNextRound(nodeId: NodeID, slotIndex: BigInt): P[F, Unit]

  def isNominating(nodeId: NodeID, slotIndex: BigInt): P[F, Boolean]

  def isNotNominating(nodeId: NodeID, slotIndex: BigInt): P[F, Boolean] =
    isNominating(nodeId, slotIndex).map(!_)

  def getVotes(nodeId: NodeID, slotIndex: BigInt, round: Int): P[F, Set[Value]]

  def getAccepted(nodeId: NodeID, slotIndex: BigInt, round: Int): P[F, Set[Value]]

  def getCandidates(nodeId: NodeID, slotIndex: BigInt, round: Int): P[F, Set[Value]]

  def updateVotes(newVotes: Set[Value],
                  nodeId: NodeID,
                  slotIndex: BigInt,
                  round: Int): P[F, Boolean]

  def hasVotedNewValue(oldVotes: Set[Value],
                       nodeId: NodeID,
                       slotIndex: BigInt,
                       round: Int): P[F, Boolean]

  def getLatestNomination[A <: Value](nodeId: NodeID): P[F, Option[Message.Nominate[A]]]
  def updateLatestNomination(nom: Message): P[F, Unit]

  def getLastEmittedEnvelope(nodeId: NodeID): P[F, Option[Envelope]]

  def updateLastEmmitedEnvelop(nodeId: NodeID, envelope: Envelope): P[F, Unit]

  def updateLatestCompositeCandidate(nodeId: NodeID, slotIndex: BigInt, value: Value): P[F, Unit]
  

}
