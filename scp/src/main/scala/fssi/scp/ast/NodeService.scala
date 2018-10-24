package fssi.scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import scala.collection.immutable._

import types._
@sp trait NodeService[F[_]] {

  /** compute next round timeout (in ms)
    */
  def computeTimeout(round: Int): P[F, Long]

  /** check if in-nominating and no any candidate produced
    */
  def canNominateNewValue(nodeId: NodeID, slotIndex: SlotIndex): P[F, Boolean]
  def cannotNominateNewValue(nodeId: NodeID, slotIndex: SlotIndex): P[F, Boolean] =
    canNominateNewValue(nodeId, slotIndex).map(!_)

  /** check if nominating is stopped
    */
  def isNominatingStopped(nodeId: NodeID, slotIndex: SlotIndex): P[F, Boolean]

  /** compute a value's hash
    */
  def hashValue(slotIndex: SlotIndex, previousValue: Value, round: Int, value: Value): P[F, Long]

  /** stop nomination process
    */
  def stopNominating(nodeId: NodeID, slotIndex: SlotIndex): P[F, Unit]

  /** do some rate-limits stuff to narrow down the nominating votes
    */
  def narrowDownVotes(nodeId: NodeID,
                      slotIndex: SlotIndex,
                      values: ValueSet,
                      previousValue: Value): P[F, ValueSet]

  /** create nomination message based on local state
    */
  def createNominationMessage(nodeId: NodeID, slotIndex: SlotIndex): P[F, Message.Nomination]

  /** create ballot message based on local state
    */
  def createBallotMessage(nodeId: NodeID, slotIndex: SlotIndex): P[F, Message.BallotMessage]

  /** make a envelope for a message
    */
  def putInEnvelope[M <: Message](nodeId: NodeID, message: M): P[F, Envelope[M]]

  /** broadcast message envelope
    */
  def broadcastEnvelope[M <: Message](nodeId: NodeID, envelope: Envelope[M]): P[F, Unit]

  /** verify the signature of the envelope
    */
  def isSignatureVerified[M <: Message](envelope: Envelope[M]): P[F, Boolean]
  def isSignatureTampered[M <: Message](envelope: Envelope[M]): P[F, Boolean] =
    isSignatureVerified(envelope).map(!_)

  /** check the statement to see if it is illegal
    */
  def isStatementValid[M <: Message](statement: Statement[M]): P[F, Boolean]
  def isStatementInvalid[M <: Message](statement: Statement[M]): P[F, Boolean] =
    isStatementValid(statement).map(!_)

  /** check the message to see if it's sane
    */
  def isMessageSane(message: Message): P[F, Boolean]

  /** check a node set to see if they can construct a quorum for a node (configured quorum slices)
    */
  def isQuorum(nodeId: NodeID, nodes: Set[NodeID]): P[F, Boolean]

  /** check a node set to see if they can construct a vblocking set for a node (configured quorum slices)
    */
  def isVBlocking(nodeId: NodeID, nodes: Set[NodeID]): P[F, Boolean]

  /** get values from a ballot message
    */
  def valuesFromBallotMessage(msg: Message.BallotMessage): P[F, ValueSet]

  /** check a ballot can be used as a prepared candidate based on local p , p' and phase
    */
  def canBallotBePrepared(nodeId: NodeID, slotIndex: SlotIndex, ballot: Ballot): P[F, Boolean]
  def ballotCannotBePrepared(nodeId: NodeID, slotIndex: SlotIndex, ballot: Ballot): P[F, Boolean] =
    canBallotBePrepared(nodeId, slotIndex, ballot).map(!_)

  /** check a ballot can be potentially raise h, be confirmed prepared, to a commit
    * @see BallotProtocol.cpp#937-938
    */
  def canBallotBeHighestCommitPotentially(nodeId: NodeID,
                                          slotIndex: SlotIndex,
                                          ballot: Ballot): P[F, Boolean]

  /** check a ballot can be potentially raise h, be confirmed prepared, to a commit
    * @see BallotProtocol.cpp#970-973, 975-976 b should be compatible with newH
    */
  def canBallotBeLowestCommitPotentially(nodeId: NodeID,
                                         slotIndex: SlotIndex,
                                         b: Ballot,
                                         newH: Ballot): P[F, Boolean]

  /** check if it's necessary to set `c` based on a new `h`
    * @see BallotProtocol.cpp#961
    */
  def needSetLowestCommitBallotUnderHigh(nodeId: NodeID,
                                         slotIndex: SlotIndex,
                                         high: Ballot): P[F, Boolean]
  def notNecessarySetLowestCommitBallotUnderHigh(nodeId: NodeID,
                                                 slotIndex: SlotIndex,
                                                 high: Ballot): P[F, Boolean] =
    needSetLowestCommitBallotUnderHigh(nodeId, slotIndex, high).map(!_)
}
