package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

@sp trait MessageService[F[_]] {
  /** create nominate message envelope
    */
  def createNominationEnvelop[A <: Value](nodeId: NodeID,
                                          slotIndex: BigInt,
                                          quorumSet: QuorumSet,
                                          votes: Set[A],
                                          accepted: Set[A]): P[F, Envelope]

  def createBallotEnvelope(nodeId: NodeID, slotIndex: BigInt, quorumSet: QuorumSet, message: Message): P[F, Envelope]

  def emitEnvelope(envelope: Envelope): P[F, Unit]

}
