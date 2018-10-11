package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait ProcessEnvelopeProgram[F[_]] {

  /** process nomination envelope
    */
  protected def processNominationEnvelope(nodeId: NodeID,
                                          slotIndex: BigInt,
                                          envelope: Envelope): SP[F, Envelope.State]

  /** process ballot envelope
    */
  protected def processBallotEnvelope(nodeId: NodeID,
                                      slotIndex: BigInt,
                                      envelope: Envelope): SP[F, Envelope.State]

  /** process scp envelope facade
    */
  def processEnvelope(nodeId: NodeID,
                      slotIndex: BigInt,
                      envelope: Envelope): SP[F, Envelope.State] =
    envelope.statement.message match {
      case _ @Message.Nominate(_, _) => processNominationEnvelope(nodeId, slotIndex, envelope)
      case _                         => processBallotEnvelope(nodeId, slotIndex, envelope)

    }
}
