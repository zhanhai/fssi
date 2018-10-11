package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait BallotHelper[F[_]] extends BaseProgram[F] {
  import model._

  protected def emitCurrentStateStatement(nodeId: NodeID, slotIndex: BigInt): SP[F, Unit] = {
    import messageService._
    import ballotStore._
    import slicesStore._
    // create envelope via current phase
    // check if can emit now

    def buildMessage(phase: Ballot.Phase): SP[F, Message] = phase match {
      case Ballot.Phase.Prepare => buildPrepareMessage[Value]().map(_.asInstanceOf[Message])
      case Ballot.Phase.Confirm => buildConfirmMessage[Value]().map(_.asInstanceOf[Message])
      case Ballot.Phase.Externalize => buildExternalizeMessage[Value]().map(_.asInstanceOf[Message])
    }

    for {
      phase <- getCurrentPhase(nodeId, slotIndex)
      message <- buildMessage(phase)
      quorumSet <- getQuorumSet(nodeId).get(new RuntimeException(s"No QuorumSet of $nodeId Found"))
      envelope <- createBallotEnvelope(nodeId, slotIndex, quorumSet, message)
    } yield ()
    
  }

  protected def checkHeardFromQuorum(nodeId: NodeID, slotIndex: BigInt): SP[F, Unit] = ???

  /** process ballot envelope
    */
  protected def processBallotEnvelope(nodeId: NodeID,
                                      slotIndex: BigInt,
                                      envelope: Envelope): SP[F, Envelope.State] = ???
}
