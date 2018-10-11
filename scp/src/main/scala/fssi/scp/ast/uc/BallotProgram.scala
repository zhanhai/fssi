package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait BallotProgram[F[_]] extends BaseProgram[F] with BallotBridge[F] with BallotHelper[F] with ProcessEnvelopeProgram[F] {
  import model._

  /** bump candidates to ballot protocol
    * but if nextValue is defined, bump nextValue
    */
  protected def bumpBallotState(nodeId: NodeID, slotIndex: BigInt, value: Value, counter: Int): SP[F, Boolean] = {
    import ballotStore._

    for {
      result <- _if(isInExternalizePhase(nodeId, slotIndex), false) {
        for {
          newValue <- getNextValueBallot[Value](nodeId, slotIndex).map(_.map(_.value)).getOrElse(value)
          newBallot = Ballot(counter = counter, value = newValue)
          need <- needUpdateBallot[Value](newBallot, nodeId, slotIndex)
          updated  <- _if(!need, false) {
            for {
              _ <- updateCurrentBallot(newBallot, nodeId, slotIndex)
              _ <- checkCurrentStateInvariants().right
              _ <- emitCurrentStateStatement(nodeId, slotIndex)
              _ <- checkHeardFromQuorum(nodeId, slotIndex)
            } yield true
          }
        } yield updated
      }
    }yield result
  }
}
