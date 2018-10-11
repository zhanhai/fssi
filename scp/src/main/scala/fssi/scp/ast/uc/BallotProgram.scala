package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait BallotProgram[F[_]] extends BaseProgram[F] with BallotBridge[F] with BallotHelper[F] {
  import model._

  /** bump candidates to ballot protocol
    * but if nextValue is defined, bump nextValue
    */
  def bumpBallotState(nodeId: NodeID, slotIndex: BigInt, value: Value, counter: Int): SP[F, Boolean] = {
    import ballotStore._

    for {
      result <- _if(isInExternalizePhase(nodeId, slotIndex), false) {
        for {
          newValue <- getNextValueBallot[Value](nodeId, slotIndex).map(_.map(_.value)).getOrElse(value)
          newBallot = Ballot(counter = counter, value = newValue)
          updated <- updateLocalState[Value](newBallot, nodeId, slotIndex)
          _ <- __ifThen(updated) {
            for {
              _ <- emitCurrentStateStatement(nodeId, slotIndex)
              _ <- checkHeardFromQuorum(nodeId, slotIndex)
            } yield ()
          }
        } yield updated
      }
    }yield result
  }
}
