package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait NominateProgram[F[_]] extends BaseProgram[F] with NominateHelper[F] {
  import model._

  /** some node (identified by a NodeID) nominate a value for the slot indexed by slotIndex on n-th round,
    * based on the value of previous slot.
    * @return whether or not having nominated a value (vote(a))
    */
  def nominate(nodeId: NodeID,
               slotIndex: BigInt,
               value: Value,
               previousValue: Value): SP[F, Boolean] = {

    // 1. nominate a value means vote(a), a should be valid (from application's point of view)
    // 2. but, the value to be voted should come from the leader of this round, so we should find the current leaders of the round
    // 3. after found leaders, we vote the value of the leader's voted(if `nodeId` is the leader, then vote `value`)
    // 4. then emit the new nominating value and set a timer for next nominating.
    // 5. notice: if the selected value has been voted, we ignore this nominating.
    import nominateStore._
    for {
      round    <- getCurrentRound(nodeId, slotIndex)
      leaders  <- roundLeaders(nodeId, slotIndex, round, previousValue)
      votes    <- getVotes(nodeId, slotIndex, round)
      leader   <- isLeader(nodeId, leaders)
      newVotes <- if (leader) vote(value) else votesFromLeaders(leaders)
      _        <- updateVotes(newVotes, nodeId, slotIndex, round)
      voted    <- hasVotedNewValue(votes, nodeId, slotIndex, round)
      _        <- setupNominateTimer(nodeId, slotIndex, value, previousValue) // SHOULD invoke renominate finally
      _ <- __if(voted) {
        for {
          _ <- emitNomination(nodeId, slotIndex, round)
        } yield ()
      }
    } yield voted
  }

  def renominate(nodeId: NodeID,
                 slotIndex: BigInt,
                 value: Value,
                 previousValue: Value): SP[F, Boolean] = {
    import nominateStore._
    _if(isNotNominating(nodeId, slotIndex), false) {
      for {
        _ <- gotoNextRound(nodeId, slotIndex)
        r <- nominate(nodeId, slotIndex, value, previousValue)
      } yield r
    }
  }
}
