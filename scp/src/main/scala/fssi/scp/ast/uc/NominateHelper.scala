package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait NominateHelper[F[_]] extends BaseProgram[F] with EnvelopeProcessProgram[F] {
  import model._

  /** calculate leaders for n-th round
    */
  def roundLeaders(nodeId: NodeID,
                   slotIndex: BigInt,
                   round: Int,
                   previousValue: Value): SP[F, Set[NodeID]] = {
    import slicesService._
    import slicesStore._
    import nodeService._
    import logService._
    for {
      _             <- info(s"find round leaders: nodeId = $nodeId, slotIndex = $slotIndex, round = $round")
      slices        <- getSlices(nodeId).get(new RuntimeException(s"No Quorum Slices Found for $nodeId"))
      _             <- debug(s"slices for $nodeId is $slices")
      withoutNodeId <- delete(slices, nodeId)
      simplified    <- simplify(withoutNodeId)
      _             <- debug(s"normalized slices for $nodeId is $slices")
      initNodesAndPriority = priorityOfLocal(nodeId, slotIndex, round, previousValue, simplified)
        .map(x => (Set(nodeId), x)): SP[F, (Set[NodeID], Long)]
      finalNodesAndPriority <- simplified.allNodes.foldLeft(initNodesAndPriority) { (acc, n) =>
        for {
          pre <- acc
          cur <- priorityOfPeer(n, slotIndex, round, previousValue, simplified).map(x => (n, x))
        } yield {
          if (cur._2 > pre._2) (Set.empty[NodeID], cur._2)
          else if (cur._2 == pre._2 && cur._2 > 0) (pre._1 + n, pre._2)
          else pre
        }
      }
      leaders = finalNodesAndPriority._1
      _ <- info(s"found leaders: $leaders")
    } yield leaders
  }

  def isLeader(nodeId: NodeID, leaders: Set[NodeID]): SP[F, Boolean] =
    leaders.exists(_ === nodeId).pureSP[F]

  def votesFromLeaders(leaders: Set[NodeID]): SP[F, Set[Value]] = {
    import nominateStore._
    import valueService._

    // votes from leader, check validity and only use valid value
    def votesFromLeader(leader: NodeID): SP[F, Option[Value]] =
      for {
        nom <- getLatestNomination(leader).getOrElse(Message.Nominate.empty[Value])
        v <- nom.allValue.foldLeft(Option.empty[Value].pureSP[F]) { (acc, n) =>
          // through the iteration, current accumulated value is always higher than previous accumulated value
          // after that, we get the highest value
          for {
            pre <- acc
            higherOpt <- if (pre.isEmpty) extractValidValue(n): SP[F, Option[Value]]
            else
              for {
                v <- extractValidValue(n)
                h <- higherValue(v, pre)
              } yield h
          } yield higherOpt
        }
      } yield v

    // iterate leaders to collect all valid value
    leaders.foldLeft(Set.empty[Value].pureSP[F]) { (acc, n) =>
      for {
        pre <- acc
        v   <- votesFromLeader(n)
      } yield pre ++ v.toSet
    }
  }

  def vote(value: Value): SP[F, Set[Value]] = Set(value).pureSP[F]

  def emitNomination(nodeId: NodeID, slotIndex: BigInt, round: Int): SP[F, Unit] = {
    import slicesStore._
    import nominateStore._
    import messageService._

    for {
      quorumSet <- getQuorumSet(nodeId).get(
        new RuntimeException(s"No Quorum Set Found for $nodeId"))
      votes             <- getVotes(nodeId, slotIndex, round)
      accepted          <- getAccepted(nodeId, slotIndex, round)
      nominationEnvelop <- createNominationEnvelop(nodeId, slotIndex, quorumSet, votes, accepted)
      lastEnvelope      <- getLastEmittedEnvelope(nodeId)
      _ <- __if(
        lastEnvelope.isEmpty || nominationEnvelop.statement.message
          .isNewerThan(lastEnvelope.get.statement.message)) {
        for {
          _ <- updateLastEmmitedEnvelop(nodeId, nominationEnvelop)
          _ <- emitEnvelope(nominationEnvelop)
        } yield ()
      }
    } yield ()
  }

  def setupNominateTimer(nodeId: NodeID,
                         slotIndex: BigInt,
                         value: Value,
                         previousValue: Value): SP[F, Unit] = {
    import nominateService._
    import nominateStore._

    for {
      round <- getCurrentRound(nodeId, slotIndex)
      delay <- nextRoundTimeout(round)
      _     <- triggerNextRoundNominate(delay, nodeId, slotIndex, value, previousValue)
    } yield ()
  }

}

object NominateHelper {
  def apply[F[_]](implicit M: components.Model[F]): NominateHelper[F] = new NominateHelper[F] {
    val model = M
  }
}
