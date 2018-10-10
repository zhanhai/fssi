package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait NominateHelper[F[_]] extends BaseProgram[F] with BallotBridge[F] {
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
    import nominateStore._

    for {
      leadersOpt <- getRoundLeaders(nodeId, slotIndex, round)
      cachedLeaders <- _if(leadersOpt.isDefined, leadersOpt.get) {
        for {
          _ <- info(s"find round leaders: nodeId = $nodeId, slotIndex = $slotIndex, round = $round")
          slices <- getSlices(nodeId).get(
            new RuntimeException(s"No Quorum Slices Found for $nodeId"))
          _             <- debug(s"slices for $nodeId is $slices")
          withoutNodeId <- delete(slices, nodeId)
          simplified    <- simplify(withoutNodeId)
          _             <- debug(s"normalized slices for $nodeId is $slices")
          initNodesAndPriority = priorityOfLocal(nodeId,
                                                 slotIndex,
                                                 round,
                                                 previousValue,
                                                 simplified)
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
          _ <- updateRoundLeaders(leaders, nodeId, slotIndex, round)
        } yield leaders
      }
    } yield cachedLeaders

  }

  def isLeader(nodeId: NodeID, leaders: Set[NodeID]): SP[F, Boolean] =
    leaders.exists(_ === nodeId).pureSP[F]

  def newValueFromNomination[A <: Value](nom: Message.Nominate[A]): SP[F, Option[Value]] = {
    // through the iteration, current accumulated value is always higher than previous accumulated value
    // after that, we get the highest value
    import valueService._
    for {
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
  }

  def votesFromLeaders(leaders: Set[NodeID]): SP[F, Set[Value]] = {
    import nominateStore._
    import valueService._

    // votes from leader, check validity and only use valid value
    def votesFromLeader(leader: NodeID): SP[F, Option[Value]] =
      for {
        nom <- getLatestNomination(leader).getOrElse(Message.Nominate.empty[Value])
        v   <- newValueFromNomination(nom)
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
      _ <- processNominationEnvelope(nodeId, slotIndex, nominationEnvelop)
        .assert(_.isValid, new RuntimeException("NominationEnvelop locally processing failed"))
      lastEnvelope <- getLastEmittedEnvelope(nodeId)
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

  def processNominationEnvelope(nodeId: NodeID,
                                slotIndex: BigInt,
                                envelope: Envelope): SP[F, Envelope.State] = {

    import nominateStore._
    import nominateService._
    import ballotStore._
    import valueService._
    for {
      lastNom <- getLatestNomination[Value](nodeId)
      sane    <- isSane(envelope.statement.message)
      isOldMessage = (lastNom.isDefined && lastNom.get.isNewerThan(envelope.statement.message))
      state <- _if(isOldMessage || !sane, Envelope.invalidState) {
        for {
          _             <- updateLatestNomination(envelope.statement.message)
          notNominating <- isNotNominating(nodeId, slotIndex)
          state0 <- _if(notNominating, Envelope.validState) {
            for {
              round              <- getCurrentRound(nodeId, slotIndex)
              promotedAccepted   <- promoteVotesToAccepted()
              promotedCandidates <- promoteAcceptedToCandidates()
              votedFromLeaders <- tryVoteFromLeaders(nodeId,
                                                     slotIndex,
                                                     round,
                                                     envelope.statement.message)
              modified = promotedAccepted || promotedCandidates || votedFromLeaders
              _ <- __ifThen(modified) {
                for {
                  round <- getCurrentRound(nodeId, slotIndex)
                  _     <- emitNomination(nodeId, slotIndex, round)
                } yield ()
              }

              currentBallot <- getCurrentBallot[Value](nodeId, slotIndex)
              _ <- __ifThen(promotedCandidates) {
                for {
                  candidates         <- getCandidates(nodeId, slotIndex, round)
                  compositeCandidate <- combineValues(candidates)
                  _                  <- updateLatestCompositeCandidate(nodeId, slotIndex, compositeCandidate)
                  _ <- __ifThen(currentBallot.isEmpty)(
                    bumpBallotState(nodeId, slotIndex, compositeCandidate))
                } yield ()
              }
            } yield Envelope.validState
          }
        } yield state0
      }
    } yield state
  }

  // federated voting to promote
  def promoteVotesToAccepted(): SP[F, Boolean]      = ???
  def promoteAcceptedToCandidates(): SP[F, Boolean] = ???

  def tryVoteFromLeaders[A <: Value](nodeId: NodeID,
                                     slotIndex: BigInt,
                                     round: Int,
                                     message: Message): SP[F, Boolean] = {
    import nominateStore._
    message match {
      case nom @ Message.Nominate(_, _) =>
        for {
          candidates <- getCandidates(nodeId, slotIndex, round)
          leadersOpt <- getRoundLeaders(nodeId, slotIndex, round)
          leader     <- _if(leadersOpt.isEmpty, false)(isLeader(nodeId, leadersOpt.get))
          wannaTry = candidates.isEmpty && leader
          result <- _if(!wannaTry, false) {
            // let's try
            for {
              newValue <- newValueFromNomination(nom)
              voted <- _if(newValue.isEmpty, false) {
                for {
                  newVotes <- vote(newValue.get)
                  _        <- updateVotes(newVotes, nodeId, slotIndex, round)
                } yield true
              }
            } yield voted
          }
        } yield result

      case _ => false.pureSP[F]
    }

  }

}

object NominateHelper {
  def apply[F[_]](implicit M: components.Model[F]): NominateHelper[F] = new NominateHelper[F] {
    val model = M

    /** bump candidates to ballot protocol
      * **Notice** This is only for test.
      */
    def bumpBallotState(nodeId: NodeID, slotIndex: BigInt, value: Value): SP[F, Unit] = {
      ().pureSP[F]
    }
  }
}
