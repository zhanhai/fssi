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
  protected def roundLeaders(nodeId: NodeID,
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

  protected def isLeader(nodeId: NodeID, leaders: Set[NodeID]): SP[F, Boolean] =
    leaders.exists(_ === nodeId).pureSP[F]

  protected def newValueFromNomination[A <: Value](nom: Message.Nominate[A]): SP[F, Option[Value]] = {
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

  protected def votesFromLeaders(leaders: Set[NodeID]): SP[F, Set[Value]] = {
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

  protected def vote(value: Value): SP[F, Set[Value]] = Set(value).pureSP[F]

  protected def emitNomination(nodeId: NodeID, slotIndex: BigInt, round: Int): SP[F, Unit] = {
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

  protected def setupNominateTimer(nodeId: NodeID,
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

  protected def processNominationEnvelope(nodeId: NodeID,
                                slotIndex: BigInt,
                                envelope: Envelope): SP[F, Envelope.State] = {

    import nominateStore._
    import nominateService._
    import ballotStore._
    import valueService._

    val message = envelope.statement.message
    for {
      lastNom <- getLatestNomination[Value](nodeId)
      sane    <- isSane(envelope.statement.message)
      isOldMessage = (lastNom.isDefined && lastNom.get.isNewerThan(message))
      state <- _if(isOldMessage || !sane, Envelope.invalidState) {
        for {
          _             <- updateLatestNomination(message)
          notNominating <- isNotNominating(nodeId, slotIndex)
          state0 <- _if(notNominating, Envelope.validState) {
            for {
              round              <- getCurrentRound(nodeId, slotIndex)
              promotedAccepted   <- promoteVotesToAccepted(nodeId, slotIndex, round, message)
              promotedCandidates <- promoteAcceptedToCandidates(nodeId, slotIndex, round, message)
              votedFromLeaders   <- tryVoteFromLeaders(nodeId, slotIndex, round, message)
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
                    bumpBallotState(nodeId, slotIndex, compositeCandidate, 1).map(_ => ())) //init ballot, counter started from 1
                } yield ()
              }
            } yield Envelope.validState
          }
        } yield state0
      }
    } yield state
  }

  // federated voting to promote
  protected def promoteVotesToAccepted(nodeId: NodeID,
                             slotIndex: BigInt,
                             round: Int,
                             message: Message): SP[F, Boolean] = {

    import nominateStore._
    import slicesStore._
    import valueService._

    // two ways to accept(a)
    // 1. if quorum accept or vote a
    // 2. if vblocking set accept a

    def votedNodes(v: Value,
                   nominations: Map[NodeID, Message.Nominate[Value]]): SP[F, Set[NodeID]] =
      nominations.filter {
        case (_, nom) => nom.voted.contains(v)
      }.keySet

    def acceptedNodes(v: Value,
                      nominations: Map[NodeID, Message.Nominate[Value]]): SP[F, Set[NodeID]] =
      nominations.filter {
        case (_, nom) => nom.accepted.contains(v)
      }.keySet

    def promoteValueToBeAccepted(value: Value, slices: Slices): SP[F, Boolean] = {
      for {
        hasAccepted <- getAccepted(nodeId, slotIndex, round)
        result <- _if(hasAccepted.contains(value), false) {
          for {
            nominations <- getLatestNominations[Value]()
            accepted <- federatedAccepted(votedNodes(value, nominations),
                                          acceptedNodes(value, nominations),
                                          slices)
            promoted <- _if(!accepted, false)(
              _if(isFullValidValue(value), for {
                _ <- updateVotes(Set(value), nodeId, slotIndex, round)
                _ <- updateAccepted(Set(value), nodeId, slotIndex, round)
              } yield true) {
                for {
                  toVote <- extractValidValue(value)
                  modified <- _if(toVote.isEmpty, false)(
                    updateVotes(Set(toVote.get), nodeId, slotIndex, round))
                } yield modified
              }
            )
          } yield promoted
        }
      } yield result
    }

    message match {
      case nom @ Message.Nominate(_, _) =>
        for {
          slices <- getSlices(nodeId).get(new RuntimeException(s"No Slices Found of $nodeId"))
          result <- nom.voted.foldLeft(false.pureSP[F]) { (acc, n) =>
            for {
              pre      <- acc
              promoted <- promoteValueToBeAccepted(n, slices)
            } yield pre || promoted
          }
        } yield result

      case _ => false
    }

  }

  protected def promoteAcceptedToCandidates(nodeId: NodeID,
                                  slotIndex: BigInt,
                                  round: Int,
                                  message: Message): SP[F, Boolean] = {

    import nominateStore._
    import slicesStore._
    import valueService._

    def acceptedNodes(v: Value,
                      nominations: Map[NodeID, Message.Nominate[Value]]): SP[F, Set[NodeID]] =
      nominations.filter {
        case (_, nom) => nom.accepted.contains(v)
      }.keySet

    def promoteValueToBeCandidates(value: Value, slices: Slices): SP[F, Boolean] = {

      for {
        hasCandidated <- getCandidates(nodeId, slotIndex, round)
        result <- _if(hasCandidated.contains(value), false) {
          for {
            nominations <- getLatestNominations[Value]()
            ratified    <- federatedRatified(acceptedNodes(value, nominations), slices)
            modified <- _if(!ratified, false) {
              for {
                _ <- updateCandidates(Set(value), nodeId, slotIndex, round)
              } yield true
            }
          } yield modified
        }
      } yield result
    }

    message match {
      case nom @ Message.Nominate(_, _) =>
        for {
          slices <- getSlices(nodeId).get(new RuntimeException(s"No Slices Found of $nodeId"))
          result <- nom.voted.foldLeft(false.pureSP[F]) { (acc, n) =>
            for {
              pre      <- acc
              promoted <- promoteValueToBeCandidates(n, slices)
            } yield pre || promoted
          }
        } yield result

      case _ => false
    }
  }

  // if current candidates is empty, and node is a leader, then
  // vote from peer's message
  protected def tryVoteFromLeaders[A <: Value](nodeId: NodeID,
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

      case _ => false
    }

  }

}

object NominateHelper {
  def apply[F[_]](implicit M: components.Model[F]): NominateHelper[F] = new NominateHelper[F] {
    val model = M

    /** bump candidates to ballot protocol
      * **Notice** This is only for test.
      */
    def bumpBallotState(nodeId: NodeID, slotIndex: BigInt, value: Value, count: Int): SP[F, Boolean] = {
      true
    }
  }
}
