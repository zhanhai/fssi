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

  /** emit ballot envelope based current ballot state
    */
  protected def emitCurrentStateStatement(nodeId: NodeID, slotIndex: BigInt): SP[F, Unit] = {
    import messageService._
    import ballotStore._
    import slicesStore._
    // create envelope via current phase
    // check if can emit now

    def buildMessage(phase: Ballot.Phase): SP[F, Message] = phase match {
      case Ballot.Phase.Prepare     => buildPrepareMessage[Value]().map(_.asInstanceOf[Message])
      case Ballot.Phase.Confirm     => buildConfirmMessage[Value]().map(_.asInstanceOf[Message])
      case Ballot.Phase.Externalize => buildExternalizeMessage[Value]().map(_.asInstanceOf[Message])
    }

    for {
      phase                <- getCurrentPhase(nodeId, slotIndex)
      message              <- buildMessage(phase)
      quorumSet            <- getQuorumSet(nodeId).get(new RuntimeException(s"No QuorumSet of $nodeId Found"))
      envelope             <- createBallotEnvelope(nodeId, slotIndex, quorumSet, message)
      processedValid       <- processBallotEnvelopeLocally(nodeId, slotIndex, envelope).map(_.isValid)
      hasCurrentBallot     <- getCurrentBallot[Value](nodeId, slotIndex).map(_.isDefined)
      latestBallotMessage  <- getLatestBallotMessage(nodeId)
      latestEmittedMessage <- getLastEmittedEnvelope(nodeId).map(_.map(_.statement.message))
      needEmit = processedValid && hasCurrentBallot && (!latestBallotMessage.contains(message)) &&
        (latestEmittedMessage.isEmpty || message.isNewerThan(latestEmittedMessage.get))
      _ <- __ifThen(needEmit) {
        for {
          _ <- updateLatestBallotMessage(nodeId, message)
          _ <- updateLastEmmitedEnvelop(nodeId, envelope)
          _ <- emitEnvelope(envelope)
        } yield ()
      }
    } yield ()
  }

  /** check heard from quorum
    * if quorum has been advanced ahead of local, we'll abandon current ballot (neutraliztion)
    * and then, goto next ballot (counter + 1)
    * this situation often would hanppened when stucked
    */
  protected def checkHeardFromQuorum(nodeId: NodeID, slotIndex: BigInt): SP[F, Unit] = {
    import ballotStore._
    import slicesStore._
    import nodeService._
    import ballotService._

    def advancedFilter[A <: Value](currentBallot: Ballot[A]): ((NodeID, Message)) => Boolean = {
      case (_, Message.Prepare(b, _, _, _, _)) => b.counter >= currentBallot.counter
      case (_, Message.Nominate(_, _))         => false // will never happen
      case _                                   => true
    }

    def nodesOfQuorumForAll(nodes: Set[NodeID]): SP[F, Set[NodeID]] = {

      // if current nodes can't satisfy the isQuorum(v) then remove the `v`(v is a element of node set)
      def _filter(nodes: Set[NodeID]): SP[F, Set[NodeID]] =
        nodes.foldLeft(Set.empty[NodeID].pureSP[F]) { (acc, n) =>
          for {
            pre    <- acc
            slices <- getSlices(n).get(new RuntimeException(s"No Slices of $n Found"))
            quorum <- isQuorum(nodes, slices)
          } yield if (quorum) (pre + n) else pre
        }

      // we should loop filter, because it's possible no to satisfy isQuorum(v) when some nodes removed
      def loop(nodes: Set[NodeID]): SP[F, Set[NodeID]] =
        for {
          remains <- _filter(nodes)
          result  <- _if(remains.size == nodes.size, nodes)(loop(remains))
        } yield result

      loop(nodes)
    }

    for {
      currentBallot <- getCurrentBallot[Value](nodeId, slotIndex)
      _ <- __ifThen(currentBallot.isDefined) {
        for {
          ballotMessages <- getLatestBallotMessages()
          advancedNodes = ballotMessages
            .filter(advancedFilter[Value](currentBallot.get))
            .map(_._1) //_1 is the NodeID
          nodes  <- nodesOfQuorumForAll(advancedNodes.toSet)
          slices <- getSlices(nodeId).get(new RuntimeException(s"No QuorumSet of $nodeId Found"))
          _ <- _if(isNotQuorum(nodes, slices), for {
            _ <- updateHeardFromQuorum(nodeId, slotIndex, false)
            _ <- stopBallotProtocolTimer()
          } yield ()) {
            //isQuorum
            for {
              heardEver <- getHeardFromQuorum(nodeId, slotIndex)
              _         <- updateHeardFromQuorum(nodeId, slotIndex, true)
              phase     <- getCurrentPhase(nodeId, slotIndex)
              _ <- __ifThen(!heardEver && phase != Ballot.externalizePhase)(
                startBallotProtocolTimer())
              _ <- __ifThen(phase == Ballot.externalizePhase)(stopBallotProtocolTimer())
            } yield ()
          }
        } yield ()
      }
    } yield ()
  }

  /** process ballot envelope
    */
  protected def processBallotEnvelope(nodeId: NodeID,
                                      slotIndex: BigInt,
                                      envelope: Envelope): SP[F, Envelope.State] = {
    import ballotStore._
    for {
      valid <- checkValidity(envelope, nodeId, slotIndex)
      state <- _if(!valid, Envelope.invalidState) {
        for {
          phase            <- getCurrentPhase(nodeId, slotIndex)
          phaseHandleState <- handleEnvelopeOnPhase(nodeId, slotIndex, phase, envelope)
        } yield phaseHandleState
      }
    } yield state
  }

  /** process ballot envelope
    */
  protected def processBallotEnvelopeLocally(nodeId: NodeID,
                                             slotIndex: BigInt,
                                             envelope: Envelope): SP[F, Envelope.State] = ???

  // a node(nodeId) check the validity of an ballot envelope which was received from peer nodes or self
  private def checkValidity(envelope: Envelope,
                            nodeId: NodeID,
                            slotIndex: BigInt): SP[F, Boolean] = {
    import messageService._
    import slicesStore._
    import slicesService._
    import ballotStore._

    _if(slotIndex != envelope.statement.slotIndex, false) {
      _if(hasBeenTampered(envelope), false) {
        for {
          slicesOfPeer <- updateAndGetSlices(envelope.statement.nodeId,
                                             envelope.statement.quorumSet)
          slicesOfPeerSane <- _if(slicesOfPeer.isEmpty, false)(isSlicesSane(slicesOfPeer.get))
          finalSanity <- _if(!slicesOfPeerSane, false) {
            for {
              statementSane <- isStatementSane(receiver = nodeId, envelope.statement)
              newer <- _if(!statementSane, false) {
                for {
                  latestBallotMessage <- getLatestBallotMessage(envelope.statement.nodeId)
                } yield
                  (latestBallotMessage.isEmpty || envelope.statement.message
                    .isNewerThan(latestBallotMessage.get))
              }
            } yield newer
          }
        } yield finalSanity
      }
    }

  }

  // @see papers, p24
  // Upon adding a newly received message m to Mv, a node v updates its state as follows:
  // It's not necessary to copy from `scp-core`, we can implement them just as what the paper said.
  private def handleEnvelopeOnPhase(nodeId: NodeID,
                                    slotIndex: BigInt,
                                    phase: Ballot.Phase,
                                    envelope: Envelope): SP[F, Envelope.State] = {
    import ballotStore._
    import messageService._
    phase match {
      case Ballot.Phase.Externalize =>
        for {
          c <- getCurrentCommitBallot[Value](nodeId, slotIndex)
          b <- getWorkingBallot[Value](envelope.statement.message)
          state <- _if(c.isEmpty || b.isEmpty || c != b, Envelope.invalidState) {
            for {
              _ <- updateLatestBallotMessage(envelope.statement.nodeId, envelope.statement.message)
            } yield Envelope.validState
          }
        } yield state

      case _ =>
        // warning: in stellar-core(cpp), check the value on application level to get validity level,
        //          the level turn into a flag to see if we could emit somthing.
        //          here, we ignore this feature, we do `save and advance` directly
        //          see: BallotProtocol.cpp, line 192
        for {
          _ <- updateLatestBallotMessage(envelope.statement.nodeId, envelope.statement.message)
          _ <- advanceSlot(nodeId, slotIndex, envelope)
        } yield Envelope.validState
    }
  }

  private def advanceSlot(nodeId: NodeID, slotIndex: BigInt, envelope: Envelope): SP[F, Unit] = {

    // ??? mCurrentMessageLevel
    for {
      pa <- attemptPreparedAccept(nodeId, slotIndex, envelope)
      pc <- attemptPreparedConfirmed()
      ac <- attemptAcceptCommit()
      cc <- attemptConfirmCommit()
      ba <- attemptBumpAll()
      worked = pa || pc || ac || cc || ba
      _ <- __ifThen(worked)(sendLatestEnvelope())
    } yield ()
  }

  private def sendLatestEnvelope(): SP[F, Unit] = {
    import ballotStore._
    // @see BallotProtocol.cpp line 1933
    ???
  }

  //placeholder
  // step 1-5 @paper
  private def attemptPreparedAccept(nodeId: NodeID,
                                    slotIndex: BigInt,
                                    envelope: Envelope): SP[F, Boolean] = {
    import ballotStore._
    import messageService._
    import slicesStore._

    // filter some ballots, remains what can help us to advance current state
    def helpToAdvancedFilter[A <: Value](ballots: Vector[Ballot[A]]): SP[F, Vector[Ballot[A]]] = {
      ballots.foldLeft(Vector.empty[Ballot[A]].pureSP[F]) { (acc, n) =>
        for {
          pre                    <- acc
          phase                  <- getCurrentPhase(nodeId, slotIndex)
          prepared               <- getCurrentPreparedBallot[A](nodeId, slotIndex)
          commit                 <- getCurrentCommitBallot[A](nodeId, slotIndex)
          compatibleWithPrepared <- _if(prepared.isEmpty, false)(isCompatible[A](n, prepared.get))
          greaterThanPrepared <- _if(prepared.isEmpty, false)(
            compareBallots[A](n, prepared.get).map(_ >= 0))
          compatibleWithCommit <- _if(commit.isEmpty, false)(isCompatible[A](n, commit.get))

          c1 = phase == Ballot.Phase.Confirm && (!(compatibleWithPrepared && greaterThanPrepared) || !compatibleWithCommit)

          nRemoved <- _if(c1, true) {
            for {
              preparedPrime <- getCurrentPreparedPrimeBallot[A](nodeId, slotIndex)
              greaterThanPreparedPrime <- _if(preparedPrime.isEmpty, true)(
                compareBallots(n, preparedPrime.get).map(_ >= 0))
              remvedWhenPreparedPrime <- _if(!greaterThanPreparedPrime, true) {
                // see prepare when not on Phase.Confirm
                _if(prepared.isEmpty, false) {
                  for {
                    c    <- isCompatible[A](n, prepared.get)
                    less <- compareBallots[A](n, prepared.get).map(_ <= 0)
                  } yield c && less
                }
              }
            } yield remvedWhenPreparedPrime
          }
        } yield if (nRemoved) pre else pre :+ n
      }
    }

    // see other nodes if they have voted ballot
    def nodesToVoteBallot[A <: Value](ballot: Ballot[A],
                                      messages: Map[NodeID, Message]): SP[F, Set[NodeID]] = {
      def hasVoted(message: Message): SP[F, Boolean] = message match {
        case Message.Prepare(b, _, _, _, _) =>
          for {
            less <- compareBallots(ballot, b).map(_ <= 0)
            c    <- isCompatible(ballot, b)
          } yield less && c
        case Message.Confirm(b, _, _, _) => isCompatible(ballot, b)
        case Message.Externalize(c, _)   => isCompatible(ballot, c)
        case _                           => false
      }

      messages.foldLeft(Set.empty[NodeID].pureSP[F]) { (acc, n) =>
        for {
          pre   <- acc
          voted <- hasVoted(n._2)
        } yield if (voted) pre + n._1 else pre
      }
    }

    // see other nodes if they have accepted ballot
    def nodesToAcceptBallot[A <: Value](ballot: Ballot[A],
                                        messages: Map[NodeID, Message]): SP[F, Set[NodeID]] = {

      def hasAccepted(message: Message): SP[F, Boolean] =
        message match {
          case Message.Prepare(_, prepared, preparedPrime, _, _)
              if prepared.isDefined && preparedPrime.isDefined =>
            for {
              lessThanPrepared            <- compareBallots(ballot, prepared.get).map(_ <= 0)
              lessThanPreparedPrime       <- compareBallots(ballot, preparedPrime.get).map(_ <= 0)
              compatibleWithPrepared      <- isCompatible(ballot, prepared.get)
              compatibleWithPreparedPrime <- isCompatible(ballot, preparedPrime.get)
            } yield
              lessThanPrepared && lessThanPreparedPrime && compatibleWithPrepared && compatibleWithPreparedPrime
          case Message.Confirm(b, nPrepared, _, _) =>
            val nb = b.copy(counter = nPrepared)
            for {
              lessThanPrepared       <- compareBallots(ballot, nb).map(_ <= 0)
              compatibleWithPrepared <- isCompatible(ballot, nb)
            } yield lessThanPrepared && compatibleWithPrepared
          case Message.Externalize(c, _) => isCompatible(ballot, c)
          case _                         => false
        }

      messages.foldLeft(Set.empty[NodeID].pureSP[F]) { (acc, n) =>
        for {
          pre      <- acc
          accepted <- hasAccepted(n._2)
        } yield if (accepted) pre + n._1 else pre
      }

    }

    def updateState[A <: Value](ballot: Ballot[A]): SP[F, Boolean] = ???

    for {
      phase <- getCurrentPhase(nodeId, slotIndex)
      result <- _if(phase.isExternalize, false) {
        for {
          candidatedPrepares <- getPrepareCandidates[Value](envelope.statement.message)
          filtered           <- helpToAdvancedFilter(candidatedPrepares.toVector)
          sorted             <- sortBallots[Value](filtered)
          accepted <- sorted.foldRight(false.pureSP[F]) { (n, acc) =>
            for {
              pre <- acc
              r <- _if(pre, true) {
                for {
                  messages <- getLatestBallotMessages()
                  votes    <- nodesToVoteBallot(n, messages)
                  accepted <- nodesToAcceptBallot(n, messages)
                  slices <- getSlices(nodeId).get(
                    new RuntimeException(s"No Slices of $nodeId Found"))
                  acceptedAndUpdated <- _if(federatedAccepted(votes, accepted, slices).map(!_),
                                            false)(updateState(n))
                } yield acceptedAndUpdated
              }
            } yield r
          }
        } yield accepted

      }
    } yield result
  }

  private def attemptPreparedConfirmed(): SP[F, Boolean] = ???
  private def attemptAcceptCommit(): SP[F, Boolean]      = ???
  private def attemptConfirmCommit(): SP[F, Boolean]     = ???
  private def attemptBumpAll(): SP[F, Boolean]           = ??? // @see BallotProtocol.cpp, line 1856-1867

}
