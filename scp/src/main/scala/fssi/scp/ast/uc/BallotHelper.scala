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
      pc <- attemptPreparedConfirmed(nodeId, slotIndex, envelope)
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

    /** set current ballot states, if current state changed, return true */
    def updateState[A <: Value](ballot: Ballot[A]): SP[F, Boolean] = {
      // set prepare and preparePrime
      def setPrepared(): SP[F, Boolean] =
        for {
          prepared <- getCurrentPreparedBallot[A](nodeId, slotIndex)
          comNum   <- _if(prepared.isEmpty, 1)(compareBallots[A](ballot, prepared.get))
          modified <- if (comNum > 0) for {
            c <- _if(prepared.isEmpty, false)(isCompatible(prepared.get, ballot))
            _ <- __ifThen(!c && prepared.isDefined) {
              for {
                _ <- updateCurrentPreparedPrimeBallot(nodeId, slotIndex, prepared.get)
              } yield ()
            }
            _ <- updateCurrentPreparedBallot(nodeId, slotIndex, ballot)
          } yield true
          else if (comNum < 0) for {
            preparedPrime <- getCurrentPreparedPrimeBallot[A](nodeId, slotIndex)
            comNumPrime <- _if(preparedPrime.isEmpty, 1)(
              compareBallots[A](ballot, preparedPrime.get))
            primeModified <- _if(comNum <= 0, false) {
              for {
                _ <- updateCurrentPreparedPrimeBallot(nodeId, slotIndex, ballot)
              } yield true
            }
          } yield primeModified
          else false.pureSP[F]
        } yield modified
      // check if we also need clear c (commit, lowest ballot)
      def clearCommit(): SP[F, Boolean] =
        for {
          commit   <- getCurrentCommitBallot[A](nodeId, slotIndex)
          high     <- getCurrentHighestBallot[A](nodeId, slotIndex)
          prepared <- getCurrentPreparedBallot[A](nodeId, slotIndex)
          c1 <- _if(prepared.isEmpty || high.isEmpty, false) {
            for {
              less         <- compareBallots(high.get, prepared.get).map(_ <= 0)
              incompatible <- isCompatible(high.get, prepared.get)
            } yield less && incompatible
          }
          preparedPrime <- getCurrentPreparedPrimeBallot[A](nodeId, slotIndex)
          c2 <- _if(preparedPrime.isEmpty || high.isEmpty, false) {
            for {
              less         <- compareBallots(high.get, preparedPrime.get).map(_ <= 0)
              incompatible <- isCompatible(high.get, preparedPrime.get)
            } yield less && incompatible
          }
        } yield c1 || c2

      for {
        c1 <- setPrepared()
        c2 <- clearCommit()
        modified = c1 || c2
        _ <- __ifThen(modified)(emitCurrentStateStatement(nodeId, slotIndex))
      } yield modified

    }

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

  private def attemptPreparedConfirmed(nodeId: NodeID,
                                       slotIndex: BigInt,
                                       envelope: Envelope): SP[F, Boolean] = {

    import ballotStore._
    import messageService._
    import slicesStore._

    def findNewH[A <: Value](sorted: Vector[Ballot[A]]): SP[F, Option[Ballot[A]]] = {
      def accepted(message: Message, ballot: Ballot[A]): SP[F, Boolean] = message match {
        case Message.Prepare(_, prepared, pareparedPrime, _, _) =>
          for {
            less <- compareBallots(ballot, prepared.get).map(_ <= 0)
            c    <- isCompatible(ballot, prepared.get)
          } yield less && c
        case Message.Confirm(Ballot(_, value), preparedCounter, _, _) =>
          val b = Ballot(preparedCounter, value)
          for {
            less <- compareBallots(ballot, b).map(_ <= 0)
            c    <- isCompatible(ballot, b)
          } yield less && c
        case Message.Externalize(c, _) => isCompatible(c, ballot)
        case _                         => false
      }

      def ratified(ballot: Ballot[A], messages: Map[NodeID, Message]): SP[F, Boolean] = {
        val acceptedNodes = messages.foldLeft(Set.empty[NodeID].pureSP[F]) {
          case (acc, (nodeId, message)) =>
            for {
              pre <- acc
              x   <- accepted(message, ballot)
            } yield if (x) pre + nodeId else pre
        }

        for {
          slices <- getSlices(nodeId).get(new RuntimeException(s"No Slices of $nodeId Found"))
          fa     <- federatedRatified(acceptedNodes, slices)
        } yield fa

      }
      /*
        envelope.statement.message match {
        case Message.Prepare(_, prepared, pareparedPrime, _, _) =>
          for {
            less <- compareBallots(ballot, prepared.get).map(_ <= 0)
            c <- isCompatible(ballot, prepared.get)
          } yield less && c

        case _ => false
      }*/

      if (sorted.isEmpty) Option.empty[Ballot[A]]
      else {
        // if we had a h, and h is greater than any ballots, nothing to return
        for {
          h <- getCurrentHighestBallot[A](nodeId, slotIndex)
          biggest = sorted.last
          breakOnBiggest <- _if(h.isEmpty, false)(compareBallots[A](h.get, biggest).map(_ >= 0))
          result <- _if(breakOnBiggest, Option.empty[Ballot[A]]) {
            for {
              messages <- getLatestBallotMessages()
              newBallot <- sorted.foldRight(Option.empty[Ballot[A]].pureSP[F]) { (n, acc) =>
                for {
                  pre <- acc
                  next <- _if(pre.isDefined, pre)(ratified(n, messages).map(x =>
                    if (x) Some(n) else None))
                } yield next
              }
            } yield newBallot
          }
        } yield result
      }
    }

    def findNewC[A <: Value](sorted: Vector[Ballot[A]],
                             newH: Ballot[A]): SP[F, Option[Ballot[A]]] = {
      def accepted(message: Message, ballot: Ballot[A]): SP[F, Boolean] = message match {
        case Message.Prepare(_, prepared, pareparedPrime, _, _) =>
          for {
            less <- compareBallots(ballot, prepared.get).map(_ <= 0)
            c    <- isCompatible(ballot, prepared.get)
          } yield less && c
        case Message.Confirm(Ballot(_, value), preparedCounter, _, _) =>
          val b = Ballot(preparedCounter, value)
          for {
            less <- compareBallots(ballot, b).map(_ <= 0)
            c    <- isCompatible(ballot, b)
          } yield less && c
        case Message.Externalize(c, _) => isCompatible(c, ballot)
        case _                         => false
      }
      def ratified(ballot: Ballot[A], messages: Map[NodeID, Message]): SP[F, Boolean] = {
        val acceptedNodes = messages.foldLeft(Set.empty[NodeID].pureSP[F]) {
          case (acc, (nodeId, message)) =>
            for {
              pre <- acc
              x   <- accepted(message, ballot)
            } yield if (x) pre + nodeId else pre
        }

        for {
          slices <- getSlices(nodeId).get(new RuntimeException(s"No Slices of $nodeId Found"))
          fa     <- federatedRatified(acceptedNodes, slices)
        } yield fa

      }
      // setp(3) in the paper
      // If𝜑=PREPARE, 𝑐=𝟎, 𝑏≤h, and neither 𝑝 ⋧ h nor 𝑝′ ⋧ h,then set 𝑐 to the lowest ballot satisfying 𝑏 ≤ 𝑐 ≲ h.
      for {
        c <- getCurrentCommitBallot[A](nodeId, slotIndex)
        r <- _if(c.isDefined, Option.empty[Ballot[A]]) {
          for {
            prepared <- getCurrentPreparedBallot[A](nodeId, slotIndex)
            preparedPassed <- _if(prepared.isEmpty, true) {
              for {
                less <- compareBallots(newH, prepared.get).map(_ <= 0)
                c    <- isCompatible(newH, prepared.get)
              } yield !(less && c)
            }
            r0 <- _if(!preparedPassed, Option.empty[Ballot[A]]) {
              for {
                preparedPrime <- getCurrentPreparedPrimeBallot[A](nodeId, slotIndex)
                preparedPrimePassed <- _if(preparedPrime.isEmpty, true) {
                  for {
                    less <- compareBallots(newH, preparedPrime.get).map(_ <= 0)
                    c    <- isCompatible(newH, preparedPrime.get)
                  } yield !(less && c)
                }
                r00 <- _if(!preparedPrimePassed, Option.empty[Ballot[A]]) {
                  for {
                    messages <- getLatestBallotMessages()
                    b        <- getCurrentBallot[A](nodeId, slotIndex)
                    r000 <- sorted.foldRight((Option.empty[Ballot[A]], false).pureSP[F]) {
                      (n, acc) =>
                        // _2 is break tag
                        for {
                          pre <- acc
                          next <- _if(pre._2, pre) {
                            for {
                              lessB <- _if(b.isEmpty, false)(compareBallots(n, b.get).map(_ < 0)) // lessB, break
                              x <- _if(lessB, (pre._1, true)) {
                                for {
                                  less <- compareBallots(n, newH).map(_ <= 0)
                                  c    <- isCompatible(n, newH)
                                  x0 <- _if(!(less && c), pre) {
                                    for {
                                      ra  <- ratified(n, messages)
                                      x00 <- _if(ra, (Option(n), pre._2))((pre._1, true)) // else break
                                    } yield x00
                                  }
                                } yield x0
                              }
                            } yield x
                          }
                        } yield next
                    }
                  } yield r000._1
                }
              } yield r00
            }
          } yield r0
        }
      } yield r
    }

    def updateState[A <: Value](newH: Ballot[A], newC: Option[Ballot[A]]): SP[F, Boolean] = {
      for {
        b  <- getCurrentBallot[A](nodeId, slotIndex)
        h  <- getCurrentHighestBallot[A](nodeId, slotIndex)
        c1 <- _if(b.isEmpty, false)(isCompatible(b.get, newH))
        c2 <- _if(h.isEmpty, false)(compareBallots(newH, h.get).map(_ > 0))
        result <- _if(!(c1 && c2), false) {
          for {
            modifyH <- _if(h.isEmpty, true)(compareBallots(newH, h.get).map(_ > 0))
            _       <- __ifThen(modifyH)(updateCurrentHighestBallot(nodeId, slotIndex, newH))
            modifyC <- _if(newC.isEmpty, false) {
              for {
                _ <- updateCurrentCommitBallot(nodeId, slotIndex, newC.get)
              } yield true
            }
            modifyB <- updateCurrentIfNeeded[A](nodeId, slotIndex, newH)
            modified = modifyH || modifyC || modifyB
            _ <- __ifThen(modified)(emitCurrentStateStatement(nodeId, slotIndex))
          } yield modified
        }
      } yield result
    }

    for {
      phase    <- getCurrentPhase(nodeId, slotIndex)
      prepared <- getCurrentPreparedBallot[Value](nodeId, slotIndex)
      result <- _if(phase.isPrepae || prepared.isEmpty, false) {
        for {
          candidatedPrepares <- getPrepareCandidates[Value](envelope.statement.message)
          sorted             <- sortBallots[Value](candidatedPrepares.toVector)
          newH               <- findNewH[Value](sorted)
          result <- _if(newH.isEmpty, false) {
            for {
              newC    <- findNewC[Value](sorted, newH.get)
              updated <- updateState(newH.get, newC)
            } yield updated
          }
        } yield result
      }
    } yield result
  }

  private def attemptAcceptCommit(): SP[F, Boolean]  = ???
  private def attemptConfirmCommit(): SP[F, Boolean] = ???
  private def attemptBumpAll(): SP[F, Boolean]       = ??? // @see BallotProtocol.cpp, line 1856-1867

  private def bumpToBallot[A <: Value](nodeId: NodeID, slotIndex: BigInt, ballot: Ballot[A], check: Boolean): SP[F, Unit] = {
    import ballotStore._
    import messageService._
    for {
      phase <- getCurrentPhase(nodeId, slotIndex)
      _ <- __ifThen(!phase.isExternalize) {
        for {
          _ <- __ifThen(check) {
            for {
              b <- getCurrentBallot[A](nodeId, slotIndex)
              checkSP = (_if(b.isEmpty, true)(compareBallots[A](ballot, b.get).map(_ >= 0)))
              _ <- checkSP.assert((x: Boolean) => x , new RuntimeException("chacke failed, ballot shoud greater currentBallot"))
            } yield ()
          }
          b <- getCurrentBallot[A](nodeId, slotIndex)
          gotBumped = (b.isEmpty || b.get.counter != ballot.counter)
          _ <- updateCurrentBallot[A](ballot, nodeId, slotIndex)
          h <- getCurrentHighestBallot[A](nodeId, slotIndex)
          hNeedReset <- _if(h.isEmpty || b.isEmpty, false)(isCompatible(h.get, b.get).map(!_))
          _ <- __ifThen(hNeedReset)(resetCurrentHighestBallot(nodeId, slotIndex))
          _ <- __ifThen(gotBumped)(updateHeardFromQuorum(nodeId, slotIndex, false))
        } yield ()
      }
    } yield ()
  }
  private def updateCurrentIfNeeded[A <: Value](nodeId: NodeID, slotIndex: BigInt, h: Ballot[A]): SP[F, Boolean] = {
    import ballotStore._
    import messageService._
    for {
      b       <- getCurrentBallot[A](nodeId, slotIndex)
      modifyB <- _if(b.isEmpty, true)(compareBallots(b.get, h).map(_ < 0))
      _       <- __ifThen(modifyB)(bumpToBallot(nodeId, slotIndex, h, true))
    } yield modifyB
  }

}
