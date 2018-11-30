package fssi.scp
package ast
package uc

import types._
import components._

import bigknife.sop._
import bigknife.sop.implicits._

trait AttemptConfirmCommitProgram[F[_]]
    extends SCP[F]
    with EmitProgram[F]
    with InitializeProgram[F] {
  import model.nodeService._
  import model.nodeStore._
  import model.applicationService._
  import model.logService._

  def attemptConfirmCommit(slotIndex: SlotIndex,
                           previousValue: Value,
                           hint: Statement[Message.BallotMessage]): SP[F, Boolean] = {

    val ballotToExternalize = hint.message.externalizableBallot
    lazy val ignoreByCurrentPhase: SP[F, Boolean] =
      ifM(ballotToExternalize.isEmpty, true)(for {
        canConfirm <- canConfirmCommitNow(slotIndex, ballotToExternalize.get)
      } yield !canConfirm)

    def isCounterConfirmed(interval: CounterInterval, ballot: Ballot): SP[F, Boolean] = {
      for {
        acceptedNodes <- nodesAcceptedCommit(slotIndex, ballot, interval)
        accepted      <- isLocalQuorum(acceptedNodes)
      } yield accepted
    }

    def confirmedCommitCounterInterval(boundaries: CounterSet,
                                       ballot: Ballot): SP[F, CounterInterval] = {
      // Option[interval], everAccepted, lastAccepted
      val x = boundaries.foldRight((Option.empty[CounterInterval], false, false).pureSP[F]) {
        (n, acc) =>
          for {
            pre <- acc
            next <- ifM(pre._2 && !pre._3, pre) {
              val interval = pre._1.map(_.withFirst(n)).getOrElse(CounterInterval(n))
              for {
                accepted <- isCounterConfirmed(interval, ballot)
                _ <- if (accepted)
                  info(s"[$slotIndex][AttemptConfirmCommit] confirm interval: $interval, $ballot")
                else info(s"[$slotIndex][AttemptConfirmCommit] reject interval: $interval, $ballot")
              } yield
                if ((pre._1.isEmpty || pre._2) && accepted) (Option(interval), accepted, accepted)
                else pre
            }
          } yield next
      }
      x.map {
        case (Some(interval), true, _) => interval

        case _ => CounterInterval()
      }
    }

    ifM(ignoreByCurrentPhase, false.pureSP[F]) {
      val ballot = ballotToExternalize.get

      for {
        phase      <- currentBallotPhase(slotIndex)
        boundaries <- commitBoundaries(slotIndex, ballot)
        _ <- info(
          s"[$slotIndex][AttemptConfirmCommit] found boundaries $boundaries at phase $phase")
        interval <- confirmedCommitCounterInterval(boundaries, ballot)
        confirmed <- ifM(interval.notAvailable, false) {
          val newC = Ballot(interval.first, ballot.value)
          val newH = Ballot(interval.second, ballot.value)
          for {
            transferred <- confirmCommitted(slotIndex, newC, newH)
            _           <- info(s"[$slotIndex][AttemptConfirmCommit] confirm ($newC - $newH), $transferred")
            _ <- ifThen(transferred) {
              for {
                _ <- info(
                  s"[$slotIndex][AttemptConfirmCommit] confirmed ($newC - $newH), stop nominating")
                _ <- stopNominating(slotIndex)
              } yield ()
            }

          } yield transferred

        }
        phaseNow <- currentBallotPhase(slotIndex)
        _ <- ifThen(phase == Ballot.Phase.Confirm && phaseNow == Ballot.Phase.Externalize) {
          for {
            _ <- info(s"[$slotIndex][AttemptConfirmCommit] phase upgraded to Externalize")
            c <- currentConfirmedBallot(slotIndex)
            _ <- phaseUpgradeToExternalize(slotIndex, c)
          } yield ()
        }
        _ <- ifThen(confirmed) {
          for {
            msg              <- createBallotMessage(slotIndex)
            _                <- emitBallot(slotIndex, previousValue, msg)
            currentSlotIndex <- currentSlotIndex()
            _                <- nominateFakeValue(SlotIndex(currentSlotIndex.value + 1))
          } yield ()
        }
      } yield confirmed
    }
  }
}
