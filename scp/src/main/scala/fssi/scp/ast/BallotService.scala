package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

@sp trait BallotService[F[_]] {
  def startBallotProtocolTimer(): P[F, Unit]
  def stopBallotProtocolTimer(): P[F, Unit]
  def nextRoundTimeout(round: Int): P[F, Long]
}
