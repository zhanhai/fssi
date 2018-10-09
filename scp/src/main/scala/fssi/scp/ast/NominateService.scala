package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._
@sp trait NominateService[F[_]] {
  def nextRoundTimeout(round: Int): P[F, Long]
  def triggerNextRoundNominate(delay: Long,
                               nodeId: NodeID,
                               slotIndex: BigInt,
                               nextRound: Int,
                               value: Value,
                               previousValue: Value): P[F, Unit]
}
