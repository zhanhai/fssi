package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._
@sp trait NominateService[F[_]] {
  def isSane(nom: Message): P[F, Boolean]
  def nextRoundTimeout(round: Int): P[F, Long]
  def triggerNextRoundNominate(delay: Long,
                               nodeId: NodeID,
                               slotIndex: BigInt,
                               value: Value,
                               previousValue: Value): P[F, Unit]
}
