package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait BallotBridge[F[_]] extends BaseProgram[F] {
  import model._

  /** bump candidates to ballot protocol
    */
  def bumpBallotState(nodeId: NodeID, slotIndex: BigInt, value: Value): SP[F, Unit]
}
