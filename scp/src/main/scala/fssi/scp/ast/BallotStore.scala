package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

/** save ballot protocol processing status
  */
@sp trait BallotStore[F[_]] {
  def getCurrentBallot[A <: Value](nodeId: NodeID, slotIndex: BigInt): P[F, Option[Ballot[A]]]
}
