package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

/** save node protocol processing status
  */
@sp trait NodeStore[F[_]] {

  /** get votes
    */
  def getVotes(nodeId: NodeID, slotIndex: BigInt, round: Int): P[F, Set[Value]]

  def updateVotes(newVotes: Set[Value], nodeId: NodeID, slotIndex: BigInt, round: Int): P[F, Boolean]

  def hasVotedNewValue(oldVotes: Set[Value], nodeId: NodeID, slotIndex: BigInt, round: Int): P[F, Boolean]

}
