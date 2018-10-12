package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

@sp trait SlicesStore[F[_]] {
  /** get slices of a node
    */
  def getSlices(nodeId: NodeID): P[F, Option[Slices]]

  /** get quorum set of a node
    */
  def getQuorumSet(nodeId: NodeID): P[F, Option[QuorumSet]]

  /** update nodeId's quorum set
    */
  def updateQuorumSet(nodeId: NodeID, quorumSet: QuorumSet): P[F, Unit]
  
}
