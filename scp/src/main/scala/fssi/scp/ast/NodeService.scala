package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

@sp trait NodeService[F[_]] {


  /** compute priority for a local node.
    * Define priority(n, v) as Gi(2 || n || v), where 2 and n are both 32-bit XDR int values.
    * @param nodeId this node is a local node, in every slices
    */
  def priorityOfLocal(nodeId: NodeID,
                      slotIndex: BigInt,
                      round: Int,
                      previousValue: Value,
                      slices: Slices): P[F, Long]

  /** compute priority for a peer node.
    * Define priority(n, v) as Gi(2 || n || v), where 2 and n are both 32-bit XDR int values.
    * @param nodeId this node is a peer node, in every slices
    */
  def priorityOfPeer(nodeId: NodeID,
                     slotIndex: BigInt,
                     round: Int,
                     previousValue: Value,
                     slices: Slices): P[F, Long]


  /** can a node set be a V-Blocking set in v's slices ?
    * @param nodes maybe a v-blocking set.
    * @param slices node v's quorum set.
    */
  def isVBlocking(nodes: Set[NodeID], slices: Slices): P[F, Boolean]

  /** can a node set be a Quorum in a node v's Slices
    * @param nodes maybe a quorum
    * @param slices node v's quorum set.
    */
  def isQuorum(nodes: Set[NodeID], slices: Slices): P[F, Boolean]

}
