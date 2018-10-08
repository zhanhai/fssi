package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

@sp trait NodeService[F[_]] {

  /** For each peer v, define weight(v) as the fraction of quorum slices containing v.
    * if the nodeId is repeated multiple times, it's weight is only the weight of the first occurrence
    */
  /*
  def weight(nodeId: NodeID,
             slotIndex: BigInt,
             round: Int,
             previousValue: Value,
             slices: Slices): P[F, Long]
   */

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

  /** Define the set of nodes neighbors(n) as the set of nodes v for which
    * Gi(1 || n || v) < 2^{256} * weight(v), where 1 and n are both 32-bit XDR int values.
    */
  /*
  def isNeighbor(nodeId: NodeID,
                 slotIndex: BigInt,
                 round: Int,
                 previousValue: Value,
                 slices: Slices): P[F, Boolean]
   */
}
