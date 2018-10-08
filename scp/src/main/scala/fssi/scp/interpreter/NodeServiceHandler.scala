package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class NodeServiceHandler extends NodeService.Handler[Stack] with LogSupport {

  /** For each peer v, define weight(v) as the fraction of quorum slices containing v.
    * if the nodeId is repeated multiple times, it's weight is only the weight of the first occurrence
    */
  /*
  override def weight(nodeId: NodeID,
                      slotIndex: BigInt,
                      round: Int,
                      previousValue: Value,
                      slices: Slices): Stack[Long] = Stack {
    import Slices._

    def compute(a: BigInt, b: BigInt, c: BigInt): Long = {
      val bi = (a * b + c - 1) / c
      bi.toLong
    }

    slices match {
      case Flat(threshold, validators) if validators.contains(nodeId) =>
        // (a * b + c - 1) / c
        val a = BigInt(Long.MaxValue)
        val b = BigInt(threshold)
        val c = BigInt(validators.size)
        compute(a, b, c)

      case Nest(threshold, validators, inners) if validators.contains(nodeId) =>
        // (a * b + c - 1) / c
        val a = BigInt(Long.MaxValue)
        val b = BigInt(threshold)
        val c = BigInt(validators.size + inners.size)
        compute(a, b, c)

      case Nest(threshold, validators, inners) =>
        inners
          .find(_.validators.contains(nodeId))
          .map { s =>
            val a = BigInt(Long.MaxValue)
            val b = BigInt(s.threshold)
            val c = BigInt(s.validators.size)

            // the node's weight in inners
            val leafW = compute(a, b, c)
            // let the leafW as the `a`, recompute in total slices
            compute(a = leafW, b = BigInt(threshold), c = BigInt(validators.size + inners.size))
          }
          .getOrElse(0L)

      case _ => 0L
    }
  }
   */

  /** compute priority for a local node.
    * Define priority(n, v) as Gi(2 || n || v), where 2 and n are both 32-bit XDR int values.
    * @param nodeId this node is a local node, in every slices, but maybe be deleted.
    */
  override def priorityOfLocal(nodeId: NodeID,
                               slotIndex: BigInt,
                               round: Int,
                               previousValue: Value,
                               slices: Slices): Stack[Long] = Stack {

    // local node may be not in this slices (because being deleted), so let the weight = Long.MaxValue
    val w = Long.MaxValue

    // if nodeId is neighbor, then calculate it's priority, or set to 0
    val extra = (1.asBytesValue.any ++ round.asBytesValue.any ++ nodeId.asBytesValue.any).bytes
    // gi is the neighbor, value gi < w means isNeighbor
    val gi = hash(slotIndex, previousValue, extra)
    if (gi < w) {
      // if isNeighbor, priority is the hash with flat 2
      hash(slotIndex,
           previousValue,
           (2.asBytesValue.any ++ round.asBytesValue.any ++ nodeId.asBytesValue.any).bytes)
    } else 0
  }

  /** compute priority for a peer node.
    * Define priority(n, v) as Gi(2 || n || v), where 2 and n are both 32-bit XDR int values.
    * @param nodeId this node is a peer node, in every slices
    */
  override def priorityOfPeer(nodeId: NodeID,
                              slotIndex: BigInt,
                              round: Int,
                              previousValue: Value,
                              slices: Slices): Stack[Long] = Stack {

    // peer node weight
    val w = weight(nodeId, slices)
    log.debug(s"peer node weight is $w")

    // if nodeId is neighbor, then calculate it's priority, or set to 0
    val extra = (1.asBytesValue.any ++ round.asBytesValue.any ++ nodeId.asBytesValue.any).bytes
    // gi is the neighbor, value gi < w means isNeighbor
    val gi = hash(slotIndex, previousValue, extra)
    log.debug(s"peer neighbor gi is $gi")
    if (gi < w) {
      // if isNeighbor, priority is the hash with flat 2
      hash(slotIndex,
           previousValue,
           (2.asBytesValue.any ++ round.asBytesValue.any ++ nodeId.asBytesValue.any).bytes)
    } else 0
  }
  /*
  override def priority(round: Int, nodeId: NodeID): Stack[Long] = Stack {
    val m = (round.asBytesValue.any ++ nodeId.asBytesValue.any).bytes
    BigInt(Gi(2, m)).toLong
  }
   */

  /** Define the set of nodes neighbors(n) as the set of nodes v for which
    * Gi(1 || n || v) < 2^{256} * weight(v), where 1 and n are both 32-bit XDR int values.
    * @param round n, the nominating round number
    * @param nodeId node id, validator
    */
  /*
  override def isNeighbor(nodeId: NodeID,
                          slotIndex: BigInt,
                          round: Int,
                          previousValue: Value,
                          slices: Slices): Stack[Boolean] = Stack {
    val extra = (1.asBytesValue.any ++ round.asBytesValue.any ++ nodeId.asBytesValue.any).bytes
    val gi    = hash(slotIndex, previousValue, extra)

    for {
      weight <- weightIn(nodeId, slices)
    } yield gi < weight
  }
   */

  private def hash(slotIndex: BigInt, previousValue: Value, extra: Array[Byte]): Long = {
    import org.bouncycastle.jcajce.provider.digest.SHA3
    val source =
      (slotIndex.asBytesValue.any ++ previousValue.asBytesValue.any ++ extra.asBytesValue.any).bytes
    BigInt(new SHA3.Digest256().digest(source)).toLong
  }

  private def weight(nodeId: NodeID, slices: Slices): Long = {
    import Slices._

    def compute(a: BigInt, b: BigInt, c: BigInt): Long = {
      val bi = (a * b + c - 1) / c
      bi.toLong
    }

    slices match {
      case Flat(threshold, validators) if validators.contains(nodeId) =>
        // (a * b + c - 1) / c
        val a = BigInt(Long.MaxValue)
        val b = BigInt(threshold)
        val c = BigInt(validators.size)
        compute(a, b, c)

      case Nest(threshold, validators, inners) if validators.contains(nodeId) =>
        // (a * b + c - 1) / c
        val a = BigInt(Long.MaxValue)
        val b = BigInt(threshold)
        val c = BigInt(validators.size + inners.size)
        compute(a, b, c)

      case Nest(threshold, validators, inners) =>
        inners
          .find(_.validators.contains(nodeId))
          .map { s =>
            val a = BigInt(Long.MaxValue)
            val b = BigInt(s.threshold)
            val c = BigInt(s.validators.size)

            // the node's weight in inners
            val leafW = compute(a, b, c)
            // let the leafW as the `a`, recompute in total slices
            compute(a = leafW, b = BigInt(threshold), c = BigInt(validators.size + inners.size))
          }
          .getOrElse(0L)

      case _ => 0L
    }
  }
}

object NodeServiceHandler {
  val instance = new NodeServiceHandler
  trait Implicits {
    implicit val scpNodeServiceHandler: NodeServiceHandler = instance
  }
}
