package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class NodeServiceHandler extends NodeService.Handler[Stack] {

  /** For each peer v, define weight(v) as the fraction of quorum slices containing v.
    * if the nodeId is repeated multiple times, it's weight is only the weight of the first occurrence
    */
  override def weightIn(nodeId: NodeID, slices: Slices): Stack[Long] = Stack {
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
        compute(a,b,c)

      case Nest(threshold, validators, inners) if validators.contains(nodeId) =>
        // (a * b + c - 1) / c
        val a = BigInt(Long.MaxValue)
        val b = BigInt(threshold)
        val c = BigInt(validators.size + inners.size)
        compute(a, b, c)

      case Nest(threshold, validators, inners) =>
        inners.find(_.validators.contains(nodeId)).map {s =>
          val a = BigInt(Long.MaxValue)
          val b = BigInt(s.threshold)
          val c = BigInt(s.validators.size)

          // the node's weight in inners
          val leafW = compute(a, b, c)
          // let the leafW as the `a`, recompute in total slices
          compute(a = leafW, b = BigInt(threshold), c = BigInt(validators.size + inners.size))
        }.getOrElse(0L)

      case _ => 0L
    }
  }

  /** Define priority(n, v) as Gi(2 || n || v), where 2 and n are both 32-bit XDR int values.
    * @param round n, the nominating round number
    * @param nodeId node id, validator
    */
  override def priority(round: Int, nodeId: NodeID): Stack[Long] = Stack {
    val m = (round.asBytesValue.any ++ nodeId.asBytesValue.any).bytes
    BigInt(Gi(2, m)).toLong
  }

  /** Define the set of nodes neighbors(n) as the set of nodes v for which 
    * Gi(1 || n || v) < 2^{256} * weight(v), where 1 and n are both 32-bit XDR int values.
    * @param round n, the nominating round number
    * @param nodeId node id, validator
    */
  override def isNeighbor(round: Int, nodeId: NodeID, slices: Slices): Stack[Boolean] = {
    val m = (round.asBytesValue.any ++ nodeId.asBytesValue.any).bytes
    val gi = BigInt(Gi(1, m)).toLong
    
    for {
      weight <- weightIn(nodeId, slices)
    } yield gi < weight
  }

  /** Gi(m) = SHA-256(i || m), 
    * where || denotes the concatenation of serialized XDR values. 
    * Treat the output of Gi as a 256-bit binary number in big-endian format.
    */
  def Gi(i: Int, m: Array[Byte]): Array[Byte] = {
    import org.bouncycastle.jcajce.provider.digest.SHA3
    val source = (i.asBytesValue.any ++ m.asBytesValue.any).bytes
    new SHA3.Digest256().digest(source)
  }
}

object NodeServiceHandler {
  val instance = new NodeServiceHandler
  trait Implicits {
    implicit val scpNodeServiceHandler: NodeServiceHandler = instance
  }
}
