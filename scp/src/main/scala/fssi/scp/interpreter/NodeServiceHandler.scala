package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class NodeServiceHandler extends NodeService.Handler[Stack] with LogSupport {

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

  /** can a node set be a V-Blocking set in v's slices ?
    * @param nodes maybe a v-blocking set.
    * @param slices node v's quorum set.
    */
  override def isVBlocking(nodes: Set[NodeID], slices: Slices): Stack[Boolean] = Stack {
    if (slices.threshold == 0) false
    else {
      // ref: what's vblocking set.
      // Definition (𝑣-blocking).
      // Let 𝑣 ∈ 𝐕 be a node in FBAS ⟨𝐕, 𝐐⟩. A set 𝐵 ⊆ 𝐕 is 𝑣-blocking iff it overlaps
      //     every one of 𝑣’s slices—i.e., ∀𝑞 ∈ 𝐐(𝑣), 𝑞 ∩ 𝐵 ≠ ∅.
      slices match {
        case Slices.Flat(threshold, validators) =>
          val blockingNum = validators.size - threshold + 1
          // if count of the intersection between validators and nodes is `ge` blockingNum, then it's a vblocking set
          nodes.count(validators.contains) >= blockingNum
        case Slices.Nest(threshold, validators, inners) =>
          val blockingNum = validators.size + inners.size - threshold + 1
          // inners is a Vector of Flat. if any inner is not covered, then it's not vblocking set
          // then check the outter, inner's vblocking count + outter validators' vblocking, if that number is ge blockingNum
          //      totally, it's a vblocking set
          val innerBlockingCount = inners.count {f =>
            val fBlockingNum = f.validators.size - f.threshold + 1
            nodes.count(f.validators.contains) >= fBlockingNum
          }
          val outterBlockingCount = nodes.count(validators.contains)

          (innerBlockingCount + outterBlockingCount) >= blockingNum
      }
    }
  }

  /** can a node set be a Quorum in a node v's Slices
    * @param nodes maybe a quorum
    * @param slices node v's quorum set.
    */
  override def isQuorum(nodes: Set[NodeID], slices: Slices): Stack[Boolean] = Stack {
    slices match {
      case Slices.Flat(threshold, validators) =>
        nodes.count(validators.contains) >= threshold
      case Slices.Nest(threshold, validators, inners) =>
        val innerCount = inners.count {f =>
          nodes.count(f.validators.contains) >= f.threshold
        }
        val outterCount = nodes.count(validators.contains)
        (innerCount + outterCount) >= threshold
    }
  }

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
