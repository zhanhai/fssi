package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class SlicesServiceHandler extends SlicesService.Handler[Stack] with LogSupport {

  /** delete a node (validators) from slices.
    * Definition (delete).
    * If ⟨𝐕, 𝐐⟩ is an FBAS and 𝐵 ⊆ 𝐕 is a set of nodes,
    * then to delete 𝐵 from ⟨𝐕, 𝐐⟩, written ⟨𝐕, 𝐐⟩𝐵 ,
    * means to compute the modified FBAS ⟨𝐕 ⧵ 𝐵, 𝐐𝐵 ⟩ where 𝐐𝐵 (𝑣) = { 𝑞 ⧵ 𝐵 ∣ 𝑞 ∈ 𝐐(𝑣) }.
    */
  override def delete(slices: Slices, nodeId: NodeID): Stack[Slices] = Stack {
    log.debug(s"deleting $nodeId from $slices")
    slices match {
      case Slices.Flat(threshold, validators) =>
        // acc is (remained validators, remained threshold)
        val (remainedValidators, remainedThreshold) =
          validators.foldLeft((Vector.empty[NodeID], threshold)) { (acc, n) =>
            if (n === nodeId) (acc._1, acc._2 - 1)
            else (acc._1 :+ n, acc._2)
          }
        Slices.flat(remainedThreshold, remainedValidators: _*)
      case Slices.Nest(threshold, validators, inners) =>
        val (remainedValidators, remainedThreshold) =
          validators.foldLeft((Vector.empty[NodeID], threshold)) { (acc, n) =>
            if (n === nodeId) (acc._1, acc._2 - 1)
            else (acc._1 :+ n, acc._2)
          }
        val remainedInners = inners.map { x =>
          val (x_remainedValidators, x_remainedThreshold) =
            validators.foldLeft((Vector.empty[NodeID], threshold)) { (acc, n) =>
              if (n === nodeId) (acc._1, acc._2 - 1)
              else (acc._1 :+ n, acc._2)
            }
          Slices.Flat(x_remainedThreshold, x_remainedValidators)
        }
        Slices.nest(remainedThreshold, remainedValidators, remainedInners:_*)
    }
  }

  /** simplify slices:
    * simplifies singleton inner set into outerset:
    *   { t: n, v: { ... }, { t: 1, X }, ... } -> { t: n, v: { ..., X }, .... }
    * simplifies singleton innersets:
    *   { t:1, { innerSet } } into innerSet
    */
  override def simplify(slices: Slices): Stack[Slices] = Stack {
    slices match {
      case x: Slices.Flat => x
      case Slices.Nest(threshold, validators, inner) =>
        // simplifies singleton innersets
        if (threshold == 1 && validators.isEmpty && inner.size == 1) inner.head
        else {
          // simplifies singleton inner set into outerset
          val (v, i) = inner.foldLeft((validators, Vector.empty[Slices.Flat])) {(acc, n) =>
            if (n.validators.size == 1 && n.threshold == 1) (acc._1 :+ n.validators.head, acc._2)
            else (acc._1, acc._2 :+ n)
          }
          Slices.nest(threshold, v, i: _*)
        }
    }
  }
}

object SlicesServiceHandler {
  val instance = new SlicesServiceHandler

  trait Implicits {
    implicit val scpSlicesServiceHandler: SlicesServiceHandler = instance
  }
}
