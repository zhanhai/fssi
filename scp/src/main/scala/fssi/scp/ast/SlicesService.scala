package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

@sp trait SlicesService[F[_]] {
  /** delete a node (validators) from slices.
    * Definition (delete). 
    * If ⟨𝐕, 𝐐⟩ is an FBAS and 𝐵 ⊆ 𝐕 is a set of nodes, 
    * then to delete 𝐵 from ⟨𝐕, 𝐐⟩, written ⟨𝐕, 𝐐⟩𝐵 , 
    * means to compute the modified FBAS ⟨𝐕 ⧵ 𝐵, 𝐐𝐵 ⟩ where 𝐐𝐵 (𝑣) = { 𝑞 ⧵ 𝐵 ∣ 𝑞 ∈ 𝐐(𝑣) }.
    */
  def delete(slices: Slices, nodeId: NodeID): P[F, Slices]

  /** simplify slices:
    * simplifies singleton inner set into outerset:
    *   { t: n, v: { ... }, { t: 1, X }, ... } -> { t: n, v: { ..., X }, .... }
    * simplifies singleton innersets:
    *   { t:1, { innerSet } } into innerSet
    */
  def simplify(slices: Slices): P[F, Slices]

  /** @see QuorumSetUtils.cpp, line 47, QuorumSetSanityChecker::checkSanity(SCPQuorumSet const& qSet, int depth)
    */
  def isSlicesSane(slices: Slices): P[F, Boolean]
}
