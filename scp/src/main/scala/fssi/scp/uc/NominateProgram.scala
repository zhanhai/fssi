package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait NominateProgram[F[_]] extends BaseProgram {
  import model._

  /** some node (identified by a NodeID) nominate a value for the slot indexed by slotIndex on n-th round,
    * based on the value of previous slot.
    */
  def nominate(nodeId: NodeID,
               slotIndex: BigInt,
               round: Int,
               value: Value,
               previousValue: Value): SP[F, Boolean] = {
    ???
  }
}
