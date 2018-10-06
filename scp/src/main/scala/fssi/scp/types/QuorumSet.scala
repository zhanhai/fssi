package fssi
package scp
package types

sealed trait QuorumSet

object QuorumSet {
  /** stateless quorum set, include node's slices in every message
    */
  case class QuorumSlices(slices: Slices) extends QuorumSet

  /** slices reference
    */
  case class QuorumRef(ref: Array[Byte]) extends QuorumSet
}
