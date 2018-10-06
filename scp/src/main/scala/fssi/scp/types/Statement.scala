package fssi
package scp
package types

case class Statement(
  nodeId: NodeID,
  slotIndex: BigInt,
  quorumSet: QuorumSet,
  message: Message
)
