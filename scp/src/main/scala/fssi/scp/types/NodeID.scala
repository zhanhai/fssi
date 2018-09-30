package fssi
package scp
package types

/** Each participant or node in the SCP protocol has a digital signature key and 
  * is named by the corresponding public key, which we term a NodeID. 
  */
case class NodeID(value: Array[Byte]) extends AnyVal

object NodeID {
  trait Implicits {
    implicit def nodeIdToBytesValue(nodeId: NodeID): Array[Byte] = nodeId.value
  }
}
