package fssi
package scp
package types

/** Each participant or node in the SCP protocol has a digital signature key and 
  * is named by the corresponding public key, which we term a NodeID. 
  */
case class NodeID(value: Array[Byte]) extends AnyVal {
  def ===(other: NodeID): Boolean = value sameElements other.value

  override def toString: String = {
    import implicits._
    this.asBytesValue.bcBase58
  }
}

object NodeID {

  trait Implicits {
    implicit def nodeIdToBytesValue(nodeId: NodeID): Array[Byte] = nodeId.value
  }
}
