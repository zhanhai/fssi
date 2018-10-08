package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class SlicesStoreHandler extends SlicesStore.Handler[Stack] {
  /** get slices for a node
    */
  override def getSlices(nodeId: NodeID): Stack[Option[Slices]] = Stack {
    // todo: this is just for testing
    val node1 = NodeID("node1".getBytes)
    val node2 = NodeID("node2".getBytes)
    val node3 = NodeID("node3".getBytes)
    val node4 = NodeID("node4".getBytes)
    val node5 = NodeID("node5".getBytes)
    val node6 = NodeID("node6".getBytes)
    val node7 = NodeID("node7".getBytes)
    val node8 = NodeID("node8".getBytes)

    val slices1 = Slices.flat(2, node1, node2, node3, node4).nest(3, node5, node6, node7, node8)
    Some(slices1)
  }
}

object SlicesStoreHandler {
  val instance = new SlicesStoreHandler

  trait Implicits {
    implicit val scpSlicesStoreHandler: SlicesStoreHandler = instance
  }
}
