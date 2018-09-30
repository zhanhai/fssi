package fssi
package scp

import org.scalatest._
import fssi.scp.types._

class ScpSpec extends FunSuite {
  test("scp slices") {
    val node1 = NodeID(java.util.UUID.randomUUID.toString.getBytes)
    val node2 = NodeID(java.util.UUID.randomUUID.toString.getBytes)
    val node3 = NodeID(java.util.UUID.randomUUID.toString.getBytes)
    val node4 = NodeID(java.util.UUID.randomUUID.toString.getBytes)
    val node5 = NodeID(java.util.UUID.randomUUID.toString.getBytes)
    val node6 = NodeID(java.util.UUID.randomUUID.toString.getBytes)
    val node7 = NodeID(java.util.UUID.randomUUID.toString.getBytes)
    val node8 = NodeID(java.util.UUID.randomUUID.toString.getBytes)

    val slices = Slices.flat(3, node1, node2, node3, node4)
    info(slices.toString)

    val nest = Slices.flat(3, node1, node2, node3).nest(2, node5, node6, node7, node8)
    info(nest.toString)
  }
}
