package fssi
package scp
package interpreter

import fssi.scp.types._
import org.scalatest._

class NodeServiceHandlerSpec extends FunSuite {
  object context {
    val setting            = Setting()
    val nodeServiceHandler = NodeServiceHandler.instance
  }

  import context._

  test("nodeId weightIn flat slices") {
    val node1 = NodeID("node1".getBytes)
    val node2 = NodeID("node2".getBytes)
    val node3 = NodeID("node3".getBytes)
    val node4 = NodeID("node4".getBytes)

    val slices1 = Slices.flat(3, node1, node2, node3, node4)
    val p = for {
      weight1 <- nodeServiceHandler.weightIn(node1, slices1)
      weight2 <- nodeServiceHandler.weightIn(node2, slices1)
      weight3 <- nodeServiceHandler.weightIn(node3, slices1)
      weight4 <- nodeServiceHandler.weightIn(node4, slices1)
    } yield (weight1, weight2, weight3, weight4)

    val (a, b, c, d) = p(setting).unsafeRunSync

    info(s"weight of node1 in slice1 is $a,$b,$c,$d")
    assert(a == b)
    assert(b == c)
    assert(c == d)
    assert(d == a)
  }

  test("nodeId weightIn nest slices") {
    val node1 = NodeID("node1".getBytes)
    val node2 = NodeID("node2".getBytes)
    val node3 = NodeID("node3".getBytes)
    val node4 = NodeID("node4".getBytes)
    val node5 = NodeID("node5".getBytes)
    val node6 = NodeID("node6".getBytes)
    val node7 = NodeID("node7".getBytes)
    val node8 = NodeID("node8".getBytes)

    val slices1 = Slices.flat(2, node1, node2, node3, node4).nest(3, node5, node6, node7, node8)


    val p = for {
      weight1 <- nodeServiceHandler.weightIn(node1, slices1)
      weight2 <- nodeServiceHandler.weightIn(node2, slices1)
      weight3 <- nodeServiceHandler.weightIn(node3, slices1)
      weight4 <- nodeServiceHandler.weightIn(node4, slices1)
      weight5 <- nodeServiceHandler.weightIn(node5, slices1)
      weight6 <- nodeServiceHandler.weightIn(node6, slices1)
      weight7 <- nodeServiceHandler.weightIn(node7, slices1)
      weight8 <- nodeServiceHandler.weightIn(node8, slices1)
    } yield (weight1, weight2, weight3, weight4, weight5, weight6, weight7, weight8)

    val (a,b,c,d,e,f,g,h) = p(setting).unsafeRunSync

    info(s"weight of node1 in slice1 is $a,$b,$c,$d")
    assert(a == b)
    assert(b == c)
    assert(c == d)
    assert(d == a)

    info(s"weight of node1 in slice1 is $e,$f,$g,$h")
    assert(e == f)
    assert(f == g)
    assert(g == h)
    assert(h == e)
  }

  test("Gi of m should be of 256-bit length") {
    val i: Int = 1
    val m: Array[Byte] = "Hello,world".getBytes
    val gi = nodeServiceHandler.Gi(i, m)
    assert(gi.length * 8 == 256)
  }

  test("priority of a node in some round") {
    val node1 = NodeID("node1".getBytes)
    val node2 = NodeID("node2".getBytes)
    val node3 = NodeID("node3".getBytes)
    val node4 = NodeID("node4".getBytes)
    val node5 = NodeID("node5".getBytes)
    val node6 = NodeID("node6".getBytes)
    val node7 = NodeID("node7".getBytes)
    val node8 = NodeID("node8".getBytes)

    val slices1 = Slices.flat(2, node1, node2, node3, node4).nest(3, node5, node6, node7, node8)

    val priority1 = nodeServiceHandler.priority(1, node1)(setting).unsafeRunSync
    info(s"priority of node1 on round 1 = ${priority1.toLong}")

    val priority2 = nodeServiceHandler.priority(1, node2)(setting).unsafeRunSync
    info(s"priority of node2 on round 1 = ${priority2.toLong}")

    val priority3 = nodeServiceHandler.priority(1, node3)(setting).unsafeRunSync
    info(s"priority of node3 on round 1 = ${priority3.toLong}")

    val priority4 = nodeServiceHandler.priority(1, node4)(setting).unsafeRunSync
    info(s"priority of node4 on round 1 = ${priority4.toLong}")
  }

  test("isNeighbor for node in slices on some round ") {
    val node1 = NodeID("node1".getBytes)
    val node2 = NodeID("node2".getBytes)
    val node3 = NodeID("node3".getBytes)
    val node4 = NodeID("node4".getBytes)
    val node5 = NodeID("node5".getBytes)
    val node6 = NodeID("node6".getBytes)
    val node7 = NodeID("node7".getBytes)
    val node8 = NodeID("node8".getBytes)

    val slices1 = Slices.flat(2, node1, node2, node3, node4).nest(3, node5, node6, node7, node8)

    val priority1 = nodeServiceHandler.isNeighbor(1, node1, slices1)(setting).unsafeRunSync
    info(s"isNeighbor node1 in slices1 on round 1 = $priority1")

    val priority2 = nodeServiceHandler.isNeighbor(1, node2, slices1)(setting).unsafeRunSync
    info(s"isNeighbor node2 in slices1 on round 1 = $priority2")

    val priority3 = nodeServiceHandler.isNeighbor(1, node3, slices1)(setting).unsafeRunSync
    info(s"isNeighbor node3 in slices1 on round 1 = $priority3")

    val priority4 = nodeServiceHandler.isNeighbor(1, node4, slices1)(setting).unsafeRunSync
    info(s"isNeighbor node4 in slices1 on round 1 = $priority4")

    
    val priority5 = nodeServiceHandler.isNeighbor(1, node5, slices1)(setting).unsafeRunSync
    info(s"isNeighbor node4 in slices1 on round 1 = $priority5")

    val priority6 = nodeServiceHandler.isNeighbor(1, node6, slices1)(setting).unsafeRunSync
    info(s"isNeighbor node6 in slices1 on round 1 = $priority6")

    val priority7 = nodeServiceHandler.isNeighbor(1, node7, slices1)(setting).unsafeRunSync
    info(s"isNeighbor node7 in slices1 on round 1 = $priority7")

    val priority8 = nodeServiceHandler.isNeighbor(1, node8, slices1)(setting).unsafeRunSync
    info(s"isNeighbor node8 in slices1 on round 1 = $priority8")

  }
}
