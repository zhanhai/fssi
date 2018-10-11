package fssi.scp

import fssi.scp.ast.uc.SCPProgram
import fssi.scp.ast.components
import fssi.scp.ast.uc.NominateHelper
import fssi.scp.interpreter._
import fssi.scp.types._
import org.scalatest._

class SCPProgramSpec extends FunSuite {

  val scpProgram = SCPProgram[components.Model.Op]
  val setting = Setting()

  test("get round leaders") {
    scpProgram

    /*
    val node1 = NodeID("node1".getBytes)
    val node2 = NodeID("node2".getBytes)
    val node3 = NodeID("node3".getBytes)
    val node4 = NodeID("node4".getBytes)
    val node5 = NodeID("node5".getBytes)
    val node6 = NodeID("node6".getBytes)
    val node7 = NodeID("node7".getBytes)
    val node8 = NodeID("node8".getBytes)

    /*
    val slices1 = Slices.flat(2, node1, node2, node3, node4).nest(3, node5, node6, node7, node8)
    */
    val previousValue = IntValue(100)

    def p(slotIndex: BigInt, round: Int) = for {
      p1 <- nominateHelper.roundLeaders(node1, slotIndex, round, previousValue)
      p2 <- nominateHelper.roundLeaders(node2, slotIndex, round, previousValue)
      p3 <- nominateHelper.roundLeaders(node3, slotIndex, round, previousValue)
      p4 <- nominateHelper.roundLeaders(node4, slotIndex, round, previousValue)
      p5 <- nominateHelper.roundLeaders(node5, slotIndex, round, previousValue)
      p6 <- nominateHelper.roundLeaders(node6, slotIndex, round, previousValue)
      p7 <- nominateHelper.roundLeaders(node7, slotIndex, round, previousValue)
      p8 <- nominateHelper.roundLeaders(node8, slotIndex, round, previousValue)
    } yield (p1, p2, p3, p4, p5, p6, p7, p8)

    for (i <- 1 to 8) {
      val res = runner.runIO(p(1, i), setting).unsafeRunSync
      info(s"round $i: $res")
    }
    */

  }
}
