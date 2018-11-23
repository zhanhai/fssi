package fssi
import fssi.scp.ast.uc.SCP
import fssi.scp.interpreter.Setting
import fssi.scp.types._

package object scp {
  type ApplicationCallback = interpreter.ApplicationCallback
  val ApplicationCallback: interpreter.ApplicationCallback.type = interpreter.ApplicationCallback

  object Portal {
    private lazy val scp = SCP[fssi.scp.ast.components.Model.Op]

    def initialize(slotIndex: SlotIndex)(implicit setting: Setting): Unit = {
      val program =
        scp.initialize(setting.localNode, setting.quorumSet, slotIndex)
      fssi.scp.interpreter.runner.runIO(program, setting).unsafeRunSync()
    }

    def nominateFakeValue(nodeId: NodeID, slotIndex: SlotIndex)(implicit setting: Setting): Unit = {
      val program = scp.nominateFakeValue(nodeId, slotIndex)
      fssi.scp.interpreter.runner.runIO(program, setting).unsafeRunSync()
    }

    def handleRequest(nodeId: NodeID, slotIndex: SlotIndex, previousValue: Value, value: Value)(
        implicit setting: Setting): Boolean = {
      val program = scp.handleAppRequest(nodeId, slotIndex, value, previousValue)
      fssi.scp.interpreter.runner.runIO(program, setting).unsafeRunSync()
    }

    def handleEnvelope[M <: Message](envelope: Envelope[M], previousValue: Value)(
        implicit setting: Setting): Boolean = {
      val program = scp.handleSCPEnvelope(envelope, previousValue)
      fssi.scp.interpreter.runner.runIO(program, setting).unsafeRunSync()
    }

    def stopBroadcastMessage()(implicit setting: Setting): Unit = {
      val program = scp.stopBroadcastMessage()
      fssi.scp.interpreter.runner.runIO(program, setting).unsafeRunSync()
    }

    def startBroadcastMessage(slotIndex: SlotIndex)(implicit setting: Setting): Unit = {
      val program = scp.broadcastMessageRegularly(slotIndex)
      fssi.scp.interpreter.runner.runIO(program, setting).unsafeRunSync()
    }
  }
}
