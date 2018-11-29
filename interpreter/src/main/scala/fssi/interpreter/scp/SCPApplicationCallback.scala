package fssi.interpreter.scp
import fssi.ast.uc.CoreNodeProgram
import fssi.interpreter.Setting.CoreNodeSetting
import fssi.interpreter.{LogSupport, UnsignedBytesSupport}
import fssi.scp.interpreter.{ApplicationCallback, FakeValue}
import fssi.scp.types._
import fssi.scp.types.implicits._
import fssi.types.base.Hash
import fssi.types.implicits._
import fssi.utils._
import fssi.interpreter._

trait SCPApplicationCallback
    extends ApplicationCallback
    with UnsignedBytesSupport
    with LogSupport
    with SCPSupport {

  def coreNodeSetting: CoreNodeSetting

  override def loggerName: String = "fssi.interpreter.scp.callback"

  override def validateValue(nodeId: NodeID, slotIndex: SlotIndex, value: Value): Value.Validity =
    value match {
      case BlockValue(block) =>
        val hash = Hash(crypto.hash(calculateUnsignedBlockBytes(block)))
        if (hash === block.hash) {
          val existedBadSignature = block.transactions.exists { transaction =>
            val unsignedBytes = calculateUnsignedTransactionBytes(transaction)
            !crypto.verifySignature(
              transaction.signature.value,
              unsignedBytes,
              crypto.rebuildECPublicKey(transaction.publicKeyForVerifying.value, crypto.SECP256K1))
          }
          val existedGoodSignature = block.transactions.exists { transaction =>
            val unsignedBytes = calculateUnsignedTransactionBytes(transaction)
            crypto.verifySignature(
              transaction.signature.value,
              unsignedBytes,
              crypto.rebuildECPublicKey(transaction.publicKeyForVerifying.value, crypto.SECP256K1))
          }
          (existedBadSignature, existedGoodSignature) match {
            case (true, true)  => Value.Validity.MaybeValid
            case (false, true) => Value.Validity.FullyValidated
            case _             => Value.Validity.Invalid
          }
        } else Value.Validity.Invalid
      case FakeValue(_) => Value.Validity.Invalid
    }

  override def combineValues(nodeId: NodeID,
                             slotIndex: SlotIndex,
                             value: ValueSet): Option[Value] = {
    val validValues = value.filter {
      case _: BlockValue => true
      case _             => false
    }
    if (validValues.isEmpty) None
    else {
      val newValue: Value = validValues.reduceLeft { (ac, value) =>
        (ac, value) match {
          case (BlockValue(acc), BlockValue(n)) =>
            if (acc.chainId == n.chainId && acc.height == n.height) {
              BlockValue(acc.copy(transactions = acc.transactions ++ n.transactions))
            } else
              throw new RuntimeException(
                s"scp value set were not inconsistent, found chainId(${acc.chainId},${n.chainId}) , height(${acc.height},${n.height})")
          case _ => ac
        }
      }
      val blockValue = newValue.asInstanceOf[BlockValue]
      val newBlock = blockValue.block.copy(
        hash = Hash(crypto.hash(calculateUnsignedBlockBytes(blockValue.block))))
      Some(BlockValue(newBlock))
    }
  }

  override def extractValidValue(nodeId: NodeID,
                                 slotIndex: SlotIndex,
                                 value: Value): Option[Value] = value match {
    case BlockValue(block) =>
      val newBlock = block.copy(transactions = block.transactions.filter { transaction =>
        val unsignedBytes = calculateUnsignedTransactionBytes(transaction)
        crypto.verifySignature(
          transaction.signature.value,
          unsignedBytes,
          crypto.rebuildECPublicKey(transaction.publicKeyForVerifying.value, crypto.SECP256K1))
      })
      val newHashedBlock =
        newBlock.copy(hash = Hash(crypto.hash(calculateUnsignedBlockBytes(newBlock))))
      Some(BlockValue(newHashedBlock))
    case FakeValue(_) => None
  }

  override def valueConfirmed(slotIndex: SlotIndex, value: Value): Unit = {
    value match {
      case BlockValue(block) =>
        log.info(s"confirmed block value slotIndex: ${slotIndex.value} --> hash: ${block.hash}")
      case FakeValue(_) =>
    }
  }

  override def valueExternalized(slotIndex: SlotIndex, value: Value): Unit = {
    value match {
      case BlockValue(block) =>
        if (block.height != slotIndex.value)
          throw new RuntimeException(
            s"can not accept block of inconsistent height, slotIndex= ${slotIndex.value} , height: ${block.height}")
        else {
          log.debug(s"to externalize: ${slotIndex.value} --> ${block.hash}")
          runner
            .runIO(CoreNodeProgram.instance.newBlockGenerated(block), coreNodeSetting)
            .unsafeRunSync()
          log.info(s"externalized: ${slotIndex.value} --> ${block.hash}")
        }
      case FakeValue(_) =>
    }
  }

  override def broadcastEnvelope[M <: Message](slotIndex: SlotIndex,
                                               envelope: Envelope[M]): Unit = {
    val transferredEnvelope = envelope.to[Message]
    val scpEnvelope         = SCPEnvelope(transferredEnvelope)
    if (slotIndex.value == envelope.statement.slotIndex.value) {
      runner
        .runIOAttempt(CoreNodeProgram.instance.broadcastConsensusMessage(scpEnvelope),
                      coreNodeSetting)
        .unsafeRunSync() match {
        case Right(_) =>
          log.debug(s"broadcast scp envelope $transferredEnvelope success")
        case Left(e) => log.error(s"broadcast scp envelope failed: ${e.getMessage}", Some(e))
      }
    } else
      throw new RuntimeException(
        s"can't broadcast envelope of inconsistent slot index, received: ${slotIndex.value} , envelope: ${envelope.statement.slotIndex.value}")
  }

  override def currentSlotIndex(): SlotIndex = {
    val blockHeight = StoreHandler.instance.currentHeight()(coreNodeSetting).unsafeRunSync()
    SlotIndex(blockHeight)
  }
}
