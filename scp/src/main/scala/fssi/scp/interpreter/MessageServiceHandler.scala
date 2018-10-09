package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class MessageServiceHandler extends MessageService.Handler[Stack] with LogSupport {

}

object MessageServiceHandler {
  val instance = new MessageServiceHandler

  trait Implicits {
    implicit val scpMessageServiceHandler: MessageServiceHandler = instance
  }
}
