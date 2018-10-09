package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class ValueServiceHandler extends ValueService.Handler[Stack] with LogSupport {

}

object ValueServiceHandler {
  val instance = new ValueServiceHandler

  trait Implicits {
    implicit val scpValueServiceHandler: ValueServiceHandler = instance
  }
}
