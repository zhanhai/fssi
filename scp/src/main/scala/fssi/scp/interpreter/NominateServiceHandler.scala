package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class NominateServiceHandler extends NominateService.Handler[Stack] with LogSupport {

}

object NominateServiceHandler {
  val instance = new NominateServiceHandler

  trait Implicits {
    implicit val scpNominateServiceHandler: NominateServiceHandler = instance
  }
}
