package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class BallotServiceHandler extends BallotService.Handler[Stack] with LogSupport {

}

object BallotServiceHandler {
  val instance = new BallotServiceHandler

  trait Implicits {
    implicit val scpBallotServiceHandler: BallotServiceHandler = instance
  }
}
