package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class BallotStoreHandler extends BallotStore.Handler[Stack] with LogSupport {

}

object BallotStoreHandler {
  val instance = new BallotStoreHandler

  trait Implicits {
    implicit val scpBallotStoreHandler: BallotStoreHandler = instance
  }
}
