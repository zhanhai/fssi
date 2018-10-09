package fssi
package scp
package interpreter

import fssi.scp.ast._
import fssi.scp.types._
import fssi.scp.types.implicits._

import bigknife.sop._

class NominateStoreHandler extends NominateStore.Handler[Stack] with LogSupport {

}

object NominateStoreHandler {
  val instance = new NominateStoreHandler

  trait Implicits {
    implicit val scpNominateStoreHandler: NominateStoreHandler = instance
  }
}
