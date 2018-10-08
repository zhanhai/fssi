package fssi
package scp
package ast

import bigknife.sop.effect.error.ErrorM
import bigknife.sop.macros._
import bigknife.sop.implicits._
import cats.data.Kleisli

object components {
  @sps trait Model[F[_]] {
    val err: ErrorM[F]
    //val log: LogService[F]

    val nodeService: NodeService[F]
  }
}
