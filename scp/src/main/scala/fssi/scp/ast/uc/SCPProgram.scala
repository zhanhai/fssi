package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait SCPProgram[F[_]] extends NominateProgram[F] with BallotProgram[F]
object SCPProgram {
  def apply[F[_]](implicit M: components.Model[F]): SCPProgram[F] = new SCPProgram[F] {
    val model = M
  }
}
