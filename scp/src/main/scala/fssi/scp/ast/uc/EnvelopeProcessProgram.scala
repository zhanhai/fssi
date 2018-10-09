package fssi
package scp
package ast
package uc

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

trait EnvelopeProcessProgram[F[_]] extends BaseProgram[F] {
  import model._
  //def processBallotEnvelope(envelope: Envelope): SP[F, Envelope.State] = ???
}
