package fssi
package scp
package ast

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import fssi.scp.types._

@sp trait ValueService[F[_]] {
  //def validate(value: Value): P[F, Value.Validity]

  /** check the value, get the correct part
    */
  def extractValidValue(value: Value): P[F, Option[Value]]

  def isFullValidValue(value: Value): P[F, Boolean]

  /** compare two values by using their hash value.
    * then return the higher one.
    */
  def higherValue(v1: Option[Value], v2: Option[Value]): P[F, Option[Value]]

  def combineValues(values: Set[Value]): P[F, Value]
}
