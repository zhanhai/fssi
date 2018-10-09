package fssi
package scp
package types

/** Value is any data, which should be totally ordered
  */
trait Value {
  def bytes: Array[Byte]
}

object Value {

  sealed trait Validity
  object Validity {
    case object FullyValidate extends Validity
    case object Invalid extends Validity
    case object MaybeValid extends Validity
  }

  trait Implicits {
    implicit def scpValueToBytesValue(x: Value): Array[Byte] = x.bytes
  }
}
