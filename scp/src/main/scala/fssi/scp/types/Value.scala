package fssi
package scp
package types

/** Value is any data, which should be totally ordered
  */
trait Value {
  def bytes: Array[Byte]
}

object Value {
  trait Implicits {
    implicit def scpValueToBytesValue(x: Value): Array[Byte] = x.bytes
  }
}
