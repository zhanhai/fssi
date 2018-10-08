package fssi
package scp
package types

case class Signature(value: Array[Byte])

object Signature {
  trait Implicits {
    implicit def scpSignatureToBytesValue(s: Signature): Array[Byte] = s.value
  }
}
