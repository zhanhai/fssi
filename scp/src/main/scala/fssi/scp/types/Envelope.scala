package fssi
package scp
package types

case class Envelope(
    statement: Statement,
    signature: Signature
)

object Envelope {
  sealed trait State
  object State {
    case object Invalid extends State
    case object Valid   extends State
  }
}
