package fssi
package scp
package types

case class Envelope(
    statement: Statement,
    signature: Signature
)

object Envelope {

  def validState: State = State.valid
  def invalidState: State = State.invalid

  sealed trait State {
    def isValid: Boolean
  }
  object State {
    case object Invalid extends State {
      def isValid: Boolean = false
    }
    case object Valid   extends State {
      def isValid: Boolean = true
    }

    def invalid: State = Invalid
    def valid: State = Valid
  }
}
