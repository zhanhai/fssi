package fssi
package scp
package types

case class Envelope(
    statement: Statement,
    signature: Signature
)
