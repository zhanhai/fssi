package fssi
package scp
package types
package json

import fssi.scp.types._
import io.circe._
import io.circe.syntax._

trait NodeIDJsonCodec {
  implicit val scpNodeIdJsonEncoder: Encoder[NodeID] = ???
}
