package fssi
package scp

import fssi.scp.types._

case class IntValue(i: Int) extends Value {
  def bytes: Array[Byte] = {
    import java.nio.ByteBuffer
    val bf = ByteBuffer.allocate(4)
    bf.putInt(i)
    bf.array
  }
}
