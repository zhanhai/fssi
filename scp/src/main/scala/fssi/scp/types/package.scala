package fssi
package scp

package object types {
  object implicits
      extends fssi.base.BytesValue.Implicits
      with NodeID.Implicits
      with Signature.Implicits

}
