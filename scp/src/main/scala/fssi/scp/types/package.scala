package fssi
package scp

package object types {
  object implicits extends fssi.types.base.BytesValue.Implicits
      with NodeID.Implicits
}
