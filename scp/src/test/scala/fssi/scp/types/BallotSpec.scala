package fssi
package scp
package types

import org.scalatest._
import fssi.scp.types._

class BallotSpec extends FunSuite {

  implicit val O: Ordering[IntValue] = new Ordering[IntValue] {
    def compare(a1: IntValue, a2: IntValue): Int = Ordering[Int].compare(a1.i, a2.i)
  }

  object implicits extends Ballot.Implicits
  import implicits._

  test("compare ballots") {
    val b1 = Ballot(1, IntValue(1))
    val b2 = Ballot(1, IntValue(3))
    assert(b1 < b2)
    assert(b1 !~ b2)
    assert(b1 ⋦ b2)
    assert(b2 ⋧ b1)
    
    val b3 = Ballot(1, IntValue(1))
    val b4 = Ballot(2, IntValue(1))
    assert(b4 > b3)
    assert(b3 ~ b4)
    assert(b3 ≲ b4)
    assert(b4 ≳ b3)

    val b5 = Ballot(2, IntValue(1))
    assert(b4 == b5)

  }
}
