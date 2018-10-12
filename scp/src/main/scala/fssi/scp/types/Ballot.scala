package fssi
package scp
package types

case class Ballot[+A <: Value](
    counter: Int,
    value: A
)

object Ballot {

  def preparePhase: Phase = Phase.Prepare
  def confirmPhase: Phase = Phase.Confirm
  def externalizePhase: Phase = Phase.Externalize

  sealed trait Phase {
    def isExternalize: Boolean = this != Phase.Externalize
  }
  object Phase {
    case object Prepare extends Phase
    case object Confirm extends Phase
    case object Externalize extends Phase
  }

  final class BallotOps[A <: Value](b1: Ballot[A])(implicit O: Ordering[Ballot[A]]) {
    import Ordered._

    def compatible(b2: Ballot[A]): Boolean = b1.value == b2.value
    def ∼(b2: Ballot[A]): Boolean          = compatible(b2)
    def ~(b2: Ballot[A]): Boolean          = compatible(b2)

    def imcompatible(b2: Ballot[A]): Boolean = b1.value != b2.value
    def !∼(b2: Ballot[A]): Boolean           = imcompatible(b2)
    def !~(b2: Ballot[A]): Boolean           = imcompatible(b2)

    def leAndCompatible(b2: Ballot[A]): Boolean = b1 <= b2 && compatible(b2)
    def ≲(b2: Ballot[A]): Boolean               = leAndCompatible(b2)

    def geAndCompatible(b2: Ballot[A]): Boolean = b1 >= b2 && compatible(b2)
    def ≳(b2: Ballot[A]): Boolean               = geAndCompatible(b2)

    def leAndImcompatible(b2: Ballot[A]): Boolean = b1 <= b2 && imcompatible(b2)
    def ⋦(b2: Ballot[A]): Boolean                 = leAndImcompatible(b2)

    def geAndImcompatible(b2: Ballot[A]): Boolean = b1 >= b2 && imcompatible(b2)
    def ⋧(b2: Ballot[A]): Boolean                 = geAndImcompatible(b2)

  }

  trait Implicits {

    implicit def ballotOrdering[A <: Value](implicit O: Ordering[A]): Ordering[Ballot[A]] =
      new Ordering[Ballot[A]] {
        def compare(b1: Ballot[A], b2: Ballot[A]): Int = {
          val i = Ordering[Int].compare(b1.counter, b2.counter)
          if (i == 0) {
            Ordering[A].compare(b1.value, b2.value)
          } else i
        }
      }

    implicit def toOrdered[A <: Value](b: Ballot[A])(
        implicit O: Ordering[Ballot[A]]): Ordered[Ballot[A]] = new Ordered[Ballot[A]] {
      def compare(other: Ballot[A]): Int = O.compare(b, other)
    }

    implicit def toBallotOps[A <: Value](b: Ballot[A])(
        implicit O: Ordering[Ballot[A]]): BallotOps[A] = new BallotOps(b)
  }

}
