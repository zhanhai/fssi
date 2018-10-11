package fssi
package scp
package types

import scala.collection._

/** SCP messages
  */
sealed trait Message {
  def isNewerThan(other: Message): Boolean = ???
}

object Message {

  /** Nominate Message.
    * For each slot, the SCP protocol begins in a NOMINATE phase, whose goal is to devise one or more candidate
    * output values for the consensus protocol. In this phase, nodes send nomination messages comprising a
    * monotonically growing set of values: voted and accepted.
    */
  case class Nominate[A <: Value](
      voted: Set[A],
      accepted: Set[A]
  ) extends Message {
    def allValue: Set[A] = voted ++ accepted
  }

  object Nominate {
    def empty[A <: Value]: Nominate[A] = Nominate(Set.empty[A], Set.empty[A])
  }

  /* for all ballot messages: 
    * Variable Meaning
    *  𝜑 Current phase: one of PREPARE, CONFIRM, or EXTERNALIZE
    *  𝑏 Current ballot that node 𝑣 is attempting to prepare and commit (𝑏 ≠ 𝟎)
    *  𝑝′,𝑝 The two highest ballots accepted as prepared such that 𝑝′ ⋦ 𝑝, where 𝑝′ =𝟎 or 𝑝=𝑝′ =𝟎
    *                                                                      if there are no such ballots
    *  𝑐,h In PREPARE: h is the highest ballot confirmed as prepared, or 𝟎 if none; 
    *                  if 𝑐 ≠ 𝟎, then 𝑐 is lowest and h the highest ballot for which
    *                  𝑣 has voted commit and not accepted abort.
    *      In CONFIRM: lowest, highest ballot for which 𝑣 accepted commit
    *      In EXTERNALIZE: lowest, highest ballot for which 𝑣 confirmed commit Invariant: if 𝑐 ≠ 𝟎, then 𝑐 ≲ h ≲ 𝑏.
    *  𝑧 Value to use in next ballot. If h = 𝟎, then 𝑧 is the composite value (see Section 6.1); otherwise, 𝑧 = h.𝑥.
    *  𝑀 Set of the latest ballot message seen from each node
    */

  /** Prepare Message.
    * This message compactly conveys the following (conceptual) federated voting messages:
    * 1. vote-or-accept prepare(ballot)
    * 2. If prepared != NULL: accept prepare(prepared)
    * 3. If preparedPrime != NULL: accept prepare(preparedPrime)
    * 4. If hCounter != 0: confirm prepare(<hCounter, ballot.value>)
    * 5. If cCounter != 0: vote commit(<n, ballot.value>) for every cCounter <= n <= hCounter
    */
  case class Prepare[A <: Value](
    ballot: Ballot[A],
    prepared: Option[Ballot[A]],
    preparedPrime: Option[Ballot[A]],
    hCounter: Int,
    cCounter: Int
  ) extends Message


  /** Confirm Message.
    * The message conveys the following federated vote messages, 
    * where infinity is 2^{32} (a value greater than any ballot counter representable in serialized form):
    * 
    * 1. accept commit(<n, ballot.value>) for every cCounter <= n <= hCounter
    * 2. vote-or-accept prepare(<infinity, ballot.value>)
    * 3. accept prepare(<preparedCounter, ballot.value>)
    * 4. confirm prepare(<hCounter, ballot.value>)
    * 5. vote commit(<n, ballot.value>) for every n >= cCounter
    */
  case class Confirm[A <: Value](
    ballot: Ballot[A],
    preparedCounter: Int,
    hCounter: Int,
    cCounter: Int
  ) extends Message

  /** Externalize Message.
    * An SCPExternalize message conveys the following federated voting messages:
    * 1. accept commit(<n, commit.value>) for every n >= commit.counter
    * 2. confirm commit(<n, commit.value>) for every commit.counter <= n <= hCounter
    * 3. confirm prepare(<infinity, commit.value>)
    */
  case class Externalize[A <: Value](
    commit: Ballot[A],
    hCounter: Int
  ) extends Message
}
