package fssi
package ast

import types._
import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._

import java.io._

@sp trait ContractStore[F[_]] {

  /** initialize a data directory to be a contract store
    * @param dataDir directory to save contract.
    */
  def initializeContractStore(dataDir: File): P[F, Unit]

  /** self test for a contract store
    * @param block contract store should be tested on block
    * @return if the store is sane return true, or false
    */
  def testContractStore(block: Block): P[F, Boolean]

  /** verify current state of contract store
    */
  def verifyContractStoreState(state: String): P[F, Boolean]

  /** commit staged contract
    */
  def commitStagedContract(height: BigInt): P[F, Unit]

  /** roolback staged contract
    */
  def rollbackStagedContract(height: BigInt): P[F, Unit]

  /** find user contract with gid
    */
  def findUserContract(name: UniqueName, version: Version): P[F, Option[Contract.UserContract]]

  /** temp save user's contract
    */
  def stageContract(height: BigInt, gid: String, contract: Contract.UserContract): P[F, Unit]
}
