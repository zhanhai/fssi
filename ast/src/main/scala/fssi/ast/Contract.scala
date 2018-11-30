package fssi.ast

import java.io.File

import bigknife.sop._
import bigknife.sop.macros._
import bigknife.sop.implicits._
import fssi.contract.lib.Context
import fssi.types.base.UniqueName
import fssi.types.biz._
import fssi.types.exception.FSSIException
import fssi.types.biz.Contract._
import fssi.types.biz.Message.ApplicationMessage.TransactionMessage

@sp trait Contract[F[_]] {

  /** check runtime, if not acceptable, throw exception
    */
  def assertRuntime(): P[F, Unit]
  def initializeRuntime(): P[F, Unit]
  def closeRuntime(): P[F, Unit]

  def createContractProject(projectRoot: File): P[F, Either[FSSIException, Unit]]
  def compileContractProject(accountId: Account.ID,
                             pubKey: Account.PubKey,
                             privateKey: Account.PrivKey,
                             projectDir: File,
                             sandboxVersion: String,
                             output: File): P[F, Either[FSSIException, Unit]]
  def checkContractDeterminism(pubKey: Account.PubKey,
                               contractFile: File): P[F, Either[FSSIException, Unit]]
  def invokeContract(context: Context,
                     contractCode: UserContract.Code,
                     method: UserContract.Method,
                     params: Option[UserContract.Parameter]): P[F, Either[FSSIException, Unit]]
  def loadContractFromFile(pubKey: Account.PubKey,
                           contractFile: File): P[F, Either[FSSIException, UserContract]]
  def generateTransactionID(): P[F, Transaction.ID]
  def createTransferTransaction(transactionId: Transaction.ID,
                                payer: Account.ID,
                                publicKey: Account.PubKey,
                                payee: Account.ID,
                                token: Token): P[F, Transaction.Transfer]
  def createDeployTransaction(transactionId: Transaction.ID,
                              owner: Account.ID,
                              publicKey: Account.PubKey,
                              contract: UserContract): P[F, Transaction.Deploy]
  def createRunTransaction(transactionId: Transaction.ID,
                           caller: Account.ID,
                           publicKey: Account.PubKey,
                           owner: Account.ID,
                           contractName: UniqueName,
                           contractVersion: Version,
                           methodAlias: String,
                           contractParameter: Option[UserContract.Parameter]): P[F, Transaction.Run]

  def transferMessageToTransaction(message: Message): P[F, Option[Transaction]]
}
