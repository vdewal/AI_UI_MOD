
/*------------------------------------------------------------------------
    File        : TestBankController.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Kishore.M
    Created     : Mon Jan 17 16:46:50 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Json.ObjectModel.*.

/* ********************  Preprocessor Definitions  ******************** */



/* ***************************  Main Block  *************************** */

/*DEFINE TEMP-TABLE ttCustomer                             */
/*    FIELD CustId        AS INTEGER                       */
/*    FIELD FirstName     AS CHARACTER                     */
/*    FIELD LastName      AS CHARACTER                     */
/*    FIELD Address1      AS CHARACTER                     */
/*    FIELD Address2      AS CHARACTER                     */
/*    FIELD City          AS CHARACTER                     */
/*    FIELD State         AS CHARACTER                     */
/*    FIELD Country       AS CHARACTER                     */
/*    FIELD EmailId       AS CHARACTER                     */
/*    FIELD Phone         AS CHARACTER                     */
/*    FIELD Mobile        AS CHARACTER                     */
/*    FIELD DateOfBirth   AS DATE                          */
/*    FIELD MaritalStatus AS CHARACTER                     */
/*    FIELD ZIPCode       AS CHARACTER.                    */
/*                                                         */
/*DEFINE TEMP-TABLE ttCustAccntDetails                     */
/*    FIELD AccntType     AS CHARACTER                     */
/*    FIELD AccntSubType  AS CHARACTER                     */
/*    FIELD IFSCCode      AS CHARACTER                     */
/*    FIELD RateofInt     AS DECIMAL                       */
/*    FIELD LoanDuration  AS INTEGER                       */
/*    FIELD TotalLoanAmt  AS INTEGER                       */
/*    FIELD TransferLimit AS INTEGER.                      */
/*                                                         */
/*DEFINE INPUT PARAMETER ipcAction AS CHARACTER NO-UNDO.   */
/*DEFINE INPUT PARAMETER ipiCustNum AS INTEGER NO-UNDO.    */
/*DEFINE OUTPUT PARAMETER lcData AS LONGCHAR NO-UNDO.      */
/*DEFINE INPUT PARAMETER cWhereClause AS CHARACTER NO-UNDO.*/
/*DEFINE INPUT PARAMETER TABLE FOR ttCustomer.             */
/*DEFINE INPUT PARAMETER TABLE FOR ttCustAccntDetails.     */
/*DEFINE INPUT PARAMETER cAcctType AS CHARACTER NO-UNDO.   */
/*DEFINE INPUT PARAMETER iAcctNum AS INTEGER NO-UNDO.      */

DEFINE INPUT PARAMETER oInputJson AS JsonObject NO-UNDO.
DEFINE OUTPUT PARAMETER lcData AS LONGCHAR NO-UNDO.

DEFINE VARIABLE ipcAction            AS CHARACTER            NO-UNDO.
DEFINE VARIABLE ipiCustNum           AS INTEGER              NO-UNDO.
DEFINE VARIABLE oInputParametersJson AS JsonObject           NO-UNDO.
DEFINE VARIABLE oTTJson              AS JsonArray            NO-UNDO.
DEFINE VARIABLE oField               AS JsonObject           NO-UNDO.
DEFINE VARIABLE iCount               AS INTEGER              NO-UNDO.
DEFINE VARIABLE cWhereClause         AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cAcctType            AS CHARACTER            NO-UNDO.
DEFINE VARIABLE iAcctNum             AS INTEGER              NO-UNDO.
DEFINE VARIABLE iEmiId               AS INTEGER              NO-UNDO.
DEFINE VARIABLE cCountryCode         AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cStateCode           AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cCityCode            AS CHARACTER            NO-UNDO.
DEFINE VARIABLE oCustomer            AS CustomerDetailsInfo  NO-UNDO.
DEFINE VARIABLE oCustomerAccount     AS CustomerAccountsInfo NO-UNDO.
DEFINE VARIABLE oAcctType            AS AccountTypeInfo      NO-UNDO.
DEFINE VARIABLE oSaving              AS SavingAccountInfo    NO-UNDO.
DEFINE VARIABLE oLoan                AS LoanAccountInfo      NO-UNDO.
DEFINE VARIABLE oLoanEMI             AS LoanEMIDetailInfo    NO-UNDO.
DEFINE VARIABLE oSavTxn              AS SavingAccTxnInfo     NO-UNDO.
DEFINE VARIABLE oReport              AS TxnDetailsReports    NO-UNDO.
DEFINE VARIABLE oAddress             AS Address              NO-UNDO.

ipcAction = (IF oInputJson:Has("Action") THEN oInputJson:GetCharacter("Action") ELSE "").
oInputParametersJson = oInputJson:GetJsonObject("InputParameters").
oTTJson = oInputParametersJson:GetJsonArray("ttTable").

 
IF ipcAction = "getCustomerDetailsByCustId" THEN
DO: 
    oCustomer = NEW CustomerDetailsInfo().
    ipiCustNum = (IF oInputParametersJson:Has("iInputParameterValue")  THEN oInputParametersJson:GetInteger("iInputParameterValue") ELSE 0).
    lcData =  oCustomer:getCustomerDetailsById(INPUT ipiCustNum).  
END.
IF ipcAction = "getCustomerDetailsByFilter" THEN
DO:
    oCustomer = NEW CustomerDetailsInfo().
    cWhereClause = (IF oInputParametersJson:Has("cInputParameterValue")  THEN oInputParametersJson:GetCharacter("cInputParameterValue") ELSE ""). 
    lcData =  oCustomer:getCustomerDetailsByFilter(INPUT cWhereClause).
   // lcData =  oCustomer:getCustomerDetailsByFilter(INPUT cWhereClause,INPUT "FirstName,LastName").
END.
IF ipcAction = "UpdateCustomerDetails" THEN
DO: 
    oCustomer = NEW CustomerDetailsInfo().
    DO iCount = 1 TO oTTJson:LENGTH:
        oField = oTTJson:GetJsonObject(iCount).
    END.
    lcData =  oCustomer:updateCustomerDetails(INPUT oField ).
END.
IF ipcAction = "AddCustomerDetails" THEN
DO:
    oCustomer = NEW CustomerDetailsInfo().
    DO iCount = 1 TO oTTJson:LENGTH:
        oField = oTTJson:GetJsonObject(iCount).
    END.
    lcData =  oCustomer:addCustomerDetails(INPUT oField ).
END.
IF ipcAction = "DeleteCustomerDetails" THEN
DO:
    oCustomer = NEW CustomerDetailsInfo().
    ipiCustNum = (IF oInputParametersJson:Has("iInputParameterValue")  THEN oInputParametersJson:GetInteger("iInputParameterValue") ELSE 0).
    lcData =  oCustomer:DeleteCustomerDetails(INPUT ipiCustNum).
END.

IF ipcAction = "getCustomerAccountDetailsByCustId" THEN
DO:
    oCustomerAccount = NEW CustomerAccountsInfo().
    ipiCustNum = (IF oInputParametersJson:Has("iInputParameterValue")  THEN oInputParametersJson:GetInteger("iInputParameterValue") ELSE 0).
    lcData =  oCustomerAccount:getCustomerAccountDetails(INPUT ipiCustNum).
END.                                                                                                               
IF ipcAction = "AddCustomerAccount" THEN
DO:
    oCustomerAccount = NEW CustomerAccountsInfo().
    ipiCustNum = (IF oInputParametersJson:Has("iInputParameterValue")  THEN oInputParametersJson:GetInteger("iInputParameterValue") ELSE 0).
    cAcctType = (IF oInputParametersJson:Has("cInputParameterValue")  THEN oInputParametersJson:GetCharacter("cInputParameterValue") ELSE "").
    DO iCount = 1 TO oTTJson:LENGTH:
        oField = oTTJson:GetJsonObject(iCount).
    END.
    lcData =  oCustomerAccount:addCustomerAccount(INPUT ipiCustNum, INPUT cAcctType, INPUT oField).
END.
IF ipcAction = "DeleteAccount" THEN
DO:
    oCustomerAccount = NEW CustomerAccountsInfo().
    iAcctNum = (IF oInputParametersJson:Has("iInputParameterValue")  THEN oInputParametersJson:GetInteger("iInputParameterValue") ELSE 0).
    cAcctType = (IF oInputParametersJson:Has("cInputParameterValue")  THEN oInputParametersJson:GetCharacter("cInputParameterValue") ELSE "").
    lcData =  oCustomerAccount:deleteAccount(INPUT iAcctNum, INPUT cAcctType).
END.

IF ipcAction = "getAcctSubType" THEN
DO:
    oAcctType = NEW AccountTypeInfo().
    cAcctType = (IF oInputParametersJson:Has("cInputParameterValue")  THEN oInputParametersJson:GetCharacter("cInputParameterValue") ELSE "").
    lcData =  oAcctType:getAcctSubType(INPUT cAcctType).
END.

IF ipcAction = "getSavAcctDetails" THEN
DO:
    oSaving = NEW SavingAccountInfo().
    iAcctNum = (IF oInputParametersJson:Has("iInputParameterValue")  THEN oInputParametersJson:GetInteger("iInputParameterValue") ELSE 0).
    lcData =  oSaving:getSavingActDetails(INPUT iAcctNum).
END.
IF ipcAction = "UpdateSavAcct" THEN
DO: 
    oSaving = NEW SavingAccountInfo().
   DO iCount = 1 TO oTTJson:LENGTH:
        oField = oTTJson:GetJsonObject(iCount).
    END.
    lcData =  oSaving:updateSavingAccount(INPUT oField ). 
END.

IF ipcAction = "getLoanAcctDetails" THEN
DO:
    oLoan = NEW LoanAccountInfo().
    iAcctNum = (IF oInputParametersJson:Has("iInputParameterValue")  THEN oInputParametersJson:GetInteger("iInputParameterValue") ELSE 0).
    lcData =  oLoan:getLoanActDetails(INPUT iAcctNum).
END.
IF ipcAction = "UpdateLoanAcct" THEN
DO:
    oLoan = NEW LoanAccountInfo().
    DO iCount = 1 TO oTTJson:LENGTH:
        oField = oTTJson:GetJsonObject(iCount).
    END.
    lcData =  oLoan:updateLoanAccount(INPUT oField ).
END.

IF ipcAction = "getTxnDetails" THEN
DO:
    oSavTxn = NEW SavingAccTxnInfo().
    iAcctNum = (IF oInputParametersJson:Has("iInputParameterValue")  THEN oInputParametersJson:GetInteger("iInputParameterValue") ELSE 0).
    lcData =  oSavTxn:getTxnDetails(INPUT iAcctNum).
END.
IF ipcAction = "AddDeposit" THEN
DO:
    oSavTxn = NEW SavingAccTxnInfo().
    DO iCount = 1 TO oTTJson:LENGTH:
        oField = oTTJson:GetJsonObject(iCount).
    END.
    lcData =  oSavTxn:AddDeposit(INPUT oField ).
END.
IF ipcAction = "AddWithdraw" THEN
DO:
    oSavTxn = NEW SavingAccTxnInfo().
    DO iCount = 1 TO oTTJson:LENGTH:
        oField = oTTJson:GetJsonObject(iCount).
    END.
    lcData =  oSavTxn:AddWithdraw(INPUT oField ).
END.
IF ipcAction = "getTxnReport" THEN
DO:
    oSavTxn = NEW SavingAccTxnInfo().
   // oReport = NEW TxnDetailsReports().
    cWhereClause = (IF oInputParametersJson:Has("cInputParameterValue")  THEN oInputParametersJson:GetCharacter("cInputParameterValue") ELSE "").
  //  lcData =  oReport:getTxnReport(INPUT cWhereClause).
    lcData =  oSavTxn:generateTxnReport(INPUT cWhereClause).
END.
IF ipcAction = "getTxnDetailByTxnType" THEN
DO:
    oSavTxn = NEW SavingAccTxnInfo().
    cWhereClause = (IF oInputParametersJson:Has("cInputParameterValue")  THEN oInputParametersJson:GetCharacter("cInputParameterValue") ELSE ""). 
    lcData =  oSavTxn:getTxnDetailByTxnType(INPUT cWhereClause).
END.

IF ipcAction = "getEMIDetails" THEN
DO:
    oLoanEMI = NEW LoanEMIDetailInfo().
    iEmiId = (IF oInputParametersJson:Has("iInputParameterValue")  THEN oInputParametersJson:GetInteger("iInputParameterValue") ELSE 0).
    lcData =  oLoanEMI:getEMIDetails(INPUT iEmiId).
END.
IF ipcAction = "PayEMI" THEN
DO:
    oLoanEMI = NEW LoanEMIDetailInfo().
    DO iCount = 1 TO oTTJson:LENGTH:
        oField = oTTJson:GetJsonObject(iCount).
    END.
    lcData =  oLoanEMI:PayEMI(INPUT oField ).
END.

IF ipcAction = "getStateCode" THEN
DO:
    oAddress = NEW Address().
    cCountryCode = (IF oInputParametersJson:Has("cInputParameterValue")  THEN oInputParametersJson:GetCharacter("cInputParameterValue") ELSE "").
    lcData =  oAddress:getStateCode(INPUT cCountryCode).
END.
IF ipcAction = "getCityCode" THEN
DO:
    oAddress = NEW Address().
    cStateCode = (IF oInputParametersJson:Has("cInputParameterValue")  THEN oInputParametersJson:GetCharacter("cInputParameterValue") ELSE "").
    lcData =  oAddress:getCityCode(INPUT cStateCode).
END.
IF ipcAction = "getPostalCode" THEN
DO:
    oAddress = NEW Address().
    cCityCode = (IF oInputParametersJson:Has("cInputParameterValue")  THEN oInputParametersJson:GetCharacter("cInputParameterValue") ELSE "").
    lcData =  oAddress:getPostalCode(INPUT cCityCode).
END.
IF ipcAction = "getCountryCodes" THEN
DO:
    oAddress = NEW Address().
    lcData =  oAddress:getCountryCodes().
END.

FINALLY:
    IF VALID-OBJECT (oCustomer) THEN DELETE OBJECT oCustomer.
    IF VALID-OBJECT (oCustomerAccount) THEN DELETE OBJECT oCustomerAccount.
    IF VALID-OBJECT (oAcctType) THEN DELETE OBJECT oAcctType.
    IF VALID-OBJECT (oSaving) THEN DELETE OBJECT oSaving.
    IF VALID-OBJECT (oLoan) THEN DELETE OBJECT oLoan.
    IF VALID-OBJECT (oReport) THEN DELETE OBJECT oReport.
    IF VALID-OBJECT (oLoanEMI) THEN DELETE OBJECT oLoanEMI.
    IF VALID-OBJECT (oAddress) THEN DELETE OBJECT oAddress.
END FINALLY.
