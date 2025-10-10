
/*------------------------------------------------------------------------
    File        : Testing.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Kishore.M
    Created     : Mon Jan 31 09:52:39 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE TEMP-TABLE ttCustAccntDetails
    FIELD AccntType     AS CHARACTER
    FIELD AccntSubType  AS CHARACTER
    FIELD IFSCCode      AS CHARACTER
    FIELD RateofInt     AS DECIMAL
    FIELD LoanDuration  AS INTEGER
    FIELD TotalLoanAmt  AS INTEGER
    FIELD TransferLimit AS INTEGER.
    
DEFINE VARIABLE oCustAcc AS CustomerAccountsInfo NO-UNDO.   
DEFINE VARIABLE cAcctType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcData     AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iCustId   AS INTEGER   NO-UNDO.
oCustAcc = NEW CustomerAccountsInfo().
cAcctType = "Loan Account".
iCustId = 14.
CREATE ttCustAccntDetails.
ASSIGN
ttCustAccntDetails.AccntSubType = "Personal"
ttCustAccntDetails.AccntType = cAcctType
ttCustAccntDetails.IFSCCode = STRING(444)
ttCustAccntDetails.TransferLimit = 0
ttCustAccntDetails.RateofInt = 2.00
ttCustAccntDetails.TotalLoanAmt = 50000
ttCustAccntDetails.LoanDuration = 2.
 lcData =  oCustAcc:addCustomerAccount(INPUT iCustId, INPUT cAcctType, INPUT TABLE ttCustAccntDetails).
MESSAGE STRING(lcData)
VIEW-AS ALERT-BOX.

