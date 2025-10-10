 
/*------------------------------------------------------------------------
   File        : SavingAccTxnInfo
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Sat Feb 05 19:32:18 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS SavingAccTxnInfo INHERITS MyQuery: 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC SavingAccTxnInfo (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR AddDeposit( INPUT oField AS JsonObject ):
        
        DEFINE VARIABLE lcStatus       AS LONGCHAR NO-UNDO.
        DEFINE VARIABLE dBalanceAmount AS DECIMAL  NO-UNDO.
        DEFINE VARIABLE dDepositAmount AS DECIMAL  NO-UNDO.
        DEFINE VARIABLE iAcctNum       AS INTEGER  NO-UNDO.
        
        iAcctNum = (IF oField:Has("AcctNum")  THEN oField:GetInteger("AcctNum") ELSE 0).
        dDepositAmount = (IF oField:Has("DepositAmount")  THEN oField:GetDecimal("DepositAmount") ELSE 0.00).
        
        FIND FIRST SavingAccountDetail WHERE SavingAccountDetail.AcctNum = iAcctNum EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE SavingAccountDetail THEN
        DO TRANSACTION:      
            dBalanceAmount = dDepositAmount + SavingAccountDetail.Balance.
            DO :
                CREATE SavingAccountTxnHistory.
            
                ASSIGN
                    SavingAccountTxnHistory.TxnID          = (IF oField:Has("TxnId")  THEN oField:GetInteger("TxnId") ELSE 0)
                    SavingAccountTxnHistory.AcctNum        = iAcctNum
                    SavingAccountTxnHistory.Balance        = dBalanceAmount
                    SavingAccountTxnHistory.DepositAmount  = dDepositAmount
                    SavingAccountTxnHistory.TxnDate        = (IF oField:Has("TxnDate")  THEN oField:GetDate("TxnDate") ELSE ?)
                    SavingAccountTxnHistory.TxnDetail      = (IF oField:Has("TxnDetail")  THEN oField:GetCharacter("TxnDetail") ELSE "")
                    SavingAccountTxnHistory.WithdrawAmount = (IF oField:Has("WithdrawAmount")  THEN oField:GetDecimal("WithdrawAmount") ELSE 0.00).
                  
                FIND FIRST SavingAccountDetail WHERE SavingAccountDetail.AcctNum = iAcctNum EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL SavingAccountDetail THEN
                DO:
                    SavingAccountDetail.Balance = dBalanceAmount.
                END.
            
                lcStatus =STRING(dDepositAmount) + " Deposited Successfully for AcctNum = " + STRING(iAcctNum) .
            
            END.
        END.
        
        

        RETURN lcStatus.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR AddWithdraw( INPUT oField AS JsonObject ):
        
        DEFINE VARIABLE lcStatus        AS LONGCHAR NO-UNDO.
        DEFINE VARIABLE dBalanceAmount  AS DECIMAL  NO-UNDO.
        DEFINE VARIABLE dWithdrawAmount AS DECIMAL  NO-UNDO.
        DEFINE VARIABLE iAcctNum        AS INTEGER  NO-UNDO.
         
        iAcctNum = (IF oField:Has("AcctNum")  THEN oField:GetInteger("AcctNum") ELSE 0).
        dWithdrawAmount = (IF oField:Has("WithdrawAmount")  THEN oField:GetDecimal("WithdrawAmount") ELSE 0.00).
        
        FIND LAST SavingAccountDetail WHERE SavingAccountDetail.AcctNum = iAcctNum NO-LOCK NO-ERROR.
        IF AVAILABLE SavingAccountDetail THEN
        DO:      
            IF dWithdrawAmount < SavingAccountDetail.Balance THEN
            DO TRANSACTION:
                dBalanceAmount = SavingAccountDetail.Balance - dWithdrawAmount.
                DO :
                    CREATE SavingAccountTxnHistory.
            
                    ASSIGN
                        SavingAccountTxnHistory.TxnID          = (IF oField:Has("TxnId")  THEN oField:GetInteger("TxnId") ELSE 0)
                        SavingAccountTxnHistory.AcctNum        = iAcctNum
                        SavingAccountTxnHistory.Balance        = dBalanceAmount
                        SavingAccountTxnHistory.DepositAmount  = (IF oField:Has("DepositAmount")  THEN oField:GetDecimal("DepositAmount") ELSE 0.00)
                        SavingAccountTxnHistory.TxnDate        = (IF oField:Has("TxnDate")  THEN oField:GetDate("TxnDate") ELSE ?)
                        SavingAccountTxnHistory.TxnDetail      = (IF oField:Has("TxnDetail")  THEN oField:GetCharacter("TxnDetail") ELSE "")
                        SavingAccountTxnHistory.WithdrawAmount = dWithdrawAmount.
                  
                    FIND FIRST SavingAccountDetail WHERE SavingAccountDetail.AcctNum = iAcctNum EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAIL SavingAccountDetail THEN
                    DO:
                        SavingAccountDetail.Balance = dBalanceAmount.
                    END.
            
                    lcStatus =STRING(dWithdrawAmount) + " WithDraw Successfully from AcctNum = " + STRING(iAcctNum) .
            
                END.
            END.
            ELSE
            DO:
                lcStatus = "InSufficient balance".
            END.
        END.
        
        RETURN lcStatus.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR generateTxnReport(INPUT cWhereClause AS CHARACTER  ):

        DEFINE VARIABLE lcStatus    AS LONGCHAR  NO-UNDO.
        DEFINE VARIABLE iCount      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hCustHandle AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cFieldName  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lValidQuery AS LOGICAL   NO-UNDO.

        THIS-OBJECT:BufferHandle = BUFFER SavingAccountTxnHistory:HANDLE.
        THIS-OBJECT:WhereClause = cWhereClause.

        lValidQuery = populateQuery().
        IF lValidQuery = NO THEN
        DO:
            MESSAGE "Invalid Query"
                VIEW-AS ALERT-BOX.
        END.
        ELSE
        DO:
            OUTPUT TO VALUE("C:\Users\kishore.m\Documents\Progress\TxnReport.csv").
            EXPORT DELIMITER "," "AcctNum" "TxnDate" "TxnID" "TxnDetail" "WithdrawAmount" "DepositAmount" "Balance".
            THIS-OBJECT:QueryHandle:QUERY-OPEN().
            DO WHILE getNext():
                EXPORT DELIMITER "," SavingAccountTxnHistory.AcctNum
                                     SavingAccountTxnHistory.TxnDate
                                     SavingAccountTxnHistory.TxnID
                                     SavingAccountTxnHistory.TxnDetail
                                     SavingAccountTxnHistory.WithdrawAmount
                                     SavingAccountTxnHistory.DepositAmount
                                     SavingAccountTxnHistory.Balance.
            END.
            THIS-OBJECT:QueryHandle:QUERY-CLOSE ().
        END.
        lcStatus = "Successfully Report generated....." + QUOTER ("C:\Users\kishore.m\Documents\Progress\TxnReport.csv").

        RETURN lcStatus.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC INTEGER getNextTxnId( INPUT iAcctNum AS INTEGER ):
        
        DEFINE VARIABLE iTxnId AS INTEGER NO-UNDO.

        RETURN iTxnId.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getTxnDetailByTxnType( INPUT cWhere AS CHARACTER  ):
        
        DEFINE VARIABLE lcResult      AS LONGCHAR   NO-UNDO.
        DEFINE VARIABLE iCount        AS INTEGER    NO-UNDO.
        DEFINE VARIABLE hCustHandle   AS HANDLE     NO-UNDO.
        DEFINE VARIABLE oInnerJsonObj AS JsonObject NO-UNDO.
        DEFINE VARIABLE oOuterJsonObj AS JsonObject NO-UNDO.
        DEFINE VARIABLE oJsonArr      AS JsonArray  NO-UNDO.
        DEFINE VARIABLE cFieldName    AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE lValidQuery   AS LOGICAL    NO-UNDO.

        THIS-OBJECT:BufferHandle = BUFFER SavingAccountTxnHistory:HANDLE.
        THIS-OBJECT:WhereClause = cWhere.
  
        lValidQuery = populateQuery().
        IF lValidQuery = NO THEN
        DO:
            MESSAGE "Invalid Query"
                VIEW-AS ALERT-BOX.
        END.
        ELSE
        DO:
            THIS-OBJECT:QueryHandle:QUERY-OPEN().
            oJsonArr = NEW JsonArray().
            DO WHILE getNext():
                oInnerJsonObj = NEW JsonObject(). 
                DO iCount = 1 TO BufferHandle:NUM-FIELDS:
                    cFieldName = BufferHandle:BUFFER-FIELD (iCount):NAME.
                    oInnerJsonObj:ADD(cFieldName,BufferHandle:BUFFER-FIELD (iCount):BUFFER-VALUE()).
                END. 
                oJsonArr:ADD(oInnerJsonObj).
                
            END.
            oOuterJsonObj = NEW JsonObject().
            oOuterJsonObj:ADD("ttSavingAccountTxnHistory",oJsonArr).
            oOuterJsonObj:WRITE(lcResult,TRUE,"UTF-8").
            THIS-OBJECT:QueryHandle:QUERY-CLOSE ().
        END.
        RETURN lcResult.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getTxnDetails( INPUT iAcctNum AS INTEGER ):
        
        DEFINE VARIABLE lcResult   AS LONGCHAR   NO-UNDO.
        DEFINE VARIABLE iCount     AS INTEGER    NO-UNDO.
        DEFINE VARIABLE hTxnHandle AS HANDLE     NO-UNDO.
        DEFINE VARIABLE oInnerObj  AS JsonObject NO-UNDO.
        DEFINE VARIABLE oOuterObj  AS JsonObject NO-UNDO.
        DEFINE VARIABLE oJsonArr   AS JsonArray  NO-UNDO.
        DEFINE VARIABLE cFieldName AS CHARACTER  NO-UNDO.
       
         
        hTxnHandle = BUFFER SavingAccountTxnHistory:HANDLE.
        oJsonArr = NEW JsonArray().
        oOuterObj = NEW JsonObject().

        FOR EACH SavingAccountTxnHistory WHERE SavingAccountTxnHistory.AcctNum = iAcctNum NO-LOCK :

            oInnerObj = NEW JsonObject().

            DO iCount = 1 TO hTxnHandle:NUM-FIELDS:

                cFieldName = hTxnHandle:BUFFER-FIELD (iCount):NAME.
                oInnerObj:ADD(cFieldName,hTxnHandle:BUFFER-FIELD (iCount):BUFFER-VALUE()).
            END.
            oJsonArr:ADD(oInnerObj).
        END.

        oOuterObj:ADD("ttSavingAccountTxnHistory",oJsonArr).
        oOuterObj:WRITE(lcResult,TRUE,"UTF-8").

        RETURN lcResult.

        CATCH e AS Progress.Lang.Error:
            MESSAGE e:GetMessage(1)
                VIEW-AS ALERT-BOX.

        END CATCH.

        FINALLY:
            IF VALID-OBJECT(oInnerObj)THEN DELETE OBJECT oInnerObj.
            IF VALID-OBJECT(oOuterObj)THEN DELETE OBJECT oOuterObj.
            IF VALID-OBJECT(oJsonArr)THEN DELETE OBJECT oJsonArr.

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC SavingAccTxnInfo ( ):

    END DESTRUCTOR.

END CLASS.