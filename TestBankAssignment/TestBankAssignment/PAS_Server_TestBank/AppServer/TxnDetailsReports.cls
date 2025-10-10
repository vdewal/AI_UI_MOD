 
/*------------------------------------------------------------------------
   File        : txnDetailsReports
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Thu Feb 03 14:55:43 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS TxnDetailsReports INHERITS MyQuery: 
    DEFINE TEMP-TABLE ttSavingAccountTxnHistory
        FIELD AcctNum        AS INTEGER
        FIELD TxnDate        AS DATE
        FIELD TxnId          AS INTEGER
        FIELD TxnDetail      AS CHARACTER
        FIELD WithdrawAmount AS DECIMAL
        FIELD DepositAmount  AS DECIMAL
        FIELD Balance        AS DECIMAL.
    
    DEFINE STREAM iStream.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC TxnDetailsReports (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getTxnReport( INPUT cWhere AS CHARACTER ):
        
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
        lcResult = THIS-OBJECT:jsonToCSV(lcResult).
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

    METHOD PUBLIC LONGCHAR jsonToCSV( INPUT lcData AS LONGCHAR ):
        
        DEFINE VARIABLE lcStatus AS LONGCHAR NO-UNDO. 
        DEFINE VARIABLE lRetOK   AS LOGICAL  NO-UNDO.
       
       
        lRetOK = TEMP-TABLE ttSavingAccountTxnHistory:READ-JSON("LONGCHAR",lcData,"empty").

        output STREAM iStream to "C:\Users\kishore.m\Documents\Progress\TxnReport.csv".
        
        PUT STREAM iStream UNFORMATTED 
            "Account Number" "Transaction date" at 20 "Transaction ID" at 40 "Transaction Detail" at 60
            "Withdraw Amount" at 80 "Deposit Amount" at 100 "Balance " AT 120 SKIP.
        
        FOR EACH ttSavingAccountTxnHistory:
         
            PUT STREAM iStream UNFORMATTED string(ttSavingAccountTxnHistory.AcctNum)  STRING(ttSavingAccountTxnHistory.TxnDate) AT 20
                string(ttSavingAccountTxnHistory.TxnId) AT 40 ttSavingAccountTxnHistory.TxnDetail AT 60 
                STRING(ttSavingAccountTxnHistory.WithdrawAmount) AT 80 STRING(ttSavingAccountTxnHistory.DepositAmount)
                AT 100 STRING(ttSavingAccountTxnHistory.Balance) AT 120 SKIP.
        END.
        OUTPUT STREAM iStream CLOSE.
        lcStatus = "C:\Users\kishore.m\Documents\Progress\TxnReport.csv".
        RETURN lcStatus.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC TxnDetailsReports ( ):

    END DESTRUCTOR.

END CLASS.