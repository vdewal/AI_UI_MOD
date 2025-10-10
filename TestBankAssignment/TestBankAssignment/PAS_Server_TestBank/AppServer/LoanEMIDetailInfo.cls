 
/*------------------------------------------------------------------------
   File        : LoanEMIDetailInfo
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Sun Feb 06 19:12:20 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS LoanEMIDetailInfo: 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC LoanEMIDetailInfo (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getEMIDetails( INPUT iEmiId AS INTEGER):
        
        DEFINE VARIABLE lcResult   AS LONGCHAR   NO-UNDO.
        DEFINE VARIABLE iCount     AS INTEGER    NO-UNDO.
        DEFINE VARIABLE hEmiHandle AS HANDLE     NO-UNDO.
        DEFINE VARIABLE oInnerObj  AS JsonObject NO-UNDO.
        DEFINE VARIABLE oOuterObj  AS JsonObject NO-UNDO.
        DEFINE VARIABLE oJsonArr   AS JsonArray  NO-UNDO.
        DEFINE VARIABLE cFieldName AS CHARACTER  NO-UNDO.
       
         
        hEmiHandle = BUFFER LoanEMIDetail:HANDLE.
        oJsonArr = NEW JsonArray().
        oOuterObj = NEW JsonObject().

        FOR EACH LoanEMIDetail WHERE LoanEMIDetail.EMIID = iEmiId NO-LOCK :
            oInnerObj = NEW JsonObject().
            DO iCount = 1 TO hEmiHandle:NUM-FIELDS:
                cFieldName = hEmiHandle:BUFFER-FIELD (iCount):NAME.
                oInnerObj:ADD(cFieldName,hEmiHandle:BUFFER-FIELD (iCount):BUFFER-VALUE()).
            END.
            oJsonArr:ADD(oInnerObj).
        END.
        oOuterObj:ADD("tt-Emi",oJsonArr).
        oOuterObj:WRITE(lcResult,TRUE,"UTF-8").

        RETURN lcResult.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:
            IF VALID-OBJECT(oInnerObj)THEN DELETE OBJECT oInnerObj.
            IF VALID-OBJECT(oOuterObj)THEN DELETE OBJECT oOuterObj.
            IF VALID-OBJECT(oJsonArr)THEN DELETE OBJECT oJsonArr.

        END FINALLY.

    END METHOD.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR PayEMI(INPUT oField AS JsonObject):
        
        DEFINE VARIABLE cStatus    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE dAmount    AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE dEmiAmount AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE ieminum    AS INTEGER   NO-UNDO.
        DEFINE VARIABLE iEmiId     AS INTEGER   NO-UNDO.
        
        iEmiId = (IF oField:Has("EMIID")  THEN oField:GetInteger("EMIID") ELSE 0).
        dEmiAmount = (IF oField:Has("EMIAmount")  THEN oField:GetDecimal("EMIAmount") ELSE 0.00).
       // cStatus = "*" + STRING(iEmiId) + "*".
        DO TRANSACTION:
            FOR EACH  LoanAccountDetail EXCLUSIVE-LOCK  WHERE LoanAccountDetail.EMIID = iEmiId BREAK BY LoanAccountDetail.Balance :
                IF LAST-OF (LoanAccountDetail.Balance) THEN 
                DO:
                    dAmount = LoanAccountDetail.Balance.
                END.
            END. 
            FOR EACH  LoanEMIDetail EXCLUSIVE-LOCK  WHERE LoanEMIDetail.EMIID = iEmiId BREAK BY LoanEMIDetail.EMINum :
                IF LAST-OF (LoanEMIDetail.EMINum) THEN 
                DO:
                    ieminum = LoanEMIDetail.EMINum.
                END.
            END.
            IF dEmiAmount > dAmount THEN
            DO:
                cStatus = "Paying More Amount".
                RETURN cStatus.
            END.
            ELSE
                dAmount = dAmount - dEmiAmount .
                
            IF ieminum = 0 THEN
            DO:
                ieminum = ((iEmiId - 1000 ) * 100)  + 1 .
             //   cStatus = cStatus + "emiid" + STRING(ieminum).
            END.
            ELSE
            DO:
                ieminum = ieminum + 1.
             //   cStatus = cStatus + "emiid" + STRING(ieminum).
            END.
            
         
            DO TRANSACTION:    
                CREATE LoanEMIDetail.
                ASSIGN
                    LoanEMIDetail.EMIID            = iEmiId
                    LoanEMIDetail.EMINum           = ieminum
                    LoanEMIDetail.EMIDate          = (IF oField:Has("EMIDate")  THEN oField:GetDate("EMIDate") ELSE ?)
                    LoanEMIDetail.EMIAmount        = (IF oField:Has("EMIAmount")  THEN oField:GetDecimal("EMIAmount") ELSE 0.00)
                    LoanEMIDetail.RemainingBalance = dAmount.
                IF dAmount <> 0 THEN
                    LoanEMIDetail.EMIStatus = "Active".
                ELSE
                    LoanEMIDetail.EMIStatus = "Closed".
                
            END.
            
            FIND FIRST LoanAccountDetail 
                WHERE LoanAccountDetail.EMIID = iEmiId EXCLUSIVE-LOCK NO-ERROR.    
            IF AVAILABLE LoanAccountDetail THEN
            DO TRANSACTION:
                ASSIGN 
                    LoanAccountDetail.Balance = dAmount.
                    
               // cStatus = cStatus + STRING(dAmount).   
            END.
            cStatus = "Successfully Payed EMI....".
            
            
    
        END.

        RETURN cStatus.
        
        CATCH e AS Progress.Lang.Error:

           


        END CATCH.
        

    END METHOD.

    DESTRUCTOR PUBLIC LoanEMIDetailInfo ( ):

    END DESTRUCTOR.

END CLASS.