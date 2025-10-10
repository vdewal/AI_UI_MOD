 
/*------------------------------------------------------------------------
   File        : SavingAccountInfo
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Tue Jan 25 08:44:04 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS SavingAccountInfo INHERITS MyQuery: 
    /*    DEFINE TEMP-TABLE ttCustAccntDetails*/
    /*        FIELD AccntType     AS CHARACTER*/
    /*        FIELD AccntSubType  AS CHARACTER*/
    /*        FIELD IFSCCode      AS CHARACTER*/
    /*        FIELD RateofInt     AS DECIMAL  */
    /*        FIELD LoanDuration  AS INTEGER  */
    /*        FIELD TotalLoanAmt  AS INTEGER  */
    /*        FIELD TransferLimit AS INTEGER. */

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC SavingAccountInfo (  ):
        SUPER ().
        
    END CONSTRUCTOR.
    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER addSavingAccount(OUTPUT iAccntNum AS INTEGER,INPUT oField AS JsonObject  ):
        
        DEFINE VARIABLE cStatus      AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE AccntSubType AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE oAcctType    AS AccountTypeInfo NO-UNDO.
        DO TRANSACTION:
            /*            FIND FIRST ttCustAccntDetails NO-LOCK NO-ERROR.*/
            /*            IF AVAILABLE ttCustAccntDetails THEN           */
            /*                                                           */
            /*            DO:                                            */
            /*                FIND FIRST SavingAccountDetail                                            */
            /*                    WHERE SavingAccountDetail.AcctNum = iAccntNum EXCLUSIVE-LOCK NO-ERROR.*/
            /*                IF NOT AVAILABLE SavingAccountDetail THEN*/
               
            oAcctType = NEW AccountTypeInfo().
            AccntSubType =(IF oField:Has("AccntSubType")  THEN oField:GetCharacter("AccntSubType") ELSE "").

            CREATE SavingAccountDetail.
            ASSIGN
              //  SavingAccountDetail.AcctNum         = iAccntNum
                SavingAccountDetail.Balance         = 0
                SavingAccountDetail.BranchCode      = INT(IF oField:Has("IFSCCode")  THEN oField:GetCharacter("IFSCCode") ELSE "")
                SavingAccountDetail.SavingAccTypeID = oAcctType:getAcctTypeId("Savings Account",AccntSubType)
                SavingAccountDetail.TransferLimit   = (IF oField:Has("TransferLimit")  THEN oField:GetInteger("TransferLimit") ELSE 0).
                
            iAccntNum = SavingAccountDetail.AcctNum .

            cStatus = " Saving Account Opened ".

                    
                
                
        //    END.

        END.

        RETURN cStatus.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.
    
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getNextSavingAcctNum(  ):
        
        DEFINE VARIABLE lcResult AS LONGCHAR NO-UNDO.
        THIS-OBJECT:SeqName = "SavingAcctNumSeq".
        THIS-OBJECT:DatabaseName = "MyTestBank".
        lcResult = STRING(getNextSeq()).
        
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

    METHOD PUBLIC LONGCHAR getSavingActDetails( INPUT iAcctNum AS INTEGER ):
        
        DEFINE VARIABLE lcCustList    AS LONGCHAR        NO-UNDO.
        DEFINE VARIABLE iCount        AS INTEGER         NO-UNDO.
        DEFINE VARIABLE hCustHandle   AS HANDLE          NO-UNDO.
        DEFINE VARIABLE oInnerJsonObj AS JsonObject      NO-UNDO.
        DEFINE VARIABLE oOuterJsonObj AS JsonObject      NO-UNDO.
        DEFINE VARIABLE oJsonArr      AS JsonArray       NO-UNDO.
        DEFINE VARIABLE cFieldName    AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE lValidQuery   AS LOGICAL         NO-UNDO.
        DEFINE VARIABLE oAccountType  AS AccountTypeInfo NO-UNDO.
        DEFINE VARIABLE iAcctTypeId   AS INTEGER         NO-UNDO.
       
        oAccountType = NEW AccountTypeInfo().

        THIS-OBJECT:BufferHandle = BUFFER SavingAccountDetail:HANDLE.
        THIS-OBJECT:WhereClause = "WHERE SavingAccountDetail.AcctNum =" + QUOTER (iAcctNum).
        
        
        lValidQuery = populateQuery().
        IF lValidQuery THEN
        DO:
            oJsonArr = NEW JsonArray().
            THIS-OBJECT:QueryHandle:QUERY-OPEN().
            DO WHILE getNext():
                oInnerJsonObj = NEW JsonObject().
            //    oInnerJsonObj:ADD("SelectRow",FALSE).
                DO iCount = 1 TO BufferHandle:NUM-FIELDS:
                    cFieldName = BufferHandle:BUFFER-FIELD (iCount):NAME.
                    oInnerJsonObj:ADD(cFieldName,BufferHandle:BUFFER-FIELD (iCount):BUFFER-VALUE ()).
                    IF cFieldName = "SavingAccTypeID" THEN
                    DO:
                        iAcctTypeId = BufferHandle:BUFFER-FIELD (iCount):BUFFER-VALUE().
                        oInnerJsonObj:ADD("AccntSubType",oAccountType:getAcctSubTypeByAcctTypeId(INPUT iAcctTypeId)).
                    END.
                END.
                oInnerJsonObj:ADD("AccntType","Savings Account").
                oJsonArr:ADD(oInnerJsonObj).
            END.
            
            oOuterJsonObj = NEW JsonObject(). 
            oOuterJsonObj:ADD("ttCustAccntDetails",oJsonArr).
            oOuterJsonObj:WRITE(lcCustList,TRUE,"UTF-8").
            THIS-OBJECT:QueryHandle:QUERY-CLOSE ().
        END.
        
        RETURN lcCustList.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.
    
    /*------------------------------------------------------------------------------
  Purpose:
  Notes:
 ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR updateSavingAccount( INPUT oField AS JsonObject ):
        
        DEFINE VARIABLE cStatus      AS LONGCHAR        NO-UNDO.
        DEFINE VARIABLE cAcctNum     AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE cAcctType    AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE cAcctSubType AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE oAcctType    AS AccountTypeInfo NO-UNDO.
        
        oAcctType = NEW AccountTypeInfo().
        
      
        cAcctNum =(IF oField:Has("AcctNum")  THEN oField:GetCharacter("AcctNum") ELSE "").
        cAcctType = (IF oField:Has("AccntType")  THEN oField:GetCharacter("AccntType") ELSE "").
        cAcctSubType = (IF oField:Has("AccntSubType")  THEN oField:GetCharacter("AccntSubType") ELSE "").
   
        FIND FIRST SavingAccountDetail 
            WHERE SavingAccountDetail.AcctNum = INT(cAcctNum) EXCLUSIVE-LOCK NO-ERROR.    
        IF AVAILABLE SavingAccountDetail THEN
        DO TRANSACTION:
            ASSIGN 
                SavingAccountDetail.SavingAccTypeID = oAcctType:getAcctTypeId(INPUT cAcctType, INPUT cAcctSubType)
                SavingAccountDetail.BranchCode      = INT(IF oField:Has("BranchCode")  THEN oField:GetCharacter("BranchCode") ELSE "")
                SavingAccountDetail.TransferLimit   = (IF oField:Has("TransferLimit")  THEN oField:GetInteger("TransferLimit") ELSE 0).
                
            cStatus = "Savings Account updated successfully".
            
        END.
        ELSE
        DO:
            cStatus = "Savings Account record locked by other user".
        END.    
      
      
        
        RETURN cStatus.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.
    

    DESTRUCTOR PUBLIC SavingAccountInfo ( ):

    END DESTRUCTOR.

END CLASS.