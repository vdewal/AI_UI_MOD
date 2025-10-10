 
/*------------------------------------------------------------------------
   File        : LoanAccountInfo
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Wed Jan 26 10:09:35 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS LoanAccountInfo INHERITS MyQuery: 
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

    CONSTRUCTOR PUBLIC LoanAccountInfo (  ):
        SUPER ().
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getLoanActDetails( INPUT iAcctNum AS INTEGER ):
        
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

        THIS-OBJECT:BufferHandle = BUFFER LoanAccountDetail:HANDLE.
        THIS-OBJECT:WhereClause = "WHERE LoanAccountDetail.AcctNum =" + QUOTER (iAcctNum).
        
        
        lValidQuery = populateQuery().
        IF lValidQuery THEN
        DO:
            oJsonArr = NEW JsonArray().
            THIS-OBJECT:QueryHandle:QUERY-OPEN().
            DO WHILE getNext():
                oInnerJsonObj = NEW JsonObject().
            
                DO iCount = 1 TO BufferHandle:NUM-FIELDS:
                    cFieldName = BufferHandle:BUFFER-FIELD (iCount):NAME.
                    oInnerJsonObj:ADD(cFieldName,BufferHandle:BUFFER-FIELD (iCount):BUFFER-VALUE ()).
                    IF cFieldName = "LoanAccountTypeID" THEN
                    DO:
                        iAcctTypeId = BufferHandle:BUFFER-FIELD (iCount):BUFFER-VALUE().
                        oInnerJsonObj:ADD("AccntSubType",oAccountType:getAcctSubTypeByAcctTypeId(INPUT iAcctTypeId)).
                    END.
                    
                END.
                oInnerJsonObj:ADD("AccntType","Loan Account").
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

    METHOD PUBLIC CHARACTER addLoanAccount( OUTPUT iAccntNum AS INTEGER,INPUT oField AS JsonObject   ):
        
        DEFINE VARIABLE cStatus      AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE AccntSubType AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE oAcctType    AS AccountTypeInfo NO-UNDO.
        DO TRANSACTION:
            /*            FIND FIRST ttCustAccntDetails NO-LOCK NO-ERROR.*/
            /*            IF AVAILABLE ttCustAccntDetails THEN           */
            /*                                                           */
            /*            DO:                                            */
            /*                FIND FIRST LoanAccountDetail                                            */
            /*                    WHERE LoanAccountDetail.AcctNum = iAccntNum EXCLUSIVE-LOCK NO-ERROR.*/
            AccntSubType =(IF oField:Has("AccntSubType")  THEN oField:GetCharacter("AccntSubType") ELSE "").
            oAcctType = NEW AccountTypeInfo().
                   

            CREATE LoanAccountDetail.
            ASSIGN
            //    LoanAccountDetail.AcctNum           = iAccntNum
                LoanAccountDetail.Balance           = (IF oField:Has("TotalLoanAmt")  THEN oField:GetInteger("TotalLoanAmt") ELSE 0)
                LoanAccountDetail.BranchCode        = INT(IF oField:Has("IFSCCode")  THEN oField:GetCharacter("IFSCCode") ELSE "")
                LoanAccountDetail.LoanAccountTypeID = oAcctType:getAcctTypeId("Loan Account", AccntSubType)
                LoanAccountDetail.LoanDuration      = (IF oField:Has("LoanDuration")  THEN oField:GetInteger("LoanDuration") ELSE 0) * 12
                LoanAccountDetail.RateOfInterest    = (IF oField:Has("RateofInt")  THEN oField:GetDecimal("RateofInt") ELSE 0.00)
                LoanAccountDetail.EMIID             = (LoanAccountDetail.AcctNum - 234000) + 1000
                LoanAccountDetail.TotalLoanAmount   = (IF oField:Has("TotalLoanAmt")  THEN oField:GetInteger("TotalLoanAmt") ELSE 0).

            iAccntNum = LoanAccountDetail.AcctNum.
            cStatus = " Loan Account Opened ".

                    
                


       //     END.
            


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

    METHOD PUBLIC LONGCHAR getNextLoanAcctNum(  ):
        
        DEFINE VARIABLE lcResult AS LONGCHAR NO-UNDO.
        THIS-OBJECT:SeqName = "LoanAcctNumSeq".
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

    METHOD PUBLIC LONGCHAR updateLoanAccount( INPUT oField AS JsonObject ):
        
        DEFINE VARIABLE cStatus      AS LONGCHAR        NO-UNDO.
        DEFINE VARIABLE cAcctNum     AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE cAcctType    AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE cAcctSubType AS CHARACTER       NO-UNDO.
        DEFINE VARIABLE oAcctType    AS AccountTypeInfo NO-UNDO.
        
        oAcctType = NEW AccountTypeInfo().
        cAcctNum = (IF oField:Has("AcctNum")  THEN oField:GetCharacter("AcctNum") ELSE "").
        cAcctType = (IF oField:Has("AccntType")  THEN oField:GetCharacter("AccntType") ELSE "").
        cAcctSubType = (IF oField:Has("AccntSubType")  THEN oField:GetCharacter("AccntSubType") ELSE "").
      
        FIND FIRST LoanAccountDetail 
            WHERE LoanAccountDetail.AcctNum = INT(cAcctNum) EXCLUSIVE-LOCK NO-ERROR.
            
        IF AVAILABLE LoanAccountDetail THEN
        DO TRANSACTION:
            ASSIGN 
                LoanAccountDetail.LoanAccountTypeID = oAcctType:getAcctTypeId(INPUT cAcctType, INPUT cAcctSubType)
                LoanAccountDetail.BranchCode        = INT(IF oField:Has("BranchCode")  THEN oField:GetCharacter("BranchCode") ELSE "")
                LoanAccountDetail.LoanDuration      = (IF oField:Has("LoanDuration")  THEN oField:GetInteger("LoanDuration") ELSE 0)*(12)
                LoanAccountDetail.RateOfInterest    = (IF oField:Has("RateOfInterest")  THEN oField:GetDecimal("RateOfInterest") ELSE 0.00)
                LoanAccountDetail.TotalLoanAmount   = (IF oField:Has("TotalLoanAmount")  THEN oField:GetInteger("TotalLoanAmount") ELSE 0).
                
            cStatus = "Loan Account updated successfully".
            
        END.
        ELSE
        DO:
            cStatus = "Loan Account record locked by other user".
        END.    
      
        
        RETURN cStatus.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC LoanAccountInfo ( ):

    END DESTRUCTOR.

END CLASS.