 
/*------------------------------------------------------------------------
   File        : CustomerAccounts
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Tue Jan 18 13:19:31 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS CustomerAccountsInfo INHERITS MyQuery:
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

    CONSTRUCTOR PUBLIC CustomerAccountsInfo (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR addCustomerAccount( INPUT iCustId AS INTEGER,INPUT cAcctType AS CHARACTER,INPUT oField AS JsonObject  ):
        
        DEFINE VARIABLE cStatus      AS LONGCHAR          NO-UNDO.
        DEFINE VARIABLE iAccntNum    AS INTEGER           NO-UNDO.
        DEFINE VARIABLE cStatus1     AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE AccntSubType AS CHARACTER         NO-UNDO.
        DEFINE VARIABLE oSavAcct     AS SavingAccountInfo NO-UNDO.
        DEFINE VARIABLE oAcctType    AS AccountTypeInfo   NO-UNDO.
        DEFINE VARIABLE oLoanAcct    AS LoanAccountInfo   NO-UNDO.
       
        DO TRANSACTION:
            /*            FIND FIRST ttCustAccntDetails NO-LOCK NO-ERROR.*/
            /*            IF AVAILABLE ttCustAccntDetails THEN           */
            /*                                                           */
            /*            DO:                                            */
            /*                FIND FIRST CustomerAccounts                                         */
            /*                    WHERE CustomerAccounts.CustId = iCustId EXCLUSIVE-LOCK NO-ERROR.*/
               
            oAcctType = NEW AccountTypeInfo().
            AccntSubType =(IF oField:Has("AccntSubType")  THEN oField:GetCharacter("AccntSubType") ELSE "").
            IF cAcctType = "Savings Account" THEN
            DO:
                oSavAcct = NEW SavingAccountInfo().
/*                iAccntNum = INT( oSavAcct:getNextSavingAcctNum()).*/
/*                MESSAGE iAccntNum                                 */
/*                    VIEW-AS ALERT-BOX.                            */

                cStatus1 = oSavAcct:addSavingAccount(OUTPUT iAccntNum, INPUT oField ).

                CREATE CustomerAccounts.
                ASSIGN
                    CustomerAccounts.CustId        = iCustId
                    CustomerAccounts.AcctNum       = iAccntNum
                    //CustomerAccounts.AccountTypeID = oAcctType:getAcctTypeId(cAcctType, ttCustAccntDetails.AccntSubType).
                    CustomerAccounts.AccountTypeID = oAcctType:getAcctTypeId(cAcctType,AccntSubType).
                cStatus = "New Account With" + cStatus1 + "Successfully with AccntNum = " + QUOTER (iAccntNum) .

            END.
            IF cAcctType = "Loan Account" THEN
            DO:
                oLoanAcct = NEW LoanAccountInfo().
/*                iAccntNum = INT( oLoanAcct:getNextLoanAcctNum()).*/
/*                MESSAGE iAccntNum                                */
/*                    VIEW-AS ALERT-BOX.                           */
                        
                   
                cStatus1 = oLoanAcct:addLoanAccount(OUTPUT iAccntNum, INPUT oField ).

                CREATE CustomerAccounts.
                ASSIGN 
                    CustomerAccounts.CustId        = iCustId 
                    CustomerAccounts.AcctNum       = iAccntNum
                    CustomerAccounts.AccountTypeID = oAcctType:getAcctTypeId(cAcctType,AccntSubType).
              
                cStatus = "New Account With" + cStatus1 + "Successfully with AccntNum = " + QUOTER (iAccntNum).
                
            END.
                    
               

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

    METHOD PUBLIC LONGCHAR deleteAccount( INPUT iAcctNum AS INTEGER,INPUT cAcctType AS CHARACTER ):
        
        DEFINE VARIABLE lcStatus AS LONGCHAR NO-UNDO.
        DO TRANSACTION:
            IF cAcctType = "Savings Account" THEN
            DO:
                FOR EACH SavingAccountDetail WHERE SavingAccountDetail.AcctNum = iAcctNum:
                    FOR EACH SavingAccountTxnHistory WHERE SavingAccountTxnHistory.AcctNum = iAcctNum:
                        DELETE SavingAccountTxnHistory.
                    END.
                    DELETE SavingAccountDetail.
                END.
            END.
        
            IF cAcctType = "Loan Account" THEN
            DO:
                FOR EACH LoanAccountDetail WHERE LoanAccountDetail.AcctNum = iAcctNum:
                    DEFINE VARIABLE iEMIId AS INTEGER NO-UNDO.
                    iEMIId = LoanAccountDetail.EMIID.
                
                    FOR EACH LoanEMIDetail WHERE LoanEMIDetail.EMIID = iEMIId:
                        DELETE LoanEMIDetail.
                    END.
                    DELETE LoanAccountDetail.
                END.
            END.
        
            FOR EACH CustomerAccounts WHERE CustomerAccounts.AcctNum = iAcctNum:
                DELETE CustomerAccounts.
            END.
        
            lcStatus = "Account Deleted".
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

    METHOD PUBLIC LONGCHAR getCustomerAccountDetails( INPUT iCustId AS INTEGER ):
       
        /*        DEFINE VARIABLE lcCustList      AS LONGCHAR        NO-UNDO.                                            */
        /*        DEFINE VARIABLE iCount          AS INTEGER         NO-UNDO.                                            */
        /*        DEFINE VARIABLE hCustAcctHandle AS HANDLE          NO-UNDO.                                            */
        /*        DEFINE VARIABLE oInnerJsonObj   AS JsonObject      NO-UNDO.                                            */
        /*        DEFINE VARIABLE oOuterJsonObj   AS JsonObject      NO-UNDO.                                            */
        /*        DEFINE VARIABLE oJsonArr        AS JsonArray       NO-UNDO.                                            */
        /*        DEFINE VARIABLE cFieldName      AS CHARACTER       NO-UNDO.                                            */
        /*        DEFINE VARIABLE oAccountType    AS AccountTypeInfo NO-UNDO.                                            */
        /*        DEFINE VARIABLE iAcctTypeId     AS INTEGER         NO-UNDO.                                            */
        /*                                                                                                               */
        /*        oAccountType = NEW AccountTypeInfo().                                                                  */
        /*                                                                                                               */
        /*        hCustAcctHandle = BUFFER CustomerAccounts:HANDLE.                                                      */
        /*                                                                                                               */
        /*        oJsonArr = NEW JsonArray().                                                                            */
        /*        FOR EACH CustomerAccounts WHERE CustomerAccounts.CustId = iCustId NO-LOCK:                             */
        /*            oInnerJsonObj = NEW JsonObject().                                                                  */
        /*                                                                                                               */
        /*            DO iCount = 1 TO hCustAcctHandle:NUM-FIELDS:                                                       */
        /*                cFieldName = hCustAcctHandle:BUFFER-FIELD (iCount):NAME.                                       */
        /*                oInnerJsonObj:ADD(cFieldName,hCustAcctHandle:BUFFER-FIELD (iCount):BUFFER-VALUE ()).           */
        /*                IF cFieldName = "AccountTypeID" THEN                                                           */
        /*                DO:                                                                                            */
        /*                    iAcctTypeId = hCustAcctHandle:BUFFER-FIELD (iCount):BUFFER-VALUE().                        */
        /*                    oInnerJsonObj:ADD("AccountType",oAccountType:getAccountTypeDescription(INPUT iAcctTypeId)).*/
        /*                END.                                                                                           */
        /*            END.                                                                                               */
        /*                                                                                                               */
        /*            oJsonArr:ADD(oInnerJsonObj).                                                                       */
        /*                                                                                                               */
        /*                                                                                                               */
        /*        END.                                                                                                   */
        /*        oOuterJsonObj = NEW JsonObject().                                                                      */
        /*        oOuterJsonObj:ADD("ttCustomerAccount",oJsonArr).                                                       */
        /*        oOuterJsonObj:WRITE(lcCustList,TRUE,"utf-8").                                                          */
        
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

        THIS-OBJECT:BufferHandle = BUFFER CustomerAccounts:HANDLE.
        THIS-OBJECT:WhereClause = "WHERE CustomerAccounts.CustId =" + QUOTER (iCustId).
        
        
        lValidQuery = populateQuery().
        IF lValidQuery THEN
        DO:
            oJsonArr = NEW JsonArray().
            THIS-OBJECT:QueryHandle:QUERY-OPEN().
            DO WHILE getNext():
                oInnerJsonObj = NEW JsonObject().
                oInnerJsonObj:ADD("SelectRow",FALSE).
                DO iCount = 1 TO BufferHandle:NUM-FIELDS:
                    cFieldName = BufferHandle:BUFFER-FIELD (iCount):NAME.
                    oInnerJsonObj:ADD(cFieldName,BufferHandle:BUFFER-FIELD (iCount):BUFFER-VALUE ()).
                    IF cFieldName = "AccountTypeID" THEN
                    DO:
                        iAcctTypeId = BufferHandle:BUFFER-FIELD (iCount):BUFFER-VALUE().
                        oInnerJsonObj:ADD("AccountType",oAccountType:getAccountTypeDescription(INPUT iAcctTypeId)).
                    END.
                END.
                oJsonArr:ADD(oInnerJsonObj).
            END.
            
            oOuterJsonObj = NEW JsonObject(). 
            oOuterJsonObj:ADD("ttCustomerAccount",oJsonArr).
            oOuterJsonObj:WRITE(lcCustList,TRUE,"UTF-8").
            THIS-OBJECT:QueryHandle:QUERY-CLOSE ().
        END.
        
        RETURN lcCustList.
         
        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:
            IF VALID-OBJECT (oAccountType) THEN DELETE OBJECT oAccountType.

        END FINALLY.

    END METHOD.

   

    DESTRUCTOR PUBLIC CustomerAccountsInfo ( ):

    END DESTRUCTOR.

END CLASS.