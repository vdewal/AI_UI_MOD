 
/*------------------------------------------------------------------------
   File        : AccountType
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Tue Jan 18 13:19:56 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

 CLASS AccountTypeInfo : 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC AccountTypeInfo (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER getAccountTypeDescription( INPUT iAcctTypeId AS INTEGER ):
        
        DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
        DEFINE VARIABLE o AS MyQuery NO-UNDO.
         o = NEW MyQuery().
        
        
        FIND FIRST AccountType WHERE AccountType.AccountTypeId = iAcctTypeId NO-LOCK NO-ERROR.
        IF AVAILABLE AccountType THEN
            cDescription = AccountType.AccountType.
        ELSE
            cDescription = "".

        RETURN cDescription.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC CHARACTER getAcctSubTypeByAcctTypeId( INPUT iAcctTypeId AS INTEGER ):
        
        DEFINE VARIABLE cAcctSubType AS CHARACTER NO-UNDO.
        
        FIND FIRST AccountType WHERE AccountType.AccountTypeId = iAcctTypeId NO-LOCK NO-ERROR.
        IF AVAILABLE AccountType THEN
            cAcctSubType = AccountType.AccSubType.
        ELSE
            cAcctSubType = "".

        RETURN cAcctSubType.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC INTEGER getAcctTypeId( INPUT cAcctType AS CHARACTER,INPUT cAccntSubType AS CHARACTER ):
        DEFINE VARIABLE iAcctTypeId AS INTEGER NO-UNDO.
        FIND FIRST AccountType WHERE AccountType.AccountType = cAcctType AND AccountType.AccSubType = cAccntSubType NO-LOCK NO-ERROR.
        IF AVAILABLE AccountType THEN
            iAcctTypeId = AccountType.AccountTypeId.
        ELSE
            iAcctTypeId = 0.

        RETURN iAcctTypeId.
        

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getAcctSubType( INPUT cAcctType AS CHARACTER ):
        
        DEFINE VARIABLE lcResult AS LONGCHAR NO-UNDO.
        
        FOR EACH AccountType WHERE AccountType.AccountType = cAcctType NO-LOCK:
            
            lcResult = lcResult + "," + AccountType.AccSubType.
            
        END.
        lcResult = TRIM(lcResult,",").
        
        RETURN lcResult.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC AccountTypeInfo ( ):

    END DESTRUCTOR.

END CLASS.