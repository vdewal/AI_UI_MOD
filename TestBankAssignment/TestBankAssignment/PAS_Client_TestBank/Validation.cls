 
/*------------------------------------------------------------------------
   File        : Validation
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Thu Dec 30 18:02:13 IST 2021
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Validation: 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC Validation (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL emailValidation( INPUT cEmail AS CHARACTER ):
      
        DEFINE VARIABLE iChar   AS INTEGER.
        DEFINE VARIABLE iLength AS INTEGER.
        DEFINE VARIABLE cLeft   AS CHARACTER FORMAT "x(50)" NO-UNDO .
        DEFINE VARIABLE cRight  AS CHARACTER FORMAT "x(50)" NO-UNDO .
        DEFINE VARIABLE iAt     AS INTEGER.
        DEFINE VARIABLE iDot    AS INTEGER.
        
        IF cEmail = "" THEN
            RETURN TRUE.
        
        ASSIGN
            iLength = LENGTH(cEmail)
            iAt     = INDEX(cEmail, "@")
            cLeft   = SUBSTRING (cEmail, 1, (iAt - 1))
            cRight  = SUBSTRING(cEmail, (iAt + 1), (iLength - (iAt )))
            iDot    = INDEX(cRight,".").
        
        IF iLength < 5 THEN 
        DO:
            RETURN FALSE.
        END.

        IF iAt = 0 OR iDot = 0 OR LENGTH(cLeft) = 0 OR LENGTH(cRight) = 0 THEN
        DO:
            RETURN FALSE.
        END.

        DO iChar = 1 TO LENGTH(cLeft) :
            IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-_" , CAPS(SUBSTRING(cLeft,iChar,1))) = 0 THEN
            DO:
                RETURN FALSE.
            END.
        END.
       
        DO iChar = 1 TO LENGTH(cRight) :
            IF INDEX("ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-" , CAPS(SUBSTRING(cRight,iChar,1))) = 0 THEN
            DO:
                RETURN FALSE.
            END.
        END.
       
        RETURN TRUE. 
       
        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL nameValidation( INPUT cName AS CHARACTER ):
        
        DEFINE VARIABLE iCount  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iValue  AS INTEGER NO-UNDO.
        DEFINE VARIABLE cResult AS LOgical NO-UNDO.
        
        IF cName = "" THEN
            RETURN TRUE.
        
        DO iCount = 1 to LENGTH(cName):
            iValue = ASC(SUBSTRING (cName,iCount,1)).
            
            IF  ((iValue >=65 AND iValue <= 90) OR (iValue >= 97 AND iValue <= 122)) THEN
            DO:
                cResult = TRUE.
            END.
            ELSE
            DO:
                cResult = FALSE.
            /*                MESSAGE "Invalid Name"*/
            /*                    VIEW-AS ALERT-BOX.*/
            /*                LEAVE.                */
            END.
        END.
        RETURN cResult.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL phoneNumValidation( INPUT cNumber AS CHARACTER ):
        
        DEFINE VARIABLE iCount  AS INTEGER NO-UNDO.
        DEFINE VARIABLE iValue  AS INTEGER NO-UNDO.
        DEFINE VARIABLE cResult AS LOgical NO-UNDO.
        
        IF cNumber = "" THEN
            RETURN TRUE.
        IF(LENGTH (cNumber) = 10) THEN
        DO iCount = 1  to LENGTH (cNumber):  
            iValue = ASC(SUBSTRING (cNumber,iCount,1)).
            IF (iValue >= 48 AND iValue <= 57) THEN
            DO:
                cResult = TRUE.   
            END.
            ELSE
            DO:
                cResult = FALSE.
                /*                MESSAGE "PHONE NUMBER IS INVALID"*/
                /*                    VIEW-AS ALERT-BOX.           */
                LEAVE.
            END.        
        END.
        ELSE
            /*            MESSAGE "NUMBER IS OUT OF RANGE"*/
            /*                VIEW-AS ALERT-BOX.          */
            /*                                            */
            RETURN cResult.
        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC Validation ( ):

    END DESTRUCTOR.

END CLASS.