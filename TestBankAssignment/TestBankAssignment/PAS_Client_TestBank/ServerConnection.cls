 
/*------------------------------------------------------------------------
   File        : ServerConnection
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Tue Jan 18 09:27:11 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS ServerConnection: 

    DEFINE PUBLIC PROPERTY hAppServerHandle AS HANDLE NO-UNDO 
        GET.
        SET. 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC ServerConnection (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL getConnect(  ):
        
        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
     
        CREATE SERVER hAppServerHandle.

        lResult = hAppServerHandle:CONNECT ("-sessionModel Session-managed -URL             http://192.168.0.102:8810/apsv").

        RETURN lResult.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LOGICAL getDisconnect( ):
        
        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.

        lResult = hAppServerHandle:DISCONNECT ().
        

        RETURN lResult.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:
         //   IF VALID-OBJECT (hAppServerHandle) THEN DELETE OBJECT hAppServerHandle.

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC ServerConnection ( ):
          IF VALID-OBJECT (hAppServerHandle) THEN DELETE OBJECT hAppServerHandle.

    END DESTRUCTOR.

END CLASS.