 
/*------------------------------------------------------------------------
   File        : ClientUtility
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Tue Feb 01 19:04:37 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS ClientUtility: 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC ClientUtility (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID getInputParameterStructure( INPUT cActionName AS CHARACTER,
        INPUT cInputParameterValue AS CHARACTER,
        INPUT iInputParameterValue AS INTEGER,
        INPUT hTTHandle  AS HANDLE,
        OUTPUT oInputJson AS JsonObject):
                                                           
       
        DEFINE VARIABLE oInputParametersJson AS JsonObject NO-UNDO.
        DEFINE VARIABLE oTTJson              AS JsonArray  NO-UNDO.
        
        
        oInputJson = NEW JsonObject().
        oInputParametersJson = NEW JsonObject().
        oTTJson = NEW JsonArray().
        
        oInputJson:Add("Action", cActionName) .
        oInputParametersJson:Add("cInputParameterValue", cInputParameterValue).  
        oInputParametersJson:Add("iInputParameterValue", iInputParameterValue).
        oTTJson:Read( hTTHandle ).
        oInputParametersJson:Add("ttTable", oTTJson).
        oInputJson:Add("InputParameters", oInputParametersJson).
        
        RETURN.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC ClientUtility ( ):

    END DESTRUCTOR.

END CLASS.