 
/*------------------------------------------------------------------------
   File        : Address
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Wed Feb 09 08:34:56 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Address: 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC Address (  ):
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getCityCode( INPUT cStateCode AS CHARACTER ):
        
        DEFINE VARIABLE lcResult AS LONGCHAR NO-UNDO.
        FOR EACH City WHERE City.StateCode = cStateCode NO-LOCK:
            
            lcResult = lcResult + "," + City.CityCode.
            
        END.
        lcResult = TRIM(lcResult,",").
        
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

    METHOD PUBLIC LONGCHAR getCountryCodes(  ):
        
        DEFINE VARIABLE lcResult AS LONGCHAR NO-UNDO.
        FOR EACH Country NO-LOCK:
            
            lcResult = lcResult + "," + Country.CountryCode.
            
        END.
        lcResult = TRIM(lcResult,",").
        
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

    METHOD PUBLIC LONGCHAR getPostalCode( INPUT cCityCode AS CHARACTER ):
        
        DEFINE VARIABLE cResult AS LONGCHAR NO-UNDO.
        
        
        FIND FIRST PostalCode WHERE PostalCode.CityCode = cCityCode NO-LOCK NO-ERROR.
        IF AVAILABLE PostalCode THEN
            cResult = PostalCode.ZIPCode.
        ELSE
            cResult = "".

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

    METHOD PUBLIC LONGCHAR getStateCode( INPUT cCountryCode AS CHARACTER  ):
        
        DEFINE VARIABLE lcResult AS LONGCHAR NO-UNDO.
        FOR EACH State WHERE State.CountryCode  = cCountryCode NO-LOCK:
            
            lcResult = lcResult + "," + State.StateCode.
            
        END.
        lcResult = TRIM(lcResult,",").
        
        RETURN lcResult.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC Address ( ):

    END DESTRUCTOR.

END CLASS.