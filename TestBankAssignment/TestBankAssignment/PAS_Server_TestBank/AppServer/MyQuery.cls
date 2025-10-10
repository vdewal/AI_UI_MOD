 
/*------------------------------------------------------------------------
   File        : MyQuery
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Wed Jan 05 16:08:34 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS MyQuery ABSTRACT: 
    DEFINE  PROTECTED  PROPERTY BufferHandle AS HANDLE NO-UNDO 
        GET.
        SET.

    DEFINE PROTECTED PROPERTY DatabaseName AS CHARACTER NO-UNDO 
        GET.
        SET. 

    /*    DEFINE PUBLIC PROPERTY includeFields AS CHARACTER NO-UNDO*/
    /*    GET.                                                     */
    /*    SET.                                                     */

    DEFINE PROTECTED PROPERTY QueryHandle AS HANDLE NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY SeqName AS CHARACTER NO-UNDO 
        GET.
        SET. 

    DEFINE PROTECTED PROPERTY WhereClause AS CHARACTER NO-UNDO 
        GET.
        SET. 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC MyQuery (  ):
        SUPER ().
        
    END CONSTRUCTOR.
    
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PROTECTED CHARACTER getField( INPUT cTableName AS CHARACTER,INPUT cWhereClause AS CHARACTER,INPUT cFieldName AS CHARACTER ):
        
        DEFINE VARIABLE cResult    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE hTableName AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hField     AS HANDLE    NO-UNDO.
        ASSIGN 
            cWhereClause = SUBSTITUTE (" WHERE &1 ",cWhereClause).
        CREATE BUFFER hTableName FOR TABLE cTableName.
        
        hTableName:FIND-FIRST (cWhereClause, NO-LOCK) NO-ERROR.
        IF hTableName:AVAILABLE THEN
        DO:
            ASSIGN 
                hField  = hTableName:BUFFER-FIELD(cFieldName)
                cResult = ""   .
            IF VALID-HANDLE (hField) THEN 
            DO:               
                cResult = hField:BUFFER-VALUE ().              
              
            END.
                
        END.
        
        RETURN cResult.

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW NEW Progress.Lang.AppError (e:GetMessage(1)).

        END CATCH.

        FINALLY:
            IF VALID-OBJECT(hTableName) THEN DELETE OBJECT hTableName NO-ERROR.

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PROTECTED LOGICAL getNext(  ):
        
        DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
        
        IF VALID-HANDLE(THIS-OBJECT:QueryHandle) THEN
        DO:
            lResult = THIS-OBJECT:QueryHandle:GET-NEXT().
        END.    

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

    METHOD PROTECTED INTEGER getNextSeq(  ):
        
        DEFINE VARIABLE cResult AS INTEGER NO-UNDO.
        
        cResult = DYNAMIC-NEXT-VALUE (THIS-OBJECT:SeqName,THIS-OBJECT:DatabaseName).

        RETURN cResult.

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW NEW Progress.Lang.AppError (e:GetMessage(1)).

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PROTECTED LOGICAL isValidRecord( INPUT cTableName AS CHARACTER ):
        
        DEFINE VARIABLE lResult    AS LOGICAL NO-UNDO.
        DEFINE VARIABLE hTableName AS HANDLE  NO-UNDO.
        
        CREATE BUFFER hTableName FOR TABLE cTableName.
        hTableName:FIND-FIRST (WhereClause, NO-LOCK) NO-ERROR.
        lResult = hTableName:AVAILABLE.

        RETURN lResult.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:
            IF VALID-OBJECT (hTableName) THEN DELETE OBJECT hTableName.

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PROTECTED LOGICAL populateQuery( ):
     
        DEFINE VARIABLE cQueryString AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTableName   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lValidQuery  AS LOGICAL   NO-UNDO.
        
        ASSIGN 
            cTableName = THIS-OBJECT:BufferHandle:NAME.
     
        CREATE QUERY THIS-OBJECT:QueryHandle.
        THIS-OBJECT:QueryHandle:SET-BUFFERS(THIS-OBJECT:BufferHandle).
        cQueryString = SUBSTITUTE (" FOR EACH &1 &2 ",cTableName,THIS-OBJECT:WhereClause).
        
        /*        MESSAGE cQueryString  */
        /*            VIEW-AS ALERT-BOX.*/
        
        lValidQuery = THIS-OBJECT:QueryHandle:QUERY-PREPARE (cQueryString) NO-ERROR.
        
        RETURN lValidQuery.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:
            
         
        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC MyQuery ( ):
        IF VALID-OBJECT (BufferHandle) THEN DELETE OBJECT BufferHandle.
        IF VALID-OBJECT (QueryHandle) THEN DELETE OBJECT QueryHandle.
        

    END DESTRUCTOR.

END CLASS.