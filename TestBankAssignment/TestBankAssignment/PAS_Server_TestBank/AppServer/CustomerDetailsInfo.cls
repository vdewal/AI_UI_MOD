 
/*------------------------------------------------------------------------
   File        : CustomerDetails
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : Kishore.M
   Created     : Mon Jan 17 16:39:05 IST 2022
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
USING OpenEdge.Core.String FROM PROPATH.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS CustomerDetailsInfo INHERITS MyQuery: 
   
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    CONSTRUCTOR PUBLIC CustomerDetailsInfo (  ):
        SUPER ().
        
        
    END CONSTRUCTOR.
    
    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR addCustomerDetails( INPUT oField AS JsonObject ):
        
        DEFINE VARIABLE cStatus AS LONGCHAR NO-UNDO.
        BlockName:
        DO TRANSACTION ON ERROR UNDO BlockName , LEAVE BlockName: 
            CREATE CustomerDetail.
            ASSIGN 
                /* no need to use add custId
                   Scheme Triggered:
                   TRIGGER PROCEDURE FOR CREATE OF CustomerDetail.
                   ASSIGN CustomerDetail.CustId = NEXT-VALUE (CustIdSeq).
                */
                CustomerDetail.FirstName     = (IF oField:Has("FirstName")  THEN oField:GetCharacter("FirstName") ELSE "")
                CustomerDetail.LastName      = (IF oField:Has("LastName")  THEN oField:GetCharacter("LastName") ELSE "")
                CustomerDetail.Address1      = (IF oField:Has("Address1")  THEN oField:GetCharacter("Address1") ELSE "")
                CustomerDetail.Address2      = (IF oField:Has("Address2")  THEN oField:GetCharacter("Address2") ELSE "")
                CustomerDetail.DateOfBirth   = (IF oField:Has("DateOfBirth")  THEN oField:GetDate("DateOfBirth") ELSE ?)
                CustomerDetail.City          = (IF oField:Has("City")  THEN oField:GetCharacter("City") ELSE "")
                CustomerDetail.State         = (IF oField:Has("State")  THEN oField:GetCharacter("State") ELSE "")
                CustomerDetail.Country       = (IF oField:Has("Country")  THEN oField:GetCharacter("Country") ELSE "")
                CustomerDetail.ZIPCode       = (IF oField:Has("ZIPCode")  THEN oField:GetCharacter("ZIPCode") ELSE "")
                CustomerDetail.MaritalStatus = (IF oField:Has("MaritalStatus")  THEN oField:GetCharacter("MaritalStatus") ELSE "").
                             
            cStatus = "Customer Record added successfully with CustId = " + QUOTER (CustomerDetail.CustId).
            
        END.
       
        RETURN cStatus.

        CATCH e AS Progress.Lang.Error:
            UNDO , THROW NEW Progress.Lang.AppError (e:GetMessage(1),1).
            // UNDO, THROW NEW Progress.Lang.AppError("Customer Details Not Found...",1).

        END CATCH.

        FINALLY:
            IF VALID-OBJECT (oField) THEN DELETE OBJECT oField.

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR deleteCustomerDetails( INPUT iCustId AS INTEGER ):
        
        DEFINE VARIABLE lcStatus AS LONGCHAR NO-UNDO.
        FIND FIRST CustomerDetail
            WHERE CustomerDetail.CustId = iCustId EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE CustomerDetail THEN
        DO:
            DELETE CustomerDetail.
               
            lcStatus = "Deleted Sucessfully.....".
                    
        END.
        ELSE
        DO:
            lcStatus = "Not Deleted...Invalid CustId...".
                    
        END.

        RETURN lcStatus.

        CATCH e AS Progress.Lang.Error:
            UNDO , THROW NEW Progress.Lang.AppError (e:GetMessage(1),1).

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC LONGCHAR getCustomerDetailsByFilter( INPUT cWhereClause AS CHARACTER /*,INPUT cIncludeFields As CHARACTER*/):

        DEFINE VARIABLE lcCustList    AS LONGCHAR   NO-UNDO.
        DEFINE VARIABLE iCount        AS INTEGER    NO-UNDO.
        DEFINE VARIABLE hCustHandle   AS HANDLE     NO-UNDO.
        DEFINE VARIABLE oInnerJsonObj AS JsonObject NO-UNDO.
        DEFINE VARIABLE oOuterJsonObj AS JsonObject NO-UNDO.
        DEFINE VARIABLE oJsonArr      AS JsonArray  NO-UNDO.
        DEFINE VARIABLE cFieldName    AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE lValidQuery   AS LOGICAL    NO-UNDO.

        THIS-OBJECT:BufferHandle = BUFFER CustomerDetail:HANDLE.
        THIS-OBJECT:WhereClause = cWhereClause.
   //     THIS-OBJECT:includeFields = cIncludeFields.
  
        lValidQuery = populateQuery().
      //  IF lValidQuery THEN
        IF lValidQuery = NO THEN
        DO:
            MESSAGE "Invalid Query"
                VIEW-AS ALERT-BOX.
        END.
        ELSE
        DO:
            THIS-OBJECT:QueryHandle:QUERY-OPEN().
            oJsonArr = NEW JsonArray().
            DO WHILE getNext():
        //    hCustHandle = BUFFER CustomerDetail:HANDLE.
                oInnerJsonObj = NEW JsonObject(). 
                DO iCount = 1 TO BufferHandle:NUM-FIELDS:
                    cFieldName = BufferHandle:BUFFER-FIELD (iCount):NAME.
          //          IF LOOKUP (cFieldName,THIS-OBJECT:includeFields) > 0 THEN
                    oInnerJsonObj:ADD(cFieldName,BufferHandle:BUFFER-FIELD (iCount):BUFFER-VALUE()).
                END. 
                oJsonArr:ADD(oInnerJsonObj).
                
            END.
            oOuterJsonObj = NEW JsonObject().
            oOuterJsonObj:ADD("ttCustomer",oJsonArr).
            oOuterJsonObj:WRITE(lcCustList,TRUE,"UTF-8").
            THIS-OBJECT:QueryHandle:QUERY-CLOSE ().
        END.
        
        RETURN lcCustList.

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW NEW Progress.Lang.AppError("Customer Details Not Found...",1).

        END CATCH.

        FINALLY:
             IF VALID-OBJECT (oJsonArr) THEN DELETE OBJECT oJsonArr.
             IF VALID-OBJECT (oInnerJsonObj) THEN DELETE OBJECT oInnerJsonObj.
             IF VALID-OBJECT (oOuterJsonObj) THEN DELETE OBJECT oOuterJsonObj.
             

        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LONGCHAR getCustomerDetailsById(INPUT iCustId AS INTEGER):

        /*        DEFINE VARIABLE lcCustList    AS LONGCHAR   NO-UNDO.                                    */
        /*        DEFINE VARIABLE iCount        AS INTEGER    NO-UNDO.                                    */
        /*        DEFINE VARIABLE hCustHandle   AS HANDLE     NO-UNDO.                                    */
        /*        DEFINE VARIABLE oInnerJsonObj AS JsonObject NO-UNDO.                                    */
        /*        DEFINE VARIABLE oOuterJsonObj AS JsonObject NO-UNDO.                                    */
        /*        DEFINE VARIABLE oJsonArr      AS JsonArray  NO-UNDO.                                    */
        /*        DEFINE VARIABLE cFieldName    AS CHARACTER  NO-UNDO.                                    */
        /*                                                                                                */
        /*                                                                                                */
        /*        DEFINE BUFFER bCust FOR CustomerDetail.                                                 */
        /*        FIND FIRST bCust WHERE bCust.CustId = iCustId NO-LOCK NO-ERROR.                        */
        /*                                                                                                */
        /*        IF AVAILABLE bCust THEN                                                                 */
        /*        DO:                                                                                     */
        /*                                                                                                */
        /*            hCustHandle = BUFFER bCust:HANDLE.                                                  */
        /*            oInnerJsonObj = NEW JsonObject().                                                   */
        /*            oOuterJsonObj = NEW JsonObject().                                                   */
        /*            DO iCount = 1 TO hCustHandle:NUM-FIELDS:                                            */
        /*                cFieldName = hCustHandle:BUFFER-FIELD (iCount):Name.                            */
        /*                oInnerJsonObj:ADD(cFieldName,hCustHandle:BUFFER-FIELD (iCount):BUFFER-VALUE ()).*/
        /*            END.                                                                                */
        /*            oJsonArr = NEW JsonArray().                                                         */
        /*            oJsonArr:ADD(oInnerJsonObj).                                                        */
        /*            oOuterJsonObj:ADD("ttCustomer",oJsonArr).                                           */
        /*            oOuterJsonObj:WRITE(lcCustList,TRUE,"utf-8").                                       */
        /*                                                                                                */
        /*        END.                                                                                    */
        /*        ELSE                                                                                    */
        /*        DO:                                                                                     */
        /*            UNDO, THROW NEW Progress.Lang.AppError("Customer Record not found.",1).             */
        /*        END.                                                                                    */

        DEFINE VARIABLE lcCustList    AS LONGCHAR   NO-UNDO.
        DEFINE VARIABLE iCount        AS INTEGER    NO-UNDO.
        DEFINE VARIABLE hCustHandle   AS HANDLE     NO-UNDO.
        DEFINE VARIABLE oInnerJsonObj AS JsonObject NO-UNDO.
        DEFINE VARIABLE oOuterJsonObj AS JsonObject NO-UNDO.
        DEFINE VARIABLE oJsonArr      AS JsonArray  NO-UNDO.
        DEFINE VARIABLE cFieldName    AS CHARACTER  NO-UNDO.
        DEFINE VARIABLE lValidQuery   AS LOGICAL    NO-UNDO.

        THIS-OBJECT:BufferHandle = BUFFER CustomerDetail:HANDLE.
        THIS-OBJECT:WhereClause = "WHERE CustomerDetail.CustId =" + QUOTER (iCustId).
  
        lValidQuery = populateQuery().
        IF lValidQuery THEN
        DO:
            THIS-OBJECT:QueryHandle:QUERY-OPEN().
            oJsonArr = NEW JsonArray().
            DO WHILE getNext():
                oInnerJsonObj = NEW JsonObject().
                DO iCount = 1 TO BufferHandle:NUM-FIELDS:
                    cFieldName = BufferHandle:BUFFER-FIELD (iCount):NAME.
                    oInnerJsonObj:ADD(cFieldName,BufferHandle:BUFFER-FIELD (iCount):BUFFER-VALUE()).
                END.
                oJsonArr:ADD(oInnerJsonObj).
            
            END.
            oOuterJsonObj = NEW JsonObject().
            oOuterJsonObj:ADD("ttCustomer",oJsonArr).
            oOuterJsonObj:WRITE(lcCustList,TRUE,"UTF-8").
            THIS-OBJECT:QueryHandle:QUERY-CLOSE ().
           
           
        END.
        ELSE
        DO:
            UNDO, THROW NEW Progress.Lang.AppError("Customer Details Not Found...",1).
          
        END.

        RETURN lcCustList.

        CATCH e AS Progress.Lang.Error:
            DEFINE VARIABLE stest AS String NO-UNDO.
            stest = NEW string("Error in Method ").
            stest:append(THIS-OBJECT:GetClass():TypeName).
            stest:append(" ") .
            stest:append(e:GetMessage(1)).
            MESSAGE stest:ToString().

       //    UNDO , THROW NEW Progress.Lang.AppError (e:GetMessage(1),1).
       //  UNDO, THROW NEW Progress.Lang.AppError("Customer Details Not Found...",1).

        END CATCH.

        FINALLY:
             IF VALID-OBJECT (oJsonArr) THEN DELETE OBJECT oJsonArr.
             IF VALID-OBJECT (oInnerJsonObj) THEN DELETE OBJECT oInnerJsonObj.
             IF VALID-OBJECT (oOuterJsonObj) THEN DELETE OBJECT oOuterJsonObj.

        END FINALLY.



    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC INTEGER getNextCustId(  ):
        
        DEFINE VARIABLE lcResult AS INTEGER NO-UNDO.
        THIS-OBJECT:SeqName = "CustIdSeq".
        THIS-OBJECT:DatabaseName = "MyTestBank".
        lcResult = getNextSeq().
        
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

    METHOD PUBLIC LONGCHAR updateCustomerDetails( /*INPUT TABLE ttCustomer*/INPUT oField AS JsonObject ):
        
        DEFINE VARIABLE cStatus AS LONGCHAR NO-UNDO.
        DEFINE VARIABLE iCustId AS INTEGER  NO-UNDO.
        iCustId =(IF oField:Has("CustId")  THEN oField:GetInteger("CustId") ELSE 0).
      //  TEMP-TABLE ttCustomer:READ-JSON ("JsonArray",oTTJson,"Empty").
        /*        FIND FIRST ttCustomer NO-LOCK NO-ERROR.*/
        /*        IF AVAILABLE ttCustomer THEN           */
        /*        DO:                                    */
        FIND FIRST CustomerDetail 
            WHERE CustomerDetail.CustId = iCustId EXCLUSIVE-LOCK NO-ERROR.
            
        IF AVAILABLE CustomerDetail THEN
        DO TRANSACTION:
            ASSIGN 
                /* CustomerDetail.FirstName     = ttCustomer.FirstName     */
                /* CustomerDetail.LastName      = ttCustomer.LastName      */
                /* CustomerDetail.Address1      = ttCustomer.Address1      */
                /* CustomerDetail.Address2      = ttCustomer.Address2      */
                /* CustomerDetail.DateOfBirth   = ttCustomer.DateOfBirth   */
                /* CustomerDetail.City          = ttCustomer.City          */
                /* CustomerDetail.State         = ttCustomer.State         */
                /* CustomerDetail.Country       = ttCustomer.Country       */
                /* CustomerDetail.ZIPCode       = ttCustomer.ZIPCode       */
                /* CustomerDetail.MaritalStatus = ttCustomer.MaritalStatus.*/
                CustomerDetail.FirstName     = (IF oField:Has("FirstName")  THEN oField:GetCharacter("FirstName") ELSE "")
                CustomerDetail.LastName      = (IF oField:Has("LastName")  THEN oField:GetCharacter("LastName") ELSE "")
                CustomerDetail.Address1      = (IF oField:Has("Address1")  THEN oField:GetCharacter("Address1") ELSE "")
                CustomerDetail.Address2      = (IF oField:Has("Address2")  THEN oField:GetCharacter("Address2") ELSE "")
                CustomerDetail.DateOfBirth   = (IF oField:Has("DateOfBirth")  THEN oField:GetDate("DateOfBirth") ELSE ?)
                CustomerDetail.City          = (IF oField:Has("City")  THEN oField:GetCharacter("City") ELSE "")
                CustomerDetail.State         = (IF oField:Has("State")  THEN oField:GetCharacter("State") ELSE "")
                CustomerDetail.Country       = (IF oField:Has("Country")  THEN oField:GetCharacter("Country") ELSE "")
                CustomerDetail.ZIPCode       = (IF oField:Has("ZIPCode")  THEN oField:GetCharacter("ZIPCode") ELSE "")
                CustomerDetail.MaritalStatus = (IF oField:Has("MaritalStatus")  THEN oField:GetCharacter("MaritalStatus") ELSE "").
                    
             
            cStatus = "Customer Record updated successfully".
            
        END.
        ELSE
        DO:
            cStatus = "Customer record locked by other user".
        END.    
        /*        END.                             */
        /*        ELSE                             */
        /*        DO:                              */
        /*            cStatus = "No data received".*/
        /*        END.                             */
        
        RETURN cStatus.

        CATCH e AS Progress.Lang.Error:

        END CATCH.

        FINALLY:

        END FINALLY.

    END METHOD.

    DESTRUCTOR PUBLIC CustomerDetailsInfo ( ):

    END DESTRUCTOR.

END CLASS.