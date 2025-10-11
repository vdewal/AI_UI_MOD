&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS gDialog 
USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 

/*------------------------------------------------------------------------

  File: 

  Description: from cntnrdlg.w - ADM2 SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.


/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER iCustId AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

/*DEFINE TEMP-TABLE ttCustomer         */
/*    FIELD CustId        AS INTEGER   */
/*    FIELD FirstName     AS CHARACTER */
/*    FIELD LastName      AS CHARACTER */
/*    FIELD Address1      AS CHARACTER */
/*    FIELD Address2      AS CHARACTER */
/*    FIELD City          AS CHARACTER */
/*    FIELD State         AS CHARACTER */
/*    FIELD Country       AS CHARACTER */
/*    FIELD EmailId       AS CHARACTER */
/*    FIELD Phone         AS CHARACTER */
/*    FIELD Mobile        AS CHARACTER */
/*    FIELD DateOfBirth   AS DATE      */
/*    FIELD MaritalStatus AS CHARACTER */
/*    FIELD ZIPCode       AS CHARACTER.*/
    
DEFINE TEMP-TABLE ttCustAccntDetails 
    FIELD AccntType     AS CHARACTER
    FIELD AccntSubType  AS CHARACTER
    FIELD IFSCCode      AS CHARACTER
    FIELD RateofInt     AS DECIMAL
    FIELD LoanDuration  AS INTEGER
    FIELD TotalLoanAmt  AS INTEGER
    FIELD TransferLimit AS INTEGER. 
    
DEFINE TEMP-TABLE ttEmpty LIKE ttCustAccntDetails. 
       


{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS gDialog 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 02/10/22 - 10:14 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME gDialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-8 RECT-9 COMBO-BOX-AccntType ~
COMBO-BOX-AccntSubType FILL-IN-IFSC FILL-IN-TransferLimit ~
COMBO-BOX-LoanDuration FILL-IN-TotalLoanAmt FILL-IN-RateOfInt Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-AccntType COMBO-BOX-AccntSubType ~
FILL-IN-IFSC FILL-IN-TransferLimit COMBO-BOX-LoanDuration ~
FILL-IN-TotalLoanAmt FILL-IN-RateOfInt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 20 BY 1.13.

DEFINE BUTTON BUTTON-Create AUTO-GO 
     LABEL "Create" 
     SIZE 20 BY 1.13.

DEFINE VARIABLE COMBO-BOX-AccntSubType AS CHARACTER 
     LABEL "Acc Sub Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN
     SIZE 25 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-AccntType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Savings Account","Loan Account","Demat Account" 
     DROP-DOWN-LIST
     SIZE 26 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-LoanDuration AS CHARACTER FORMAT "X(256)":U 
     LABEL "Loan Duration(yrs)" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3","4","5","6","7","8" 
     DROP-DOWN-LIST
     SIZE 9 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-IFSC AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "IFSC Code" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-RateOfInt AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     LABEL "Rate of Interest(%)" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-TotalLoanAmt AS INT64 FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Total Loan Amount" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-TransferLimit AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Transfer Limit" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 6.5.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89 BY 4.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     COMBO-BOX-AccntType AT ROW 2.75 COL 20 COLON-ALIGNED WIDGET-ID 2
     COMBO-BOX-AccntSubType AT ROW 2.75 COL 64 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-IFSC AT ROW 4 COL 20 COLON-ALIGNED WIDGET-ID 14
     FILL-IN-TransferLimit AT ROW 4 COL 64 COLON-ALIGNED WIDGET-ID 8
     COMBO-BOX-LoanDuration AT ROW 4 COL 66 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-TotalLoanAmt AT ROW 5.5 COL 20 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-RateOfInt AT ROW 5.5 COL 64 COLON-ALIGNED WIDGET-ID 16
     BUTTON-Create AT ROW 7.25 COL 29
     Btn_Cancel AT ROW 7.25 COL 52
     "ADD NEW ACCOUNT" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 1.25 COL 35 WIDGET-ID 24
          FONT 5
     RECT-8 AT ROW 2.25 COL 2 WIDGET-ID 26
     RECT-9 AT ROW 2.5 COL 3 WIDGET-ID 28
     SPACE(1.99) SKIP(1.93)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Add Account"
         DEFAULT-BUTTON BUTTON-Create CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB gDialog 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX gDialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME gDialog:SCROLLABLE       = FALSE
       FRAME gDialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-Create IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-RateOfInt:HIDDEN IN FRAME gDialog           = TRUE.

ASSIGN 
       FILL-IN-TotalLoanAmt:HIDDEN IN FRAME gDialog           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX gDialog
/* Query rebuild information for DIALOG-BOX gDialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX gDialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME gDialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL gDialog gDialog
ON WINDOW-CLOSE OF FRAME gDialog /* Add Account */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Create
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Create gDialog
ON CHOOSE OF BUTTON-Create IN FRAME gDialog /* Create */
DO:
        
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcStatus          AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE cAcctType         AS CHARACTER        NO-UNDO. 
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        

        
        
        
        cAcctType = COMBO-BOX-AccntType:SCREEN-VALUE.
        
        CREATE ttCustAccntDetails.
        ASSIGN
            ttCustAccntDetails.AccntSubType  = COMBO-BOX-AccntSubType:SCREEN-VALUE
            ttCustAccntDetails.AccntType     = COMBO-BOX-AccntType:SCREEN-VALUE
            ttCustAccntDetails.IFSCCode      = STRING(FILL-IN-IFSC:SCREEN-VALUE)
            ttCustAccntDetails.TransferLimit = INT(FILL-IN-TransferLimit:SCREEN-VALUE)
            ttCustAccntDetails.RateofInt     = DECIMAL(FILL-IN-RateOfInt:SCREEN-VALUE)
            ttCustAccntDetails.TotalLoanAmt  = INT(FILL-IN-TotalLoanAmt:SCREEN-VALUE)
            ttCustAccntDetails.LoanDuration  = INT(COMBO-BOX-LoanDuration:SCREEN-VALUE).
    
        /*        MESSAGE ttCustAccntDetails.AccntSubType ttCustAccntDetails.TotalLoanAmt*/
        /*            VIEW-AS ALERT-BOX.                                                 */
            
        lConnectionStatus = oConnection:getConnect().
        /*        MESSAGE "Connected = "lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                 */
        oUtility:getInputParameterStructure(INPUT "AddCustomerAccount", INPUT cAcctType, INPUT iCustId, INPUT TEMP-TABLE ttCustAccntDetails:HANDLE,OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX.
        RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcStatus).
     //   RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT "AddCustomerAccount",INPUT iCustId,OUTPUT lcStatus,INPUT "",INPUT TABLE ttCustomer,INPUT TABLE ttCustAccntDetails,INPUT cAcctType,INPUT 0).
        lConnectionStatus = oConnection:getDisconnect().
        /*        MESSAGE "Disconnected = " lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                     */
       
        MESSAGE STRING(lcStatus)
            VIEW-AS ALERT-BOX.
    
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-AccntSubType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-AccntSubType gDialog
ON VALUE-CHANGED OF COMBO-BOX-AccntSubType IN FRAME gDialog /* Acc Sub Type */
DO:
    RUN validation.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-AccntType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-AccntType gDialog
ON VALUE-CHANGED OF COMBO-BOX-AccntType IN FRAME gDialog /* Account Type */
DO:
    
      
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcAccSubTypeList  AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE cAcctType         AS CHARACTER        NO-UNDO. 
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        

        
        
        
        cAcctType = COMBO-BOX-AccntType:SCREEN-VALUE.
        
        IF cAcctType = "Savings Account" THEN
        DO:
            ASSIGN
                FILL-IN-IFSC:VISIBLE           = TRUE
                FILL-IN-TransferLimit:VISIBLE  = TRUE
                COMBO-BOX-LoanDuration:VISIBLE = FALSE
                FILL-IN-RateOfInt:VISIBLE      = FALSE
                FILL-IN-TotalLoanAmt:VISIBLE   = FALSE.
        END.
        
        IF cAcctType = "Loan Account" THEN
        DO:
            ASSIGN
                FILL-IN-IFSC:VISIBLE           = TRUE
                FILL-IN-TransferLimit:VISIBLE  = FALSE
                COMBO-BOX-LoanDuration:VISIBLE = TRUE
                FILL-IN-RateOfInt:VISIBLE      = TRUE
                FILL-IN-TotalLoanAmt:VISIBLE   = TRUE.
        END.
        lConnectionStatus = oConnection:getConnect().
        /*        MESSAGE "Connected = "lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                 */
        oUtility:getInputParameterStructure(INPUT "getAcctSubType", INPUT cAcctType, INPUT 0, INPUT TEMP-TABLE ttEmpty:HANDLE,OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX.
        RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcAccSubTypeList).
       // RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT "getAcctSubType",INPUT 0,OUTPUT lcAccSubTypeList,INPUT "",INPUT TABLE ttCustomer,INPUT TABLE ttCustAccntDetails,INPUT cAcctType,INPUT 0).
        lConnectionStatus = oConnection:getDisconnect().
        /*        MESSAGE "Disconnected = " lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                     */
       
        MESSAGE STRING(lcAccSubTypeList)
            VIEW-AS ALERT-BOX.
        
    
        COMBO-BOX-AccntSubType:LIST-ITEMS = STRING(lcAccSubTypeList).
        RUN validation.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-LoanDuration
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-LoanDuration gDialog
ON VALUE-CHANGED OF COMBO-BOX-LoanDuration IN FRAME gDialog /* Loan Duration(yrs) */
DO:
RUN validation.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-IFSC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-IFSC gDialog
ON VALUE-CHANGED OF FILL-IN-IFSC IN FRAME gDialog /* IFSC Code */
DO:
RUN validation.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-RateOfInt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-RateOfInt gDialog
ON VALUE-CHANGED OF FILL-IN-RateOfInt IN FRAME gDialog /* Rate of Interest(%) */
DO:
RUN validation.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TotalLoanAmt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TotalLoanAmt gDialog
ON VALUE-CHANGED OF FILL-IN-TotalLoanAmt IN FRAME gDialog /* Total Loan Amount */
DO:
RUN validation.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TransferLimit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TransferLimit gDialog
ON VALUE-CHANGED OF FILL-IN-TransferLimit IN FRAME gDialog /* Transfer Limit */
DO:
RUN validation.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK gDialog 


/* ***************************  Main Block  *************************** */

{src/adm2/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects gDialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI gDialog  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME gDialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI gDialog  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY COMBO-BOX-AccntType COMBO-BOX-AccntSubType FILL-IN-IFSC 
          FILL-IN-TransferLimit COMBO-BOX-LoanDuration FILL-IN-TotalLoanAmt 
          FILL-IN-RateOfInt 
      WITH FRAME gDialog.
  ENABLE RECT-8 RECT-9 COMBO-BOX-AccntType COMBO-BOX-AccntSubType FILL-IN-IFSC 
         FILL-IN-TransferLimit COMBO-BOX-LoanDuration FILL-IN-TotalLoanAmt 
         FILL-IN-RateOfInt Btn_Cancel 
      WITH FRAME gDialog.
  VIEW FRAME gDialog.
  {&OPEN-BROWSERS-IN-QUERY-gDialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject gDialog 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
                             Purpose:
                             Notes:
                            ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    RUN SUPER.

    /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
     //   DISABLE BUTTON-Create.
        ASSIGN
            FILL-IN-IFSC:VISIBLE           = FALSE
            FILL-IN-RateOfInt:VISIBLE      = FALSE
            FILL-IN-TotalLoanAmt:VISIBLE   = FALSE
            FILL-IN-TransferLimit:VISIBLE  = FALSE
            COMBO-BOX-LoanDuration:VISIBLE = FALSE.

    END.
    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validation gDialog 
PROCEDURE validation :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cAccntSubType  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAccntType     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cIFSCCode      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTransferLimit AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dRateofInt     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iTotalLoanAmt  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLoanDuration  AS INTEGER   NO-UNDO.
   
    
    DO WITH FRAME {&FRAME-NAME}:         
        cAccntSubType  = COMBO-BOX-AccntSubType:SCREEN-VALUE.
        cAccntType     = COMBO-BOX-AccntType:SCREEN-VALUE.
        cIFSCCode      = STRING(FILL-IN-IFSC:SCREEN-VALUE).
        iTransferLimit = INT(FILL-IN-TransferLimit:SCREEN-VALUE).
        dRateofInt     = DECIMAL(FILL-IN-RateOfInt:SCREEN-VALUE).
        iTotalLoanAmt  = INT(FILL-IN-TotalLoanAmt:SCREEN-VALUE).
        iLoanDuration  = INT(COMBO-BOX-LoanDuration:SCREEN-VALUE).
    END.
    IF (cAccntSubType <> ? AND cAccntType <> ? AND cIFSCCode <> "" AND iTransferLimit <> 0) 
        OR(cAccntSubType <> ? AND cAccntType <> ? AND cIFSCCode <> "" AND dRateofInt <> 0.00 AND iTotalLoanAmt <> 0 AND iLoanDuration <> ?) THEN
    DO:
        
        BUTTON-Create:SENSITIVE = TRUE.
    END.
    ELSE
    DO:
        BUTTON-Create:SENSITIVE = FALSE.
    END.

    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

