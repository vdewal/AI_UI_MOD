&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS gDialog 
USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
USING Progress.Json.ObjectModel.JsonArray FROM PROPATH.

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
DEFINE TEMP-TABLE ttCustAccntDetails
    FIELD AcctNum         AS CHARACTER
    FIELD AccntType       AS CHARACTER
    FIELD AccntSubType    AS CHARACTER
    FIELD TransferLimit   AS INTEGER
    FIELD Balance         AS INTEGER
    FIELD BranchCode      AS CHARACTER
    FIELD RateOfInterest  AS DECIMAL
    FIELD LoanDuration    AS INTEGER
    FIELD TotalLoanAmount AS INTEGER.
    
DEFINE TEMP-TABLE ttEmpty LIKE ttCustAccntDetails.

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER TABLE FOR ttCustAccntDetails.

/* Local Variable Definitions ---                                       */

       


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

  Created: 02/10/22 - 10:17 am

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-13 FILL-IN-AcctNum ~
COMBO-BOX-AccntSubType FILL-IN-TransferLimit COMBO-BOX-LoanDuration ~
FILL-IN-IFSC FILL-IN-RateOfInt FILL-IN-TotalLoanAmt Btn_Update Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-AcctNum COMBO-BOX-AccntSubType ~
FILL-IN-TransferLimit COMBO-BOX-AccntType COMBO-BOX-LoanDuration ~
FILL-IN-IFSC FILL-IN-RateOfInt FILL-IN-TotalLoanAmt 

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

DEFINE BUTTON Btn_Update AUTO-GO 
     LABEL "Update" 
     SIZE 20 BY 1.13.

DEFINE VARIABLE COMBO-BOX-AccntSubType AS CHARACTER 
     LABEL "Acc Sub Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN
     SIZE 26 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-AccntType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Savings Account","Loan Account","Demat Account" 
     DROP-DOWN-LIST
     SIZE 26 BY 1
     FONT 2 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-LoanDuration AS CHARACTER FORMAT "X(256)":U 
     LABEL "Loan Duration(yrs)" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3","4","5","6","7","8" 
     DROP-DOWN-LIST
     SIZE 9 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-AcctNum AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Account #" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-IFSC AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "IFSC Code" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FILL-IN-RateOfInt AS DECIMAL FORMAT ">9.99":U INITIAL 0 
     LABEL "Rate of Interest" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FILL-IN-TotalLoanAmt AS INT64 FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Total Loan Amount" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-TransferLimit AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Transfer Limit" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 8.25.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     FILL-IN-AcctNum AT ROW 3 COL 23 COLON-ALIGNED WIDGET-ID 26
     COMBO-BOX-AccntSubType AT ROW 3 COL 69 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-TransferLimit AT ROW 4.25 COL 69 COLON-ALIGNED WIDGET-ID 8
     COMBO-BOX-AccntType AT ROW 4.5 COL 23 COLON-ALIGNED WIDGET-ID 2
     COMBO-BOX-LoanDuration AT ROW 4.5 COL 69 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-IFSC AT ROW 5.75 COL 23 COLON-ALIGNED WIDGET-ID 14
     FILL-IN-RateOfInt AT ROW 5.75 COL 69 COLON-ALIGNED WIDGET-ID 16
     FILL-IN-TotalLoanAmt AT ROW 7.25 COL 23 COLON-ALIGNED WIDGET-ID 18
     Btn_Update AT ROW 9 COL 29
     Btn_Cancel AT ROW 9 COL 59
     "UPDATE ACCOUNT" VIEW-AS TEXT
          SIZE 24 BY .63 AT ROW 1.25 COL 39 WIDGET-ID 30
          FONT 5
     RECT-6 AT ROW 2.5 COL 4 WIDGET-ID 24
     RECT-13 AT ROW 2.25 COL 3 WIDGET-ID 28
     SPACE(1.49) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Update Account"
         DEFAULT-BUTTON Btn_Update CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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

/* SETTINGS FOR COMBO-BOX COMBO-BOX-AccntType IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-AcctNum:READ-ONLY IN FRAME gDialog        = TRUE.

ASSIGN 
       FILL-IN-RateOfInt:HIDDEN IN FRAME gDialog           = TRUE.

ASSIGN 
       FILL-IN-TotalLoanAmt:HIDDEN IN FRAME gDialog           = TRUE
       FILL-IN-TotalLoanAmt:READ-ONLY IN FRAME gDialog        = TRUE.

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
ON WINDOW-CLOSE OF FRAME gDialog /* Update Account */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update gDialog
ON CHOOSE OF Btn_Update IN FRAME gDialog /* Update */
DO:
        DEFINE VARIABLE lConnectionStatus    AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcStatus             AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE cAcctType            AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE oConnection          AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson           AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oInputParametersJson AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oTTJson              AS JsonArray        NO-UNDO.
        DEFINE VARIABLE oUtility             AS ClientUtility    NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        
        EMPTY TEMP-TABLE ttCustAccntDetails.
        /*        CREATE ttCustAccntDetails.                                                       */
        /*        ASSIGN                                                                           */
        /*            ttCustAccntDetails.AccntSubType    = COMBO-BOX-AccntSubType:SCREEN-VALUE     */
        /*            ttCustAccntDetails.AccntType       = COMBO-BOX-AccntType:SCREEN-VALUE        */
        /*            ttCustAccntDetails.AcctNum         = FILL-IN-AcctNum:SCREEN-VALUE            */
        /*            ttCustAccntDetails.BranchCode      = STRING(FILL-IN-IFSC:SCREEN-VALUE)       */
        /*            ttCustAccntDetails.LoanDuration    = INT(COMBO-BOX-LoanDuration:SCREEN-VALUE)*/
        /*            ttCustAccntDetails.RateOfInterest  = DECIMAL(FILL-IN-RateOfInt:SCREEN-VALUE) */
        /*            ttCustAccntDetails.TotalLoanAmount = INT(FILL-IN-TotalLoanAmt:SCREEN-VALUE)  */
        /*            ttCustAccntDetails.TransferLimit   = INT(FILL-IN-TransferLimit:SCREEN-VALUE).*/
    
        
        cAcctType = COMBO-BOX-AccntType:SCREEN-VALUE.
            
        lConnectionStatus = oConnection:getConnect().
        /*        MESSAGE "Connected = "lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                 */
            
        IF cAcctType = "Savings Account" THEN
        DO:  
            CREATE ttCustAccntDetails.
            ASSIGN
                ttCustAccntDetails.AccntSubType  = COMBO-BOX-AccntSubType:SCREEN-VALUE
                ttCustAccntDetails.AccntType     = COMBO-BOX-AccntType:SCREEN-VALUE
                ttCustAccntDetails.AcctNum       = FILL-IN-AcctNum:SCREEN-VALUE 
                ttCustAccntDetails.BranchCode    = STRING(FILL-IN-IFSC:SCREEN-VALUE) 
                ttCustAccntDetails.TransferLimit = INT(FILL-IN-TransferLimit:SCREEN-VALUE).
            /*            MESSAGE ttCustAccntDetails.AccntSubType ttCustAccntDetails.AccntType*/
            /*                VIEW-AS ALERT-BOX.                                              */
            oUtility:getInputParameterStructure(INPUT "UpdateSavAcct",INPUT "",INPUT 0, INPUT TEMP-TABLE ttCustAccntDetails:HANDLE, OUTPUT oInputJson).
            MESSAGE STRING(oInputJson:GetJsonText())
                VIEW-AS ALERT-BOX.
        END.
        IF cAcctType = "Loan Account" THEN
        DO:  
            CREATE ttCustAccntDetails.
            ASSIGN
                ttCustAccntDetails.AccntSubType    = COMBO-BOX-AccntSubType:SCREEN-VALUE
                ttCustAccntDetails.AccntType       = COMBO-BOX-AccntType:SCREEN-VALUE
                ttCustAccntDetails.AcctNum         = FILL-IN-AcctNum:SCREEN-VALUE 
                ttCustAccntDetails.BranchCode      = STRING(FILL-IN-IFSC:SCREEN-VALUE) 
                ttCustAccntDetails.LoanDuration    = INT(COMBO-BOX-LoanDuration:SCREEN-VALUE) 
                ttCustAccntDetails.RateOfInterest  = DECIMAL(FILL-IN-RateOfInt:SCREEN-VALUE)
                ttCustAccntDetails.TotalLoanAmount = INT(FILL-IN-TotalLoanAmt:SCREEN-VALUE).
            /*            MESSAGE ttCustAccntDetails.AccntSubType ttCustAccntDetails.AccntType*/
            /*                VIEW-AS ALERT-BOX.                                              */
            
            oUtility:getInputParameterStructure(INPUT "UpdateLoanAcct",INPUT "",INPUT 0, INPUT TEMP-TABLE ttCustAccntDetails:HANDLE, OUTPUT oInputJson).
            MESSAGE STRING(oInputJson:GetJsonText())
                VIEW-AS ALERT-BOX.
        END.    
        
        RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcStatus).
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
ON ENTRY OF COMBO-BOX-AccntSubType IN FRAME gDialog /* Acc Sub Type */
DO:

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-AccntSubType gDialog
ON VALUE-CHANGED OF COMBO-BOX-AccntSubType IN FRAME gDialog /* Acc Sub Type */
DO:
          
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-AccntType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-AccntType gDialog
ON VALUE-CHANGED OF COMBO-BOX-AccntType IN FRAME gDialog /* Account Type */
DO:
        
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
  DISPLAY FILL-IN-AcctNum COMBO-BOX-AccntSubType FILL-IN-TransferLimit 
          COMBO-BOX-AccntType COMBO-BOX-LoanDuration FILL-IN-IFSC 
          FILL-IN-RateOfInt FILL-IN-TotalLoanAmt 
      WITH FRAME gDialog.
  ENABLE RECT-6 RECT-13 FILL-IN-AcctNum COMBO-BOX-AccntSubType 
         FILL-IN-TransferLimit COMBO-BOX-LoanDuration FILL-IN-IFSC 
         FILL-IN-RateOfInt FILL-IN-TotalLoanAmt Btn_Update Btn_Cancel 
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
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcAccSubTypeList  AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE cAcctType         AS CHARACTER        NO-UNDO. 
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        

        ASSIGN
            FILL-IN-IFSC:VISIBLE           = FALSE
            FILL-IN-RateOfInt:VISIBLE      = FALSE
            FILL-IN-TotalLoanAmt:VISIBLE   = FALSE
            FILL-IN-TransferLimit:VISIBLE  = FALSE
            COMBO-BOX-LoanDuration:VISIBLE = FALSE.
        FIND FIRST ttCustAccntDetails.   
        FILL-IN-AcctNum:SCREEN-VALUE = ttCustAccntDetails.AcctNum.
       
       
        IF LOOKUP (ttCustAccntDetails.AccntType, COMBO-BOX-AccntType:LIST-ITEMS) > 0 THEN
            COMBO-BOX-AccntType:SCREEN-VALUE = ttCustAccntDetails.AccntType.
       
        cAcctType = COMBO-BOX-AccntType:SCREEN-VALUE.
        IF cAcctType = "Savings Account" THEN
        DO:
            ASSIGN
                FILL-IN-IFSC:VISIBLE               = TRUE
                FILL-IN-TransferLimit:VISIBLE      = TRUE
                COMBO-BOX-LoanDuration:VISIBLE     = FALSE
                FILL-IN-RateOfInt:VISIBLE          = FALSE
                FILL-IN-TotalLoanAmt:VISIBLE       = FALSE
                FILL-IN-IFSC:SCREEN-VALUE          = ttCustAccntDetails.BranchCode
                FILL-IN-TransferLimit:SCREEN-VALUE = STRING(ttCustAccntDetails.TransferLimit).
        END.
        
        IF cAcctType = "Loan Account" THEN
        DO:
     //       ASSIGN
            FILL-IN-IFSC:VISIBLE                = TRUE.
            FILL-IN-TransferLimit:VISIBLE       = FALSE.
            COMBO-BOX-LoanDuration:VISIBLE      = TRUE.
            FILL-IN-RateOfInt:VISIBLE           = TRUE.
            FILL-IN-TotalLoanAmt:VISIBLE        = TRUE.
            FILL-IN-IFSC:SCREEN-VALUE           = ttCustAccntDetails.BranchCode.
            COMBO-BOX-LoanDuration:SCREEN-VALUE = STRING((ttCustAccntDetails.LoanDuration)/(12.00)).
            FILL-IN-RateOfInt:SCREEN-VALUE      = STRING(ttCustAccntDetails.RateOfInterest).
            FILL-IN-TotalLoanAmt:SCREEN-VALUE   = STRING(ttCustAccntDetails.TotalLoanAmount).
                
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
        FIND FIRST ttCustAccntDetails.   
        IF LOOKUP (ttCustAccntDetails.AccntSubType, COMBO-BOX-AccntSubType:LIST-ITEMS) > 0 THEN
            COMBO-BOX-AccntSubType:SCREEN-VALUE = ttCustAccntDetails.AccntSubType.
            

    END.
    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

