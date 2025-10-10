&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
USING Progress.Json.ObjectModel.*.
{adecomm/appserv.i}
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
/*DEFINE TEMP-TABLE ttCustomer          */
/*    FIELD CustId        AS INTEGER    */
/*    FIELD FirstName     AS CHARACTER  */
/*    FIELD LastName      AS CHARACTER  */
/*    FIELD Address1      AS CHARACTER  */
/*    FIELD Address2      AS CHARACTER  */
/*    FIELD City          AS CHARACTER  */
/*    FIELD State         AS CHARACTER  */
/*    FIELD Country       AS CHARACTER  */
/*    FIELD EmailId       AS CHARACTER  */
/*    FIELD Phone         AS CHARACTER  */
/*    FIELD Mobile        AS CHARACTER  */
/*    FIELD DateOfBirth   AS DATE       */
/*    FIELD MaritalStatus AS CHARACTER  */
/*    FIELD ZIPCode       AS CHARACTER. */
/*                                      */
/*DEFINE TEMP-TABLE ttCustomerAccount   */
/*    FIELD SelectRow     AS LOGICAL    */
/*    FIELD CustId        AS INTEGER    */
/*    FIELD AcctNum       AS INTEGER    */
/*    FIELD AccountTypeID AS INTEGER    */
/*    FIELD AccountType   AS CHARACTER .*/
    
DEFINE TEMP-TABLE ttDeleteInfo
    FIELD CustId      AS INTEGER
    FIELD CustName    AS CHARACTER
    FIELD AcctNum     AS INTEGER
    FIELD AccountType AS CHARACTER .
    
/*DEFINE TEMP-TABLE ttCustAccntDetails*/
/*    FIELD AccntType     AS CHARACTER*/
/*    FIELD AccntSubType  AS CHARACTER*/
/*    FIELD IFSCCode      AS CHARACTER*/
/*    FIELD RateofInt     AS DECIMAL  */
/*    FIELD LoanDuration  AS INTEGER  */
/*    FIELD TotalLoanAmt  AS INTEGER  */
/*    FIELD TransferLimit AS INTEGER. */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER TABLE FOR ttDeleteInfo.
//DEFINE INPUT PARAMETER TABLE FOR ttCustomer.
//DEFINE INPUT PARAMETER TABLE FOR ttCustomerAccount.

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}


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
&Scoped-Define ENABLED-OBJECTS RECT-9 RECT-10 RECT-11 FILL-IN-CustId ~
FILL-IN-CustName FILL-IN-AcctNum FILL-IN-AcctType BUTTON-Delete Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-CustId FILL-IN-CustName ~
FILL-IN-AcctNum FILL-IN-AcctType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Delete AUTO-GO 
     LABEL "Delete" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-AcctNum AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Account Num" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AcctType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Type" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-CustId AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "CustId" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-CustName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Customer Name" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 8.25.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 4.75.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 2.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     FILL-IN-CustId AT ROW 2.5 COL 19 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-CustName AT ROW 3.75 COL 19 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-AcctNum AT ROW 5.75 COL 19 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-AcctType AT ROW 7 COL 19 COLON-ALIGNED WIDGET-ID 8
     BUTTON-Delete AT ROW 8.5 COL 7 WIDGET-ID 20
     Btn_Cancel AT ROW 8.5 COL 26 WIDGET-ID 18
     "CONFIRM" VIEW-AS TEXT
          SIZE 13 BY .63 AT ROW 1.25 COL 18 WIDGET-ID 14
          FONT 5
     RECT-9 AT ROW 2.25 COL 5 WIDGET-ID 10
     RECT-10 AT ROW 2 COL 4 WIDGET-ID 12
     RECT-11 AT ROW 5.25 COL 5 WIDGET-ID 16
     SPACE(3.87) SKIP(0.93)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "DeleteAccount" WIDGET-ID 100.


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

ASSIGN 
       FILL-IN-AcctNum:READ-ONLY IN FRAME gDialog        = TRUE.

ASSIGN 
       FILL-IN-AcctType:READ-ONLY IN FRAME gDialog        = TRUE.

ASSIGN 
       FILL-IN-CustId:READ-ONLY IN FRAME gDialog        = TRUE.

ASSIGN 
       FILL-IN-CustName:READ-ONLY IN FRAME gDialog        = TRUE.

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
ON WINDOW-CLOSE OF FRAME gDialog /* DeleteAccount */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Delete gDialog
ON CHOOSE OF BUTTON-Delete IN FRAME gDialog /* Delete */
DO:
        DEFINE VARIABLE iAcctNum          AS INTEGER          NO-UNDO.
        DEFINE VARIABLE cAcctType         AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcStatus          AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
    
        iAcctNum = INT(FILL-IN-AcctNum:SCREEN-VALUE).
        cAcctType = FILL-IN-AcctType:SCREEN-VALUE.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
    
        lConnectionStatus = oConnection:getConnect().
/*        MESSAGE "Connected = "lConnectionStatus*/
/*            VIEW-AS ALERT-BOX.                 */
            oUtility:getInputParameterStructure(INPUT "DeleteAccount", INPUT cAcctType, INPUT iAcctNum, INPUT TEMP-TABLE ttDeleteInfo:HANDLE,OUTPUT oInputJson).
            MESSAGE STRING(oInputJson:GetJsonText())
                VIEW-AS ALERT-BOX.
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcStatus).
   //     RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT "DeleteAccount",INPUT 0,OUTPUT lcStatus,INPUT "",INPUT TABLE ttCustomer,INPUT TABLE ttCustAccntDetails,INPUT cAcctType,INPUT iAcctNum).
        lConnectionStatus = oConnection:getDisconnect().
/*        MESSAGE "Disconnected = " lConnectionStatus*/
/*            VIEW-AS ALERT-BOX.                     */
            
        MESSAGE STRING(lcStatus)
            VIEW-AS ALERT-BOX.
        
  
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
  DISPLAY FILL-IN-CustId FILL-IN-CustName FILL-IN-AcctNum FILL-IN-AcctType 
      WITH FRAME gDialog.
  ENABLE RECT-9 RECT-10 RECT-11 FILL-IN-CustId FILL-IN-CustName FILL-IN-AcctNum 
         FILL-IN-AcctType BUTTON-Delete Btn_Cancel 
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
        FIND FIRST ttDeleteInfo.

        FILL-IN-CustId:SCREEN-VALUE = STRING(ttDeleteInfo.CustId).
        FILL-IN-CustName:SCREEN-VALUE = ttDeleteInfo.CustName.

        FILL-IN-AcctNum:SCREEN-VALUE = STRING(ttDeleteInfo.AcctNum).
        FILL-IN-AcctType:SCREEN-VALUE = ttDeleteInfo.AccountType.

/*FIND FIRST ttCustomer.                                                             */
/*FIND FIRST ttCustomerAccount.                                                      */
/*                                                                                   */
/*        FILL-IN-CustId:SCREEN-VALUE = STRING(ttCustomer.CustId).                   */
/*        FILL-IN-CustName:SCREEN-VALUE = ttCustomer.FirstName + ttCustomer.LastName.*/
/*                                                                                   */
/*        FILL-IN-AcctNum:SCREEN-VALUE = STRING(ttCustomerAccount.AcctNum).          */
/*        FILL-IN-AcctType:SCREEN-VALUE = ttCustomerAccount.AccountType.             */

    END.


    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

