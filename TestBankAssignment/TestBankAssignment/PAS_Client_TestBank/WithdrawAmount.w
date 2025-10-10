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
DEFINE TEMP-TABLE ttSavingAccountTxnHistory
    FIELD AcctNum        AS INTEGER
    FIELD TxnDate        AS DATE
    FIELD TxnId          AS INTEGER
    FIELD TxnDetail      AS CHARACTER
    FIELD WithdrawAmount AS DECIMAL
    FIELD DepositAmount  AS DECIMAL
    FIELD Balance        AS DECIMAL.

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER iAccNum AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iTxnId AS INTEGER NO-UNDO.

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

  Created: 02/10/22 - 10:19 am

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
&Scoped-Define ENABLED-OBJECTS RECT-19 RECT-20 FILL-IN-FromAccount ~
FILL-IN-TxnId FILL-IN-WithdrawDate COMBO-BOX-Source FILL-IN-Amount ~
Btn_Cancel-2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-FromAccount FILL-IN-TxnId ~
FILL-IN-WithdrawDate COMBO-BOX-Source FILL-IN-Amount 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel-2 AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.13.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "WithDraw" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE COMBO-BOX-Source AS CHARACTER FORMAT "X(256)":U 
     LABEL "Source" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "CASH","ATM","UPI" 
     DROP-DOWN-LIST
     SIZE 23 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-Amount AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Amount" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-FromAccount AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "AccountNum" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-TxnId AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "TxnId" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-WithdrawDate AS DATE FORMAT "99/99/99":U 
     LABEL "Withdraw Date" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 6.5.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 8.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     FILL-IN-FromAccount AT ROW 3 COL 17 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-TxnId AT ROW 4.25 COL 17 COLON-ALIGNED WIDGET-ID 20
     FILL-IN-WithdrawDate AT ROW 5.5 COL 17 COLON-ALIGNED WIDGET-ID 2
     COMBO-BOX-Source AT ROW 6.75 COL 17 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-Amount AT ROW 8 COL 17 COLON-ALIGNED WIDGET-ID 4
     Btn_OK AT ROW 9.5 COL 4 WIDGET-ID 16
     Btn_Cancel-2 AT ROW 9.5 COL 29 WIDGET-ID 14
     "WITHDRAW AMOUNT" VIEW-AS TEXT
          SIZE 26 BY 1 AT ROW 1.25 COL 11 WIDGET-ID 22
          FONT 5
     RECT-19 AT ROW 2.75 COL 4 WIDGET-ID 24
     RECT-20 AT ROW 2.5 COL 3 WIDGET-ID 26
     SPACE(1.49) SKIP(0.40)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Withdraw Amount" WIDGET-ID 100.


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

/* SETTINGS FOR BUTTON Btn_OK IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-FromAccount:READ-ONLY IN FRAME gDialog        = TRUE.

ASSIGN 
       FILL-IN-TxnId:READ-ONLY IN FRAME gDialog        = TRUE.

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
ON WINDOW-CLOSE OF FRAME gDialog /* Withdraw Amount */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* WithDraw */
DO:
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcStatus          AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
       
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
    
        EMPTY TEMP-TABLE ttSavingAccountTxnHistory.
        CREATE ttSavingAccountTxnHistory.
        ASSIGN
            ttSavingAccountTxnHistory.TxnId          = INT(FILL-IN-TxnId:SCREEN-VALUE) //iTxnId + 100
            ttSavingAccountTxnHistory.TxnDate        = DATE(FILL-IN-WithdrawDate:SCREEN-VALUE)
            ttSavingAccountTxnHistory.WithdrawAmount = DECIMAL(FILL-IN-Amount:SCREEN-VALUE)
            ttSavingAccountTxnHistory.AcctNum        = iAccNum
            ttSavingAccountTxnHistory.TxnDetail      = COMBO-BOX-Source:SCREEN-VALUE.
            
        lConnectionStatus = oConnection:getConnect().
        /*        MESSAGE "Connected = "lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                 */
          
        oUtility:getInputParameterStructure(INPUT "AddWithdraw",INPUT "",INPUT 0, INPUT TEMP-TABLE ttSavingAccountTxnHistory:HANDLE, OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX.
                
        IF VALID-HANDLE (oConnection:hAppServerHandle) THEN
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcStatus).
        ELSE
            lConnectionStatus = oConnection:getDisconnect().
        
        lConnectionStatus = oConnection:getDisconnect().
        /*        MESSAGE "Disconnected = " lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                     */
        MESSAGE STRING(lcStatus)
            VIEW-AS ALERT-BOX.   
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Source
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Source gDialog
ON VALUE-CHANGED OF COMBO-BOX-Source IN FRAME gDialog /* Source */
DO:
  RUN validation.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Amount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Amount gDialog
ON VALUE-CHANGED OF FILL-IN-Amount IN FRAME gDialog /* Amount */
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
  DISPLAY FILL-IN-FromAccount FILL-IN-TxnId FILL-IN-WithdrawDate 
          COMBO-BOX-Source FILL-IN-Amount 
      WITH FRAME gDialog.
  ENABLE RECT-19 RECT-20 FILL-IN-FromAccount FILL-IN-TxnId FILL-IN-WithdrawDate 
         COMBO-BOX-Source FILL-IN-Amount Btn_Cancel-2 
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
        FILL-IN-FromAccount:READ-ONLY      =       TRUE.
        FILL-IN-WithdrawDate:READ-ONLY    =       TRUE.
        FILL-IN-TxnId:READ-ONLY          =       TRUE .
        FILL-IN-FromAccount:SCREEN-VALUE = STRING(iAccNum).
        FILL-IN-WithdrawDate:SCREEN-VALUE = STRING(TODAY).
        FILL-IN-TxnId:SCREEN-VALUE =STRING(iTxnId + 100).
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

    DEFINE VARIABLE dAmount AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cSource AS CHARACTER NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}: 
        dAmount = DEC(FILL-IN-Amount:SCREEN-VALUE).
        cSource = COMBO-BOX-Source:SCREEN-VALUE.
        IF (dAmount <> 0.00 AND cSource <> ? ) THEN
        DO:
        
            Btn_OK:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            Btn_OK:SENSITIVE = FALSE.
        END.
    END.
    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

