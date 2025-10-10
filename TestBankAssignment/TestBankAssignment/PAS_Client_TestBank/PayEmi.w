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
DEFINE TEMP-TABLE tt-Emi NO-UNDO
    FIELD EMIID            AS INTEGER  
    FIELD EMINum           AS INTEGER   
    FIELD EMIDate          AS DATE      
    FIELD EMIAmount        AS DECIMAL   
    FIELD EMIStatus        AS CHARACTER 
    FIELD RemainingBalance AS DECIMAL   
    INDEX indRemainingBalance RemainingBalance ASC.
    
DEFINE TEMP-TABLE ttCustAccntDetails
    FIELD AcctNum         AS CHARACTER
    FIELD AccntType       AS CHARACTER
    FIELD AccntSubType    AS CHARACTER
    FIELD TransferLimit   AS INTEGER
    FIELD Balance         AS INTEGER
    FIELD BranchCode      AS CHARACTER
    FIELD RateOfInterest  AS DECIMAL
    FIELD LoanDuration    AS INTEGER
    FIELD EMIID           AS INTEGER
    FIELD TotalLoanAmount AS INTEGER.

DEFINE INPUT PARAMETER TABLE FOR tt-Emi.
DEFINE INPUT PARAMETER TABLE FOR ttCustAccntDetails.
//DEFINE INPUT  PARAMETER iAcctNum AS INTEGER NO-UNDO.
DEFINE VARIABLE iEMIID AS INTEGER NO-UNDO.


/* Parameters Definitions ---                                           */

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

  Created: 02/03/22 -  5:11 pm

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
&Scoped-Define ENABLED-OBJECTS RECT-26 RECT-27 FILL-IN-loanacctnum ~
FILL-IN-EMIDate FILL-IN-Amount Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-loanacctnum FILL-IN-EMIDate ~
FILL-IN-Amount 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 12 BY 1.13.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "PAY" 
     SIZE 12 BY 1.13.

DEFINE VARIABLE FILL-IN-Amount AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "EMI Amount" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-EMIDate AS DATE FORMAT "99/99/99":U 
     LABEL "EMI Date" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-loanacctnum AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "To Account" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 4.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 6.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     FILL-IN-loanacctnum AT ROW 2.75 COL 14 COLON-ALIGNED WIDGET-ID 14
     FILL-IN-EMIDate AT ROW 4 COL 14 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-Amount AT ROW 5.25 COL 14 COLON-ALIGNED WIDGET-ID 4
     Btn_OK AT ROW 7 COL 3
     Btn_Cancel AT ROW 7 COL 28
     "PAY EMI" VIEW-AS TEXT
          SIZE 11 BY 1 AT ROW 1 COL 16 WIDGET-ID 16
          FONT 5
     RECT-26 AT ROW 2.5 COL 3 WIDGET-ID 18
     RECT-27 AT ROW 2.25 COL 2 WIDGET-ID 20
     SPACE(1.24) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Pay EMI"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
       FILL-IN-Amount:READ-ONLY IN FRAME gDialog        = TRUE.

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
ON WINDOW-CLOSE OF FRAME gDialog /* Pay EMI */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK gDialog
ON CHOOSE OF Btn_OK IN FRAME gDialog /* PAY */
DO:
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcCustData        AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
        DEFINE VARIABLE iEmiId            AS INTEGER          NO-UNDO.
           
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        FIND FIRST tt-Emi.
        iEmiId = tt-Emi.EMIID.
        EMPTY TEMP-TABLE tt-Emi.
        CREATE tt-Emi.
        ASSIGN
            tt-Emi.EMIAmount = INT(FILL-IN-Amount:SCREEN-VALUE)
            tt-Emi.EMIDate   = DATE(FILL-IN-EMIDate:SCREEN-VALUE)
            tt-Emi.EMIID     = iEMIID.
       
          
        MESSAGE tt-Emi.EMIID
            VIEW-AS ALERT-BOX.
        lConnectionStatus = oConnection:getConnect().
        /*        MESSAGE "Connected = "lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                 */
     
        oUtility:getInputParameterStructure(INPUT "PayEMI",INPUT "",INPUT 0, INPUT TEMP-TABLE tt-Emi:HANDLE, OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX.
      
        IF VALID-HANDLE (oConnection:hAppServerHandle) THEN
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcCustData).
        ELSE
            lConnectionStatus = oConnection:getDisconnect().
        
        lConnectionStatus = oConnection:getDisconnect().
        /*        MESSAGE "Disconnected = " lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                     */
        MESSAGE STRING(lcCustData)
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
  DISPLAY FILL-IN-loanacctnum FILL-IN-EMIDate FILL-IN-Amount 
      WITH FRAME gDialog.
  ENABLE RECT-26 RECT-27 FILL-IN-loanacctnum FILL-IN-EMIDate FILL-IN-Amount 
         Btn_OK Btn_Cancel 
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
    DO WITH FRAME gDialog:
        
        FIND FIRST tt-Emi.
        FIND FIRST ttCustAccntDetails.
        ASSIGN
            FILL-IN-EMIDate:SCREEN-VALUE     = STRING (tt-Emi.EMIDate + 30)
            iEMIID                           = tt-Emi.EMIID
            FILL-IN-loanacctnum:SCREEN-VALUE = STRING ( ttCustAccntDetails.AcctNum)
            FILL-IN-EMIDate:SENSITIVE        = FALSE 
            FILL-IN-loanacctnum:SENSITIVE    = FALSE.
        FIND FIRST tt-Emi.
        FIND FIRST ttCustAccntDetails.
        IF ttCustAccntDetails.Balance < tt-Emi.EMIAmount THEN
            FILL-IN-Amount:SCREEN-VALUE = STRING(ttCustAccntDetails.Balance).
        ELSE
            FILL-IN-Amount:SCREEN-VALUE = STRING(tt-Emi.EMIAmount).
    END.

    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

