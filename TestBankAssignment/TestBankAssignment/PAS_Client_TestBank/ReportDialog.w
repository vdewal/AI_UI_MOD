&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS Procedure
USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME gDialog
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
DEFINE TEMP-TABLE ttSavingAccountTxnHistory
    FIELD AcctNum        AS INTEGER
    FIELD TxnDate        AS DATE
    FIELD TxnId          AS INTEGER
    FIELD TxnDetail      AS CHARACTER
    FIELD WithdrawAmount AS DECIMAL
    FIELD DepositAmount  AS DECIMAL
    FIELD Balance        AS DECIMAL.

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER iAccnum AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER cTxnType AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-21 RECT-22 FILL-IN-AcctNum cbtxntype ~
fistartdate fienddate btngenerateReport Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-AcctNum cbtxntype fistartdate ~
fienddate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btngenerateReport 
     LABEL "Generate Report" 
     SIZE 18 BY 1.25.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY  NO-CONVERT-3D-COLORS
     LABEL "Cancel" 
     SIZE 14 BY 1.13.

DEFINE VARIABLE cbtxntype AS CHARACTER FORMAT "X(256)":U 
     LABEL "Txn Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","Deposit","Withdraw" 
     DROP-DOWN-LIST
     SIZE 21 BY 1
     FONT 2 NO-UNDO.

DEFINE VARIABLE fienddate AS DATE FORMAT "99/99/99":U 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-AcctNum AS CHARACTER FORMAT "X(256)":U 
     LABEL "AcctNum" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     FONT 2 NO-UNDO.

DEFINE VARIABLE fistartdate AS DATE FORMAT "99/99/99":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 7.5.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 34 BY 5.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
     FILL-IN-AcctNum AT ROW 3 COL 14 COLON-ALIGNED WIDGET-ID 10
     cbtxntype AT ROW 4.25 COL 14 COLON-ALIGNED WIDGET-ID 6
     fistartdate AT ROW 5.5 COL 14 COLON-ALIGNED WIDGET-ID 2
     fienddate AT ROW 6.75 COL 14 COLON-ALIGNED WIDGET-ID 4
     btngenerateReport AT ROW 8.25 COL 5 WIDGET-ID 8
     Btn_Cancel AT ROW 8.25 COL 25
     "Generate  Report" VIEW-AS TEXT
          SIZE 19 BY 1 AT ROW 1.25 COL 12 WIDGET-ID 12
          FONT 5
     RECT-21 AT ROW 2.5 COL 4 WIDGET-ID 14
     RECT-22 AT ROW 2.75 COL 5 WIDGET-ID 16
     SPACE(3.24) SKIP(2.46)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Report Generate"
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
ON WINDOW-CLOSE OF FRAME gDialog /* Report Generate */
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btngenerateReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btngenerateReport gDialog
ON CHOOSE OF btngenerateReport IN FRAME gDialog /* Generate Report */
DO:
        DEFINE VARIABLE startDate         AS DATE             NO-UNDO.
        DEFINE VARIABLE endDate           AS DATE             NO-UNDO.
        DEFINE VARIABLE cWhere            AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE lcResult          AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcCustData        AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
           
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
       

       
        ASSIGN
            startDate = DATE(fistartdate:SCREEN-VALUE)
            endDate   = DATE(fienddate:SCREEN-VALUE)
            cTxnType  = cbtxntype:SCREEN-VALUE
            cWhere    = "WHERE".

        ASSIGN

            cWhere = cWhere + "" +
            (IF (iAccnum <> 0) THEN (" AND AcctNum = " + STRING(iAccnum)) ELSE "") +
            (IF (startDate <> ?) THEN (" AND txnDate > " + QUOTER(startDate)) ELSE "") +
            (IF (endDate <> ?) THEN (" AND txnDate < " + QUOTER(endDate)) ELSE "") +
           (IF (cTxnType = "Withdraw") THEN (" AND withdrawAmount > 0 ") ELSE IF(cTxnType = "Deposit") THEN (" AND depositAmount > 0") 
           ELSE "").
        cWhere = REPLACE(cWhere,"WHERE AND","WHERE").
        MESSAGE cWhere
            VIEW-AS ALERT-BOX.
        lConnectionStatus = oConnection:getConnect().
        /*        MESSAGE "Connected = "lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                 */
     
        oUtility:getInputParameterStructure(INPUT "getTxnReport",INPUT cWhere ,INPUT 0, INPUT TEMP-TABLE ttSavingAccountTxnHistory:HANDLE, OUTPUT oInputJson).
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
        //    OS-COMMAND NO-WAIT QUOTER (lcCustData).
        APPLY "CHOOSE" TO Btn_Cancel.
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fienddate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fienddate gDialog
ON VALUE-CHANGED OF fienddate IN FRAME gDialog /* End Date */
DO:
    RUN validation.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fistartdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fistartdate gDialog
ON VALUE-CHANGED OF fistartdate IN FRAME gDialog /* Start Date */
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
  DISPLAY FILL-IN-AcctNum cbtxntype fistartdate fienddate 
      WITH FRAME gDialog.
  ENABLE RECT-21 RECT-22 FILL-IN-AcctNum cbtxntype fistartdate fienddate 
         btngenerateReport Btn_Cancel 
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
        ASSIGN 
            btngenerateReport:SENSITIVE  = FALSE
            FILL-IN-AcctNum:READ-ONLY    = TRUE
            cbtxntype:SENSITIVE          = FALSE
            FILL-IN-AcctNum:SCREEN-VALUE = STRING(iAccnum)
            cbtxntype:SCREEN-VALUE       = cTxnType.
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
    DEFINE VARIABLE dStartDate AS DATE NO-UNDO.
    DEFINE VARIABLE dEndDate   AS DATE NO-UNDO.
    
    DO WITH FRAME {&FRAME-NAME}:  
        dStartDate = DATE(fistartdate:SCREEN-VALUE).
        dEndDate = DATE(fienddate:SCREEN-VALUE).
        IF (dStartDate <> DATE("")  OR dEndDate <> DATE(""))THEN
        DO:
            btngenerateReport:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            btngenerateReport:SENSITIVE = FALSE.
        END.
    END.

    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

