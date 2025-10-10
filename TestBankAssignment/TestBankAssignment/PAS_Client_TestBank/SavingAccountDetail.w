&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
USING Progress.Json.ObjectModel.*.
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
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
    
DEFINE TEMP-TABLE ttSavingAccountTxnHistory
    FIELD AcctNum        AS INTEGER
    FIELD TxnDate        AS DATE
    FIELD TxnId          AS INTEGER
    FIELD TxnDetail      AS CHARACTER
    FIELD WithdrawAmount AS DECIMAL
    FIELD DepositAmount  AS DECIMAL
    FIELD Balance        AS DECIMAL.

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER iAcctNum AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iCustId AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BROWSE-SavingAccountTxnHistory

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttSavingAccountTxnHistory

/* Definitions for BROWSE BROWSE-SavingAccountTxnHistory                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-SavingAccountTxnHistory ttSavingAccountTxnHistory.AcctNum ttSavingAccountTxnHistory.TxnDate ttSavingAccountTxnHistory.TxnId ttSavingAccountTxnHistory.TxnDetail ttSavingAccountTxnHistory.WithdrawAmount    
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-SavingAccountTxnHistory   
&Scoped-define SELF-NAME BROWSE-SavingAccountTxnHistory
&Scoped-define QUERY-STRING-BROWSE-SavingAccountTxnHistory FOR EACH ttSavingAccountTxnHistory
&Scoped-define OPEN-QUERY-BROWSE-SavingAccountTxnHistory OPEN QUERY {&SELF-NAME} FOR EACH ttSavingAccountTxnHistory.
&Scoped-define TABLES-IN-QUERY-BROWSE-SavingAccountTxnHistory ~
ttSavingAccountTxnHistory
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-SavingAccountTxnHistory ttSavingAccountTxnHistory


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BROWSE-SavingAccountTxnHistory}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-8 RECT-14 RECT-15 RECT-16 ~
BUTTON-Deposit FILL-IN-AccountNo FILL-IN-Balance FILL-IN-TransferLimit ~
FILL-IN-IFSC BUTTON-Withdraw COMBO-BOX-AccType RADIO-SET-TxnType ~
BROWSE-SavingAccountTxnHistory 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-AccountNo FILL-IN-Balance ~
FILL-IN-TransferLimit FILL-IN-IFSC COMBO-BOX-AccType RADIO-SET-TxnType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Deposit 
    LABEL "Deposit" 
    SIZE 15 BY 1.13.

DEFINE BUTTON BUTTON-GenerateReport 
    LABEL "GenerateReport" 
    SIZE 16 BY 1.13.

DEFINE BUTTON BUTTON-Withdraw 
    LABEL "Withdraw" 
    SIZE 15 BY 1.

DEFINE VARIABLE COMBO-BOX-AccType     AS CHARACTER FORMAT "X(256)":U 
    LABEL "Account Type" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "Savings Account","Loan Account","Demat Account" 
    DROP-DOWN-LIST
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AccountNo     AS CHARACTER FORMAT "X(256)":U 
    LABEL "Account #" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Balance       AS CHARACTER FORMAT "X(256)":U 
    LABEL "Balance" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-IFSC          AS CHARACTER FORMAT "X(256)":U 
    LABEL "IFSC Code" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TransferLimit AS CHARACTER FORMAT "X(256)":U 
    LABEL "Transfer Limit" 
    VIEW-AS FILL-IN 
    SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE RADIO-SET-TxnType     AS CHARACTER 
    VIEW-AS RADIO-SET HORIZONTAL
    RADIO-BUTTONS 
    "All", "All",
    "Deposit", "Deposit",
    "WithDraw", "WithDraw"
    SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-14
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 103 BY 5.25.

DEFINE RECTANGLE RECT-15
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 103 BY 10.5.

DEFINE RECTANGLE RECT-16
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 101 BY 9.25.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 83 BY 4.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-SavingAccountTxnHistory FOR 
    ttSavingAccountTxnHistory SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-SavingAccountTxnHistory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-SavingAccountTxnHistory wWin _FREEFORM
    QUERY BROWSE-SavingAccountTxnHistory DISPLAY
    ttSavingAccountTxnHistory.AcctNum
    ttSavingAccountTxnHistory.TxnDate
    ttSavingAccountTxnHistory.TxnId
    ttSavingAccountTxnHistory.TxnDetail
    ttSavingAccountTxnHistory.WithdrawAmount /* COLUMN-LABEL "WithdrawAmount"*/
    ttSavingAccountTxnHistory.DepositAmount /* COLUMN-LABEL "DepositAmount"*/
    ttSavingAccountTxnHistory.Balance
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 7.25 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
    BUTTON-Deposit AT ROW 3.25 COL 89 WIDGET-ID 24
    FILL-IN-AccountNo AT ROW 3.5 COL 17 COLON-ALIGNED WIDGET-ID 4
    FILL-IN-Balance AT ROW 3.5 COL 57 COLON-ALIGNED WIDGET-ID 6
    FILL-IN-TransferLimit AT ROW 4.75 COL 17 COLON-ALIGNED WIDGET-ID 8
    FILL-IN-IFSC AT ROW 4.75 COL 57 COLON-ALIGNED WIDGET-ID 14
    BUTTON-Withdraw AT ROW 4.75 COL 89 WIDGET-ID 20
    COMBO-BOX-AccType AT ROW 6 COL 17 COLON-ALIGNED WIDGET-ID 16
    RADIO-SET-TxnType AT ROW 9 COL 5 NO-LABEL WIDGET-ID 26
    BUTTON-GenerateReport AT ROW 9 COL 88 WIDGET-ID 30
    BROWSE-SavingAccountTxnHistory AT ROW 10.5 COL 5 WIDGET-ID 200
    "Transaction Details :" VIEW-AS TEXT
    SIZE 28 BY .63 AT ROW 8 COL 4 WIDGET-ID 38
    FONT 2
    "Account Details :" VIEW-AS TEXT
    SIZE 22 BY .63 AT ROW 2.5 COL 4 WIDGET-ID 34
    FONT 2
    "SAVING ACCOUNT DETAILS" VIEW-AS TEXT
    SIZE 34 BY .63 AT ROW 1.25 COL 39 WIDGET-ID 36
    FONT 5
    RECT-8 AT ROW 3.25 COL 4 WIDGET-ID 2
    RECT-14 AT ROW 2.25 COL 3 WIDGET-ID 32
    RECT-15 AT ROW 7.75 COL 3 WIDGET-ID 40
    RECT-16 AT ROW 8.75 COL 4 WIDGET-ID 42
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 130.63 BY 20.78 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW wWin ASSIGN
        HIDDEN             = YES
        TITLE              = "Saving Account Detail"
        HEIGHT             = 17.69
        WIDTH              = 106.63
        MAX-HEIGHT         = 28.81
        MAX-WIDTH          = 146.25
        VIRTUAL-HEIGHT     = 28.81
        VIRTUAL-WIDTH      = 146.25
        RESIZE             = no
        SCROLL-BARS        = no
        STATUS-AREA        = no
        BGCOLOR            = ?
        FGCOLOR            = ?
        THREE-D            = yes
        MESSAGE-AREA       = no
        SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

    {src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-SavingAccountTxnHistory BUTTON-GenerateReport fMain */
/* SETTINGS FOR BUTTON BUTTON-GenerateReport IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
    THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-SavingAccountTxnHistory
/* Query rebuild information for BROWSE BROWSE-SavingAccountTxnHistory
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSavingAccountTxnHistory.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-SavingAccountTxnHistory */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Saving Account Detail */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Saving Account Detail */
    DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-SavingAccountTxnHistory
&Scoped-define SELF-NAME BROWSE-SavingAccountTxnHistory
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-SavingAccountTxnHistory wWin
ON ROW-DISPLAY OF BROWSE-SavingAccountTxnHistory IN FRAME fMain
    DO:
        IF AVAILABLE ttSavingAccountTxnHistory AND ttSavingAccountTxnHistory.DepositAmount = DECIMAL("") THEN
        DO:
            ttSavingAccountTxnHistory.WithdrawAmount:FGCOLOR IN BROWSE BROWSE-SavingAccountTxnHistory = 12 .

        END.
        ELSE IF AVAILABLE ttSavingAccountTxnHistory AND ttSavingAccountTxnHistory.WithdrawAmount = DECIMAL("") THEN
            DO:
                ttSavingAccountTxnHistory.DepositAmount:FGCOLOR IN BROWSE BROWSE-SavingAccountTxnHistory = 9 .

            END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Deposit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Deposit wWin
ON CHOOSE OF BUTTON-Deposit IN FRAME fMain /* Deposit */
    DO:
        DEFINE VARIABLE iAccNum AS INTEGER NO-UNDO.
        DEFINE VARIABLE iTxnId  AS INTEGER NO-UNDO.
        iAccNum = INT(ttCustAccntDetails.AcctNum).
            
        FOR EACH ttSavingAccountTxnHistory WHERE ttSavingAccountTxnHistory.AcctNum = iAccNum NO-LOCK BREAK BY ttSavingAccountTxnHistory.TxnId :
            IF LAST-OF (ttSavingAccountTxnHistory.TxnId) THEN
            DO:

                iTxnId = ttSavingAccountTxnHistory.TxnId.

            END.
        END.
        IF iTxnID = 0 THEN 
            iTxnId = iAccNum - 456000.
        
        RUN DepositAmount.w(INPUT iAccNum,INPUT iTxnId).
        RUN initializeObject.
        
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-GenerateReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-GenerateReport wWin
ON CHOOSE OF BUTTON-GenerateReport IN FRAME fMain /* GenerateReport */
    DO:
        DEFINE VARIABLE cTxnType AS CHARACTER NO-UNDO.
        cTxnType = RADIO-SET-TxnType:SCREEN-VALUE.
        RUN ReportDialog.w(INPUT iAcctNum,INPUT cTxnType).

    
    
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Withdraw
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Withdraw wWin
ON CHOOSE OF BUTTON-Withdraw IN FRAME fMain /* Withdraw */
    DO:
        DEFINE VARIABLE iAccNum AS INTEGER NO-UNDO.
        DEFINE VARIABLE iTxnId  AS INTEGER NO-UNDO.
        iAccNum = INT(ttCustAccntDetails.AcctNum).
            
        FOR EACH ttSavingAccountTxnHistory WHERE ttSavingAccountTxnHistory.AcctNum = iAccNum NO-LOCK BREAK BY ttSavingAccountTxnHistory.TxnId :
            IF LAST-OF (ttSavingAccountTxnHistory.TxnId) THEN
            DO:

                iTxnId = ttSavingAccountTxnHistory.TxnId.

            END.
        END.
        IF iTxnID = 0 THEN 
            iTxnId = iAccNum - 456000.
        
        RUN WithdrawAmount.w(INPUT iAccNum,INPUT iTxnId).  
        RUN initializeObject.
        
        
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-TxnType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-TxnType wWin
ON VALUE-CHANGED OF RADIO-SET-TxnType IN FRAME fMain
    DO:
        DEFINE VARIABLE dHandle           AS HANDLE           NO-UNDO.
        DEFINE VARIABLE wHandle           AS HANDLE           NO-UNDO.
        DEFINE VARIABLE lHide             AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE iAcctNum          AS INTEGER          NO-UNDO.
        DEFINE VARIABLE cWhere            AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE cTxnType          AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcResult          AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        
        ASSIGN
            iAcctNum = INT(FILL-IN-AccountNo:SCREEN-VALUE)
            cTxnType = RADIO-SET-TxnType:SCREEN-VALUE
            cWhere   = "WHERE".

        ASSIGN
            cWhere = cWhere + "" +
            (IF (iAcctNum <> 0) THEN (" AND AcctNum = " + STRING(iAcctNum)) ELSE "") +
           (IF (cTxnType = "Withdraw") THEN (" AND withdrawAmount > 0 ") ELSE IF(cTxnType = "Deposit") THEN (" AND depositAmount > 0") 
           ELSE "").
        cWhere = REPLACE(cWhere,"WHERE AND","WHERE").
        MESSAGE cWhere
            VIEW-AS ALERT-BOX.
        lConnectionStatus = oConnection:getConnect().
        /*        MESSAGE "Connected = "lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                 */
     
        oUtility:getInputParameterStructure(INPUT "getTxnDetailByTxnType",INPUT cWhere ,INPUT 0, INPUT TEMP-TABLE ttEmpty:HANDLE, OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX.
      
        IF VALID-HANDLE (oConnection:hAppServerHandle) THEN
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcResult).
        ELSE
            lConnectionStatus = oConnection:getDisconnect().
        
        lConnectionStatus = oConnection:getDisconnect().
        /*        MESSAGE "Disconnected = " lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                     */
        MESSAGE STRING(lcResult)
            VIEW-AS ALERT-BOX. 
        TEMP-TABLE ttSavingAccountTxnHistory:READ-JSON ("LONGCHAR",lcResult,"Empty").
        OPEN QUERY BROWSE-SavingAccountTxnHistory FOR EACH ttSavingAccountTxnHistory.
            
   
        IF RADIO-SET-TxnType:SCREEN-VALUE = "Deposit" THEN
        DO:
            dHandle = BROWSE BROWSE-SavingAccountTxnHistory:FIRST-COLUMN.
            DO WHILE VALID-HANDLE(dHandle):
                IF dHandle:NAME = "WithdrawAmount"  THEN
                DO:
                    HIDE dHandle.
                END.
                ELSE
                    VIEW dHandle.
                dHandle = dHandle:NEXT-COLUMN.
            END.
        END.
        ELSE  IF RADIO-SET-TxnType:SCREEN-VALUE = "WithDraw" THEN
            DO:
                wHandle = BROWSE BROWSE-SavingAccountTxnHistory:FIRST-COLUMN.
                DO WHILE VALID-HANDLE(wHandle):
                    IF wHandle:NAME = "DepositAmount" THEN
                    DO:
                        HIDE wHandle.
                    END.
                    ELSE 
                        VIEW wHandle.
                    wHandle = wHandle:NEXT-COLUMN.
                END.
            END.  
            ELSE 
            DO:
                wHandle = BROWSE BROWSE-SavingAccountTxnHistory:FIRST-COLUMN.
                DO WHILE VALID-HANDLE(wHandle):
                    VIEW wHandle.
                    wHandle = wHandle:NEXT-COLUMN.
                END.
            END.  
            
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Delete the WINDOW we created */
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
        THEN DELETE WIDGET wWin.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
    DISPLAY FILL-IN-AccountNo FILL-IN-Balance FILL-IN-TransferLimit FILL-IN-IFSC 
        COMBO-BOX-AccType RADIO-SET-TxnType 
        WITH FRAME fMain IN WINDOW wWin.
    ENABLE RECT-8 RECT-14 RECT-15 RECT-16 BUTTON-Deposit FILL-IN-AccountNo 
        FILL-IN-Balance FILL-IN-TransferLimit FILL-IN-IFSC BUTTON-Withdraw 
        COMBO-BOX-AccType RADIO-SET-TxnType BROWSE-SavingAccountTxnHistory 
        WITH FRAME fMain IN WINDOW wWin.
    {&OPEN-BROWSERS-IN-QUERY-fMain}
    VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
    /*------------------------------------------------------------------------------
                                  Purpose:  Window-specific override of this procedure which destroys 
                                            its contents and itself.
                                    Notes:  
                                ------------------------------------------------------------------------------*/

    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
    /*------------------------------------------------------------------------------
                                 Purpose:
                                 Notes:
                                ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    RUN SUPER.

    /* Code placed here will execute AFTER standard behavior.    */
    DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE lcCustData        AS LONGCHAR         NO-UNDO.
    DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
    DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
    DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
           
    oConnection = NEW ServerConnection().
    oUtility = NEW ClientUtility().
  
    EMPTY TEMP-TABLE ttCustAccntDetails.

    lConnectionStatus = oConnection:getConnect().
    /*    MESSAGE "Connected = "lConnectionStatus*/
    /*        VIEW-AS ALERT-BOX.                 */
     
    oUtility:getInputParameterStructure(INPUT "getSavAcctDetails",INPUT "",INPUT iAcctNum, INPUT TEMP-TABLE ttEmpty:HANDLE, OUTPUT oInputJson).
    MESSAGE STRING(oInputJson:GetJsonText())
        VIEW-AS ALERT-BOX.
      
    IF VALID-HANDLE (oConnection:hAppServerHandle) THEN
        RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcCustData).
    ELSE
        lConnectionStatus = oConnection:getDisconnect().
        
    lConnectionStatus = oConnection:getDisconnect().
    /*    MESSAGE "Disconnected = " lConnectionStatus*/
    /*        VIEW-AS ALERT-BOX.                     */
    MESSAGE STRING(lcCustData)
        VIEW-AS ALERT-BOX.   
        
    TEMP-TABLE ttCustAccntDetails:READ-JSON ("LONGCHAR",lcCustData,"Empty").
    FIND FIRST ttCustAccntDetails. 
    MESSAGE ttCustAccntDetails.AcctNum
        VIEW-AS ALERT-BOX.
  
    FIND FIRST ttCustAccntDetails NO-LOCK.
    IF AVAIL ttCustAccntDetails THEN 
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN 
                COMBO-BOX-AccType:SENSITIVE        = FALSE
                FILL-IN-AccountNo:READ-ONLY        = TRUE
                FILL-IN-Balance:READ-ONLY          = TRUE
                FILL-IN-IFSC:READ-ONLY             = TRUE
                FILL-IN-TransferLimit:READ-ONLY    = TRUE
                FILL-IN-AccountNo:SCREEN-VALUE     = STRING(ttCustAccntDetails.AcctNum)
                FILL-IN-Balance:SCREEN-VALUE       = STRING(ttCustAccntDetails.Balance)
                FILL-IN-IFSC:SCREEN-VALUE          = STRING(ttCustAccntDetails.BranchCode)
                FILL-IN-TransferLimit:SCREEN-VALUE = STRING(ttCustAccntDetails.TransferLimit)
                COMBO-BOX-AccType:SCREEN-VALUE     = ttCustAccntDetails.AccntType.
        END.
    END.
    
    EMPTY TEMP-TABLE ttSavingAccountTxnHistory.
    lConnectionStatus = oConnection:getConnect().
    /*    MESSAGE "Connected = "lConnectionStatus*/
    /*        VIEW-AS ALERT-BOX.                 */
          
    oUtility:getInputParameterStructure(INPUT "getTxnDetails",INPUT "",INPUT INT(ttCustAccntDetails.AcctNum), INPUT TEMP-TABLE ttEmpty:HANDLE, OUTPUT oInputJson).
    MESSAGE STRING(oInputJson:GetJsonText())
        VIEW-AS ALERT-BOX.
                
    IF VALID-HANDLE (oConnection:hAppServerHandle) THEN
        RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcCustData).
    ELSE
        lConnectionStatus = oConnection:getDisconnect().
        
    lConnectionStatus = oConnection:getDisconnect().
    /*    MESSAGE "Disconnected = " lConnectionStatus*/
    /*        VIEW-AS ALERT-BOX.                     */
    MESSAGE STRING(lcCustData)
        VIEW-AS ALERT-BOX.   
        
    TEMP-TABLE ttSavingAccountTxnHistory:READ-JSON ("LONGCHAR",lcCustData,"Empty").
    OPEN QUERY BROWSE-SavingAccountTxnHistory FOR EACH ttSavingAccountTxnHistory.
    IF AVAILABLE ttSavingAccountTxnHistory THEN
    DO:
        BUTTON-GenerateReport:SENSITIVE = TRUE.
    END.


    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

