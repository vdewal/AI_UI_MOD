&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS wWin 
USING OpenEdge.Core.String FROM PROPATH.
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS Procedure
USING Progress.Json.ObjectModel.JsonObject FROM PROPATH.

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
    FIELD EMIID           AS INTEGER
    FIELD TotalLoanAmount AS INTEGER.
   
DEFINE TEMP-TABLE ttEmpty LIKE ttCustAccntDetails. 

DEFINE TEMP-TABLE tt-Emi NO-UNDO
    FIELD EMIID            AS INTEGER  
    FIELD EMINum           AS INTEGER 
    FIELD EMIDate          AS DATE     
    FIELD EMIAmount        AS DECIMAL  
    FIELD EMIStatus        AS CHARACTER
    FIELD RemainingBalance AS DECIMAL 
    INDEX indRemainingBalance RemainingBalance ASC.
    
    

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER iAcctNum AS INTEGER NO-UNDO.
DEFINE VARIABLE iEmiId AS INTEGER NO-UNDO.


/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 01/24/22 -  5:10 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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
&Scoped-define BROWSE-NAME BROWSE-Emi-Details

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-Emi

/* Definitions for BROWSE BROWSE-Emi-Details                            */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Emi-Details tt-Emi.EMIID tt-Emi.EMINum tt-Emi.EMIDate tt-Emi.EMIAmount tt-Emi.EMIStatus tt-Emi.RemainingBalance   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Emi-Details   
&Scoped-define SELF-NAME BROWSE-Emi-Details
&Scoped-define QUERY-STRING-BROWSE-Emi-Details FOR EACH tt-Emi
&Scoped-define OPEN-QUERY-BROWSE-Emi-Details OPEN QUERY {&SELF-NAME} FOR EACH tt-Emi.
&Scoped-define TABLES-IN-QUERY-BROWSE-Emi-Details tt-Emi
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Emi-Details tt-Emi


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BROWSE-Emi-Details}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-23 RECT-24 RECT-25 FILL-IN-LoanAccNo ~
FILL-IN-TotalAmt FILL-IN-Balance FILL-IN-ROI FILL-IN-AccountSubType ~
FILL-IN-Duration FILL-IN-EmiAmount BUTTON-PayEMI BROWSE-Emi-Details 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-LoanAccNo FILL-IN-TotalAmt ~
FILL-IN-Balance FILL-IN-ROI FILL-IN-AccountSubType FILL-IN-Duration ~
FILL-IN-EmiAmount FILL-IN-STATUS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-PayEMI 
     LABEL "Pay EMI" 
     SIZE 25 BY 1.

DEFINE VARIABLE FILL-IN-AccountSubType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Account Sub Type" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Balance AS CHARACTER FORMAT "X(256)":U 
     LABEL "Balancel Amount" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Duration AS CHARACTER FORMAT "X(256)":U 
     LABEL "Duration(Months)" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-EmiAmount AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "EMI Amount" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LoanAccNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Loan Account #" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ROI AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rate of Interest" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STATUS AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-TotalAmt AS CHARACTER FORMAT "X(256)":U 
     LABEL "Total Amount" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 6.5.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 91 BY 4.

DEFINE RECTANGLE RECT-25
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 7.75.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Emi-Details FOR 
      tt-Emi SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Emi-Details
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Emi-Details wWin _FREEFORM
  QUERY BROWSE-Emi-Details DISPLAY
      tt-Emi.EMIID
    tt-Emi.EMINum
    tt-Emi.EMIDate
    tt-Emi.EMIAmount
    tt-Emi.EMIStatus
    tt-Emi.RemainingBalance
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 91 BY 5.25 ROW-HEIGHT-CHARS 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FILL-IN-LoanAccNo AT ROW 3.75 COL 21 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-TotalAmt AT ROW 3.75 COL 66 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-Balance AT ROW 5 COL 21 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-ROI AT ROW 5 COL 66 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-AccountSubType AT ROW 6.25 COL 21 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-Duration AT ROW 6.25 COL 66 COLON-ALIGNED WIDGET-ID 10
     FILL-IN-EmiAmount AT ROW 7.75 COL 21 COLON-ALIGNED WIDGET-ID 36
     BUTTON-PayEMI AT ROW 7.75 COL 68 WIDGET-ID 14
     BROWSE-Emi-Details AT ROW 10.5 COL 5 WIDGET-ID 200
     FILL-IN-STATUS AT ROW 16 COL 21 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     "EMI Details :" VIEW-AS TEXT
          SIZE 17 BY .63 AT ROW 9.75 COL 5 WIDGET-ID 28
          FONT 2
     "Account Details :" VIEW-AS TEXT
          SIZE 22 BY .63 AT ROW 2.75 COL 4 WIDGET-ID 26
          FONT 2
     "LOAN ACCOUNT DETAILS" VIEW-AS TEXT
          SIZE 32 BY 1.25 AT ROW 1.25 COL 34 WIDGET-ID 24
          FONT 5
     RECT-23 AT ROW 2.5 COL 3 WIDGET-ID 30
     RECT-24 AT ROW 3.5 COL 4 WIDGET-ID 32
     RECT-25 AT ROW 9.5 COL 3 WIDGET-ID 34
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.75 BY 16.56 WIDGET-ID 100.


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
         TITLE              = "Loan Acoount Detail"
         HEIGHT             = 16.56
         WIDTH              = 97.75
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
/* BROWSE-TAB BROWSE-Emi-Details BUTTON-PayEMI fMain */
ASSIGN 
       FILL-IN-AccountSubType:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-Balance:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-Duration:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-EmiAmount:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-LoanAccNo:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-ROI:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STATUS IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-STATUS:HIDDEN IN FRAME fMain           = TRUE
       FILL-IN-STATUS:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-TotalAmt:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Emi-Details
/* Query rebuild information for BROWSE BROWSE-Emi-Details
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-Emi.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Emi-Details */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Loan Acoount Detail */
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
ON WINDOW-CLOSE OF wWin /* Loan Acoount Detail */
DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Emi-Details
&Scoped-define SELF-NAME BROWSE-Emi-Details
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Emi-Details wWin
ON ROW-DISPLAY OF BROWSE-Emi-Details IN FRAME fMain
DO:
        IF AVAILABLE tt-Emi AND tt-Emi.RemainingBalance = DECIMAL("") THEN
        DO:
            tt-Emi.EMIStatus:FGCOLOR IN BROWSE BROWSE-Emi-Details = 12.
        END.
        ELSE 
        DO:
           tt-Emi.EMIStatus:FGCOLOR IN BROWSE BROWSE-Emi-Details = 9.
        END.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-PayEMI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-PayEMI wWin
ON CHOOSE OF BUTTON-PayEMI IN FRAME fMain /* Pay EMI */
DO:
       
      
        IF AVAILABLE tt-Emi THEN
        DO:
            /*            MESSAGE tt-Emi.EMIID  */
            /*                VIEW-AS ALERT-BOX.*/
            RUN PayEmi.w(INPUT TABLE tt-Emi , INPUT TABLE ttCustAccntDetails).
            RUN initializeObject.
        END.
        ELSE
        DO:
            MESSAGE iEmiId
            VIEW-AS ALERT-BOX.
            CREATE tt-Emi.
            tt-Emi.EMIID = iEmiId.
            tt-Emi.EMIDate = TODAY.
            tt-Emi.EMIAmount = INT(FILL-IN-EmiAmount:SCREEN-VALUE).
            /*            MESSAGE tt-Emi.EMIID  */
            /*                VIEW-AS ALERT-BOX.*/
            RUN PayEmi.w(INPUT TABLE tt-Emi , INPUT TABLE ttCustAccntDetails).
            RUN initializeObject.
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
  DISPLAY FILL-IN-LoanAccNo FILL-IN-TotalAmt FILL-IN-Balance FILL-IN-ROI 
          FILL-IN-AccountSubType FILL-IN-Duration FILL-IN-EmiAmount 
          FILL-IN-STATUS 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-23 RECT-24 RECT-25 FILL-IN-LoanAccNo FILL-IN-TotalAmt 
         FILL-IN-Balance FILL-IN-ROI FILL-IN-AccountSubType FILL-IN-Duration 
         FILL-IN-EmiAmount BUTTON-PayEMI BROWSE-Emi-Details 
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
     
    oUtility:getInputParameterStructure(INPUT "getLoanAcctDetails",INPUT "",INPUT iAcctNum, INPUT TEMP-TABLE ttEmpty:HANDLE, OUTPUT oInputJson).
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
    /*    FIND FIRST ttCustAccntDetails.    */
    /*    MESSAGE ttCustAccntDetails.AcctNum*/
    /*        VIEW-AS ALERT-BOX.            */
  
    FIND FIRST ttCustAccntDetails NO-LOCK.
    IF AVAIL ttCustAccntDetails THEN 
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN 
                FILL-IN-LoanAccNo:SCREEN-VALUE      = STRING (ttCustAccntDetails.AcctNum)
                FILL-IN-TotalAmt:SCREEN-VALUE       = STRING (ttCustAccntDetails.TotalLoanAmount)
                FILL-IN-Duration:SCREEN-VALUE       = STRING (ttCustAccntDetails.LoanDuration)
                FILL-IN-Balance:SCREEN-VALUE        = STRING (ttCustAccntDetails.Balance)
                FILL-IN-ROI:SCREEN-VALUE            = STRING (ttCustAccntDetails.RateOfInterest) +  "%"
                FILL-IN-AccountSubType:SCREEN-VALUE = ttCustAccntDetails.AccntSubType
                FILL-IN-EmiAmount:SCREEN-VALUE      = STRING(ROUND ((ttCustAccntDetails.TotalLoanAmount / ttCustAccntDetails.LoanDuration) ,3))
                iEmiId                              = ttCustAccntDetails.EMIID.
        END.
    END.    
    /*    MESSAGE iEmiId        */
    /*        VIEW-AS ALERT-BOX.*/
    EMPTY TEMP-TABLE tt-Emi.
    lConnectionStatus = oConnection:getConnect().
    /*    MESSAGE "Connected = "lConnectionStatus*/
    /*        VIEW-AS ALERT-BOX.                 */
          
    oUtility:getInputParameterStructure(INPUT "getEMIDetails",INPUT "",INPUT iEmiId, INPUT TEMP-TABLE tt-Emi:HANDLE, OUTPUT oInputJson).
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
        
    TEMP-TABLE tt-Emi:READ-JSON ("LONGCHAR",lcCustData,"Empty").
    OPEN QUERY BROWSE-Emi-Details FOR EACH tt-Emi.
            
    
    IF AVAILABLE tt-Emi THEN 
    DO:
        IF tt-Emi.RemainingBalance = 0 THEN
        DO:
            BUTTON-PayEMI:SENSITIVE = FALSE.
            FILL-IN-STATUS:HIDDEN = FALSE . 
            FILL-IN-STATUS:SCREEN-VALUE = "YOUR EMI HAS BEEN COMPLETELY PAID".
        END.
    END.

    CATCH e AS Progress.Lang.Error:
        DEFINE VARIABLE stest AS String NO-UNDO.
        stest = NEW string("Error in proc initializeObject").
        stest:append(" ") .
        stest:append(e:GetMessage(1)).
        MESSAGE  stest:ToString() VIEW-AS ALERT-BOX.
    END CATCH.

    FINALLY:
      //  IF VALID-OBJECT (oServerConnection) THEN DELETE OBJECT oServerConnection.
    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

