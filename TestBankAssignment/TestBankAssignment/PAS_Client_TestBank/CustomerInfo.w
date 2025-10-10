&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS wWin 
USING OpenEdge.Core.String FROM PROPATH.
USING Progress.Json.ObjectModel.*.

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


DEFINE TEMP-TABLE ttCustomer
    FIELD CustId        AS INTEGER
    FIELD FirstName     AS CHARACTER
    FIELD LastName      AS CHARACTER
    FIELD Address1      AS CHARACTER
    FIELD Address2      AS CHARACTER
    FIELD City          AS CHARACTER
    FIELD State         AS CHARACTER
    FIELD Country       AS CHARACTER
    FIELD EmailId       AS CHARACTER
    FIELD Phone         AS CHARACTER
    FIELD Mobile        AS CHARACTER
    FIELD DateOfBirth   AS DATE
    FIELD MaritalStatus AS CHARACTER
    FIELD ZIPCode       AS CHARACTER.
  //  INDEX x AS UNIQUE CustId.
    
DEFINE TEMP-TABLE ttCustomerAccount
    FIELD SelectRow     AS LOGICAL
    FIELD CustId        AS INTEGER
    FIELD AcctNum       AS INTEGER
    FIELD AccountTypeID AS INTEGER
    FIELD AccountType   AS CHARACTER .
    
DEFINE TEMP-TABLE ttDeleteInfo
    FIELD CustId      AS INTEGER
    FIELD CustName    AS CHARACTER
    FIELD AcctNum     AS INTEGER
    FIELD AccountType AS CHARACTER .
    
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
    
DEFINE TEMP-TABLE ttEmpty LIKE ttCustomer.

/* Parameters Definitions ---                                           */

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

  Created: 01/24/22 - 10:52 am

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

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BROWSE-CustomerAcctDetails

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomerAccount

/* Definitions for BROWSE BROWSE-CustomerAcctDetails                    */
&Scoped-define FIELDS-IN-QUERY-BROWSE-CustomerAcctDetails ttCustomerAccount.SelectRow ttCustomerAccount.AcctNum ttCustomerAccount.AccountType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-CustomerAcctDetails   
&Scoped-define SELF-NAME BROWSE-CustomerAcctDetails
&Scoped-define QUERY-STRING-BROWSE-CustomerAcctDetails FOR EACH ttCustomerAccount
&Scoped-define OPEN-QUERY-BROWSE-CustomerAcctDetails OPEN QUERY {&SELF-NAME} FOR EACH ttCustomerAccount.
&Scoped-define TABLES-IN-QUERY-BROWSE-CustomerAcctDetails ttCustomerAccount
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-CustomerAcctDetails ttCustomerAccount


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BROWSE-CustomerAcctDetails}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-6 RECT-7 RECT-8 RECT-9 ~
RECT-10 FILL-IN-CustId BUTTON-AdvSearch BUTTON-Clear FILL-IN-FirstName ~
FILL-IN-LastName BUTTON-Add BUTTON-Update FILL-IN-Address1 FILL-IN-Address2 ~
BUTTON-Delete FILL-IN-City FILL-IN-State FILL-IN-Country FILL-IN-PostalCode ~
BUTTON-AddAccount TOGGLE-SelectAll BROWSE-CustomerAcctDetails ~
BUTTON-DeleteAccount BUTTON-UpdateAccount BUTTON-Details 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-CustId FILL-IN-FirstName ~
FILL-IN-LastName FILL-IN-Address1 FILL-IN-Address2 FILL-IN-City ~
FILL-IN-State FILL-IN-Country FILL-IN-PostalCode COMBO-BOX-MaritalStatus ~
TOGGLE-SelectAll 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Add 
     LABEL "Add" 
     SIZE 16 BY 1.

DEFINE BUTTON BUTTON-AddAccount 
     LABEL "Add" 
     SIZE 16 BY 1.13.

DEFINE BUTTON BUTTON-AdvSearch 
     LABEL "Advanced Search" 
     SIZE 22 BY 1.

DEFINE BUTTON BUTTON-Clear 
     LABEL "Clear" 
     SIZE 15 BY 1.

DEFINE BUTTON BUTTON-Delete 
     LABEL "Delete" 
     SIZE 16 BY 1.

DEFINE BUTTON BUTTON-DeleteAccount 
     LABEL "Delete" 
     SIZE 16 BY 1.13.

DEFINE BUTTON BUTTON-Details 
     LABEL "Details" 
     SIZE 16 BY 1.13.

DEFINE BUTTON BUTTON-Search 
     LABEL "Search" 
     SIZE 16 BY 1.

DEFINE BUTTON BUTTON-Update 
     LABEL "Update" 
     SIZE 16 BY 1.

DEFINE BUTTON BUTTON-UpdateAccount 
     LABEL "Update" 
     SIZE 16 BY 1.13.

DEFINE VARIABLE COMBO-BOX-MaritalStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Marital Status" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Single","Married","Divorced","Single Parent" 
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Address1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Addr Line 1" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Address2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Addr Line 2" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-City AS CHARACTER FORMAT "X(256)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Country AS CHARACTER FORMAT "X(256)":U 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-CustId AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Customer Id" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-FirstName AS CHARACTER FORMAT "X(256)":U 
     LABEL "First Name" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LastName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Last Name" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PostalCode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Postal Code" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-State AS CHARACTER FORMAT "X(256)":U 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 1.53.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 7.25.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 7.25.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 8.53
     BGCOLOR 11 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 8.25
     BGCOLOR 11 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 7.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 18 BY 7.

DEFINE VARIABLE TOGGLE-SelectAll AS LOGICAL INITIAL no 
     LABEL "SelectAll" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .72 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-CustomerAcctDetails FOR 
      ttCustomerAccount SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-CustomerAcctDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-CustomerAcctDetails wWin _FREEFORM
  QUERY BROWSE-CustomerAcctDetails DISPLAY
      ttCustomerAccount.SelectRow VIEW-AS TOGGLE-BOX
    ttCustomerAccount.AcctNum  FORMAT "999999"
    ttCustomerAccount.AccountType FORMAT "x(30)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 77 BY 5.75 ROW-HEIGHT-CHARS .69 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FILL-IN-CustId AT ROW 2.75 COL 16 COLON-ALIGNED WIDGET-ID 6
     BUTTON-Search AT ROW 2.75 COL 43 WIDGET-ID 8
     BUTTON-AdvSearch AT ROW 2.75 COL 61 WIDGET-ID 10
     BUTTON-Clear AT ROW 2.75 COL 85 WIDGET-ID 64
     FILL-IN-FirstName AT ROW 5.53 COL 16 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-LastName AT ROW 5.53 COL 55 COLON-ALIGNED WIDGET-ID 24
     BUTTON-Add AT ROW 5.53 COL 83 WIDGET-ID 40
     BUTTON-Update AT ROW 6.75 COL 83 WIDGET-ID 42
     FILL-IN-Address1 AT ROW 7 COL 16 COLON-ALIGNED WIDGET-ID 28
     FILL-IN-Address2 AT ROW 7 COL 55 COLON-ALIGNED WIDGET-ID 30
     BUTTON-Delete AT ROW 8 COL 83 WIDGET-ID 44
     FILL-IN-City AT ROW 8.53 COL 16 COLON-ALIGNED WIDGET-ID 32
     FILL-IN-State AT ROW 8.53 COL 55 COLON-ALIGNED WIDGET-ID 34
     FILL-IN-Country AT ROW 10 COL 16 COLON-ALIGNED WIDGET-ID 36
     FILL-IN-PostalCode AT ROW 10 COL 55 COLON-ALIGNED WIDGET-ID 38
     COMBO-BOX-MaritalStatus AT ROW 11.25 COL 55 COLON-ALIGNED WIDGET-ID 48
     BUTTON-AddAccount AT ROW 14.25 COL 83 WIDGET-ID 50
     TOGGLE-SelectAll AT ROW 14.28 COL 4 WIDGET-ID 66
     BROWSE-CustomerAcctDetails AT ROW 15 COL 4 WIDGET-ID 200
     BUTTON-DeleteAccount AT ROW 15.75 COL 83 WIDGET-ID 52
     BUTTON-UpdateAccount AT ROW 17.25 COL 83 WIDGET-ID 70
     BUTTON-Details AT ROW 18.75 COL 83 WIDGET-ID 78
     "CUSTOMER DETAILS" VIEW-AS TEXT
          SIZE 27 BY 1.13 AT ROW 1.25 COL 38 WIDGET-ID 54
          FONT 5
     "Account Details" VIEW-AS TEXT
          SIZE 20 BY .63 AT ROW 13.25 COL 3 WIDGET-ID 60
          FONT 2
     "Customer Information" VIEW-AS TEXT
          SIZE 31 BY .53 AT ROW 4.63 COL 3 WIDGET-ID 62
          FONT 2
     RECT-1 AT ROW 2.53 COL 2 WIDGET-ID 2
     RECT-2 AT ROW 5.25 COL 3 WIDGET-ID 12
     RECT-6 AT ROW 4.25 COL 2 WIDGET-ID 56
     RECT-7 AT ROW 13 COL 2 WIDGET-ID 58
     RECT-8 AT ROW 14 COL 3 WIDGET-ID 68
     RECT-9 AT ROW 14 COL 82 WIDGET-ID 74
     RECT-10 AT ROW 5.25 COL 82 WIDGET-ID 76
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 156 BY 23.31
         DEFAULT-BUTTON BUTTON-Search WIDGET-ID 100.


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
         TITLE              = "Customer Info"
         HEIGHT             = 20.31
         WIDTH              = 100.63
         MAX-HEIGHT         = 28.78
         MAX-WIDTH          = 158.63
         VIRTUAL-HEIGHT     = 28.78
         VIRTUAL-WIDTH      = 158.63
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
/* BROWSE-TAB BROWSE-CustomerAcctDetails TOGGLE-SelectAll fMain */
/* SETTINGS FOR BUTTON BUTTON-Search IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX COMBO-BOX-MaritalStatus IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-Address1:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-Address2:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-City:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-Country:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-FirstName:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-LastName:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-PostalCode:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       FILL-IN-State:READ-ONLY IN FRAME fMain        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-CustomerAcctDetails
/* Query rebuild information for BROWSE BROWSE-CustomerAcctDetails
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttCustomerAccount.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-CustomerAcctDetails */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Customer Info */
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
ON WINDOW-CLOSE OF wWin /* Customer Info */
DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-CustomerAcctDetails
&Scoped-define SELF-NAME BROWSE-CustomerAcctDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-CustomerAcctDetails wWin
ON MOUSE-SELECT-CLICK OF BROWSE-CustomerAcctDetails IN FRAME fMain
DO:
        DEFINE VARIABLE hColumn AS HANDLE NO-UNDO.
        DEFINE VARIABLE hBrowse AS HANDLE NO-UNDO.

        hBrowse = BROWSE-CustomerAcctDetails:HANDLE.
        hColumn = hBrowse:GET-BROWSE-COLUMN (1).
        IF hColumn:CHECKED = FALSE THEN
        DO:
            hColumn:CHECKED = TRUE.
            ttCustomerAccount.SelectRow = TRUE.
        END.
        ELSE
        DO:
            hColumn:CHECKED = FALSE.
            ttCustomerAccount.SelectRow = FALSE.
        END.
        CATCH e AS Progress.Lang.Error:
            MESSAGE SUBSTITUTE (e:GetMessage(1))
                VIEW-AS ALERT-BOX.

        END CATCH.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-CustomerAcctDetails wWin
ON MOUSE-SELECT-DBLCLICK OF BROWSE-CustomerAcctDetails IN FRAME fMain
DO:
        DEFINE VARIABLE iAcctNum  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cAcctType AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iCustId   AS INTEGER   NO-UNDO.
        
        iCustId = INT(FILL-IN-CustId:SCREEN-VALUE).
        iAcctNum = ttCustomerAccount.AcctNum.
        cAcctType = ttCustomerAccount.AccountType.
         
        FOR EACH ttCustomerAccount NO-LOCK:
            ttCustomerAccount.SelectRow = FALSE.
        END.
        BROWSE-CustomerAcctDetails:REFRESH ().   
        
        IF cAcctType = "Savings Account" THEN
        DO:  
            RUN SavingAccountDetail.w(INPUT iAcctNum,INPUT iCustId).
        END.
        IF cAcctType = "Loan Account" THEN
        DO:  
            RUN LoanAccountDetail.w(INPUT iAcctNum).
        END.
        CATCH e AS Progress.Lang.Error:
            MESSAGE SUBSTITUTE (e:GetMessage(1))
                VIEW-AS ALERT-BOX.

        END CATCH.
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-CustomerAcctDetails wWin
ON RETURN OF BROWSE-CustomerAcctDetails IN FRAME fMain
DO:
        APPLY "CHOOSE" TO BUTTON-Details.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-CustomerAcctDetails wWin
ON VALUE-CHANGED OF BROWSE-CustomerAcctDetails IN FRAME fMain
DO:
    /*            DEFINE VARIABLE hColumn AS HANDLE NO-UNDO.  */
    /*            DEFINE VARIABLE hBrowse AS HANDLE NO-UNDO.  */
    /*                                                        */
    /*            hBrowse = BROWSE-CustomerAcctDetails:HANDLE.*/
    /*            hColumn = hBrowse:GET-BROWSE-COLUMN (1).    */
    /*            IF hColumn:CHECKED = FALSE THEN             */
    /*            DO:                                         */
    /*                hColumn:CHECKED = TRUE.                 */
    /*                ttCustomerAccount.SelectRow = TRUE.     */
    /*            END.                                        */
    /*            ELSE                                        */
    /*            DO:                                         */
    /*                hColumn:CHECKED = FALSE.                */
    /*                ttCustomerAccount.SelectRow = FALSE.    */
    /*            END.                                        */


    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Add wWin
ON CHOOSE OF BUTTON-Add IN FRAME fMain /* Add */
DO:
        DEFINE VARIABLE iCustId AS INTEGER NO-UNDO.
        
        EMPTY TEMP-TABLE ttCustomer.
        RUN AddorUpdCust.w(INPUT TABLE ttCustomer,INPUT iCustId).
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-AddAccount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-AddAccount wWin
ON CHOOSE OF BUTTON-AddAccount IN FRAME fMain /* Add */
DO:
        DEFINE VARIABLE iCustId AS INTEGER NO-UNDO.
        
        iCustId = INT(FILL-IN-CustId:SCREEN-VALUE).
        
        RUN AddAccount.w(INPUT iCustId).
        
        APPLY "CHOOSE" TO BUTTON-Search.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-AdvSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-AdvSearch wWin
ON CHOOSE OF BUTTON-AdvSearch IN FRAME fMain /* Advanced Search */
DO:
        DEFINE VARIABLE iCustId AS INTEGER NO-UNDO.
      
        RUN CustomerAdvanceSearch.w(OUTPUT iCustId).
        
        IF iCustId <> 0 THEN
        DO:
            FILL-IN-CustId:SCREEN-VALUE = STRING(iCustId).
            
            APPLY "CHOOSE" TO BUTTON-Search.
        END.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Clear wWin
ON CHOOSE OF BUTTON-Clear IN FRAME fMain /* Clear */
DO:
        RUN enable_UI.
        
        RUN initializeObject.
        
        EMPTY TEMP-TABLE ttCustomerAccount.
        
        OPEN QUERY BROWSE-CustomerAcctDetails FOR EACH ttCustomerAccount.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Delete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Delete wWin
ON CHOOSE OF BUTTON-Delete IN FRAME fMain /* Delete */
DO:
        DEFINE VARIABLE iCustId           AS INTEGER          NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcStatus          AS LONGCHAR         NO-UNDO. 
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        
        iCustId = INT(FILL-IN-CustId:SCREEN-VALUE).
        
        lConnectionStatus = oConnection:getConnect().
        /*        MESSAGE "Connected = "lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                 */
                
        oUtility:getInputParameterStructure(INPUT "DeleteCustomerDetails", INPUT "", INPUT iCustId, INPUT TEMP-TABLE ttEmpty:HANDLE,OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX BUTTON OK.
        IF VALID-HANDLE (oConnection:hAppServerHandle) THEN   
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcStatus).
        ELSE
            lConnectionStatus = oConnection:getDisconnect().
 
        lConnectionStatus = oConnection:getDisconnect().
        /*        MESSAGE "Disconnected = " lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                     */
                
        MESSAGE STRING(lcStatus)
            VIEW-AS ALERT-BOX.
            
        RUN enable_UI.                                                                                                                                                                                                                      
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-DeleteAccount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-DeleteAccount wWin
ON CHOOSE OF BUTTON-DeleteAccount IN FRAME fMain /* Delete */
DO:
        FOR EACH ttCustomerAccount
            WHERE ttCustomerAccount.SelectRow = TRUE EXCLUSIVE-LOCK:
            /*            MESSAGE ttCustomerAccount.AcctNum*/
            /*                VIEW-AS ALERT-BOX.           */
            EMPTY TEMP-TABLE ttDeleteInfo.
            FIND FIRST ttCustomer.
            MESSAGE ttCustomer.CustId
                VIEW-AS ALERT-BOX.
            CREATE ttDeleteInfo.
            DO:
                ttDeleteInfo.CustId = ttCustomer.CustId.
                ttDeleteInfo.CustName = ttCustomer.FirstName + ttCustomer.LastName.
                ttDeleteInfo.AcctNum = ttCustomerAccount.AcctNum.
                ttDeleteInfo.AccountType = ttCustomerAccount.AccountType.

            END.

 //       RUN gDialog-DeleteAccount.w(INPUT TABLE ttCustomer,INPUT TABLE ttCustomerAccount).
            RUN gDialog-DeleteAccount.w(INPUT TABLE ttDeleteInfo).
           
        END.
        APPLY "CHOOSE" TO BUTTON-Search.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Details
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Details wWin
ON CHOOSE OF BUTTON-Details IN FRAME fMain /* Details */
DO:
        DEFINE VARIABLE iAcctNum  AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cAcctType AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iCustId   AS INTEGER   NO-UNDO.
        
        FOR EACH ttCustomerAccount
            WHERE ttCustomerAccount.SelectRow = TRUE EXCLUSIVE-LOCK:
               
            iCustId = INT(FILL-IN-CustId:SCREEN-VALUE).
            iAcctNum = ttCustomerAccount.AcctNum.
            cAcctType = ttCustomerAccount.AccountType.
            
            IF cAcctType = "Savings Account" THEN
            DO:  
                RUN SavingAccountDetail.w(INPUT iAcctNum,INPUT iCustId).
            END.
            IF cAcctType = "Loan Account" THEN
            DO:  
                RUN LoanAccountDetail.w(INPUT iAcctNum).
            END.
                
        END.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Search wWin
ON CHOOSE OF BUTTON-Search IN FRAME fMain /* Search */
DO:
        DEFINE VARIABLE iCustNum             AS INTEGER          NO-UNDO.
        DEFINE VARIABLE cCustName            AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE lConnectionStatus    AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcCustData           AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE lcInput              AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection          AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson           AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oInputParametersJson AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oTTJson              AS JsonArray        NO-UNDO.
        DEFINE VARIABLE oUtility             AS ClientUtility    NO-UNDO.
        
        RUN initializeObject.
        /* DO WITH FRAME {&FRAME-NAME}: */
        /*     ENABLE BUTTON-Clear.     */
        /*     ENABLE BUTTON-Delete.    */
        /*     ENABLE BUTTON-Update.    */
        /*     ENABLE BUTTON-AddAccount.*/
        /*     DISABLE BUTTON-Add.      */
        ASSIGN
            BUTTON-Clear:SENSITIVE      = TRUE
            BUTTON-Delete:SENSITIVE     = TRUE
            BUTTON-Update:SENSITIVE     = TRUE
            BUTTON-AddAccount:SENSITIVE = TRUE
            BUTTON-Add:SENSITIVE        = FALSE.
      
        ASSIGN
            FILL-IN-FirstName:SCREEN-VALUE       = ""
            FILL-IN-LastName:SCREEN-VALUE        = ""
            FILL-IN-Address1:SCREEN-VALUE        = ""
            FILL-IN-Address2:SCREEN-VALUE        = ""
            FILL-IN-City:SCREEN-VALUE            = ""
            FILL-IN-State:SCREEN-VALUE           = ""
            FILL-IN-Country:SCREEN-VALUE         = ""
            FILL-IN-PostalCode:SCREEN-VALUE      = ""
            COMBO-BOX-MaritalStatus:SCREEN-VALUE = "".
            
        EMPTY TEMP-TABLE ttCustomer.
        IF  AVAILABLE ttCustomerAccount THEN
        DO:
            EMPTY TEMP-TABLE ttCustomerAccount.
            BROWSE-CustomerAcctDetails:REFRESH ().
        END.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        iCustNum = INT(FILL-IN-CustId:SCREEN-VALUE).

        lConnectionStatus = oConnection:getConnect().
        /*        MESSAGE "Connected = "lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                 */
            
        oInputJson = NEW JsonObject(). 
        oUtility:getInputParameterStructure(INPUT "getCustomerDetailsByCustId",INPUT "",INPUT iCustNum, INPUT TEMP-TABLE ttEmpty:HANDLE, OUTPUT oInputJson).
        /*        MESSAGE STRING(oInputJson:GetJsonText())*/
        /*            VIEW-AS ALERT-BOX.                  */
        oInputJson:WRITE(lcInput,TRUE,"UTF-8").
        MESSAGE STRING(lcInput)
            VIEW-AS ALERT-BOX.
       // oInputJson:WriteFile("C:\Users\kishore.m\Documents\Progress\InputJson.txt").     
        IF VALID-HANDLE (oConnection:hAppServerHandle) THEN
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcCustData).
        ELSE
            lConnectionStatus = oConnection:getDisconnect().
        
        lConnectionStatus = oConnection:getDisconnect().
        /*        MESSAGE "Disconnected = " lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                     */
        MESSAGE STRING(lcCustData)
            VIEW-AS ALERT-BOX.   
        IF lcCustData = "" THEN 
        DO:
            BUTTON-Update:SENSITIVE = FALSE.
            BUTTON-Delete:SENSITIVE = FALSE.
            BUTTON-AddAccount:SENSITIVE = FALSE.
            UNDO, THROW NEW Progress.Lang.AppError("Customer Details Not Found...",1).
        END.
        ELSE    
            TEMP-TABLE ttCustomer:READ-JSON ("LONGCHAR",lcCustData,"Empty").
     
        FIND FIRST ttCustomer.   
        ASSIGN
            FILL-IN-FirstName:SCREEN-VALUE  = ttCustomer.FirstName
            FILL-IN-LastName:SCREEN-VALUE   = ttCustomer.LastName
            FILL-IN-Address1:SCREEN-VALUE   = ttCustomer.Address1
            FILL-IN-Address2:SCREEN-VALUE   = ttCustomer.Address2
            FILL-IN-City:SCREEN-VALUE       = ttCustomer.City
            FILL-IN-State:SCREEN-VALUE      = ttCustomer.State
            FILL-IN-Country:SCREEN-VALUE    = ttCustomer.Country
            FILL-IN-PostalCode:SCREEN-VALUE = ttCustomer.ZIPCode.
        IF LOOKUP (ttCustomer.MaritalStatus, COMBO-BOX-MaritalStatus:LIST-ITEMS) > 0 THEN
            COMBO-BOX-MaritalStatus:SCREEN-VALUE = ttCustomer.MaritalStatus.

        lConnectionStatus = oConnection:getConnect().
        /*        MESSAGE "Connected = "lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                 */
            
        
        oUtility:getInputParameterStructure(INPUT "getCustomerAccountDetailsByCustId",INPUT "",INPUT iCustNum, INPUT TEMP-TABLE ttEmpty:HANDLE, OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX.
        IF VALID-HANDLE (oConnection:hAppServerHandle) THEN
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcCustData).
       
        
        lConnectionStatus = oConnection:getDisconnect().
        /*        MESSAGE "Disconnected = " lConnectionStatus*/
        /*            VIEW-AS ALERT-BOX.                     */
        MESSAGE STRING(lcCustData)
            VIEW-AS ALERT-BOX. 

        TEMP-TABLE ttCustomerAccount:READ-JSON ("LONGCHAR",lcCustData,"Empty").
        OPEN QUERY BROWSE-CustomerAcctDetails FOR EACH ttCustomerAccount.
     
        IF  AVAILABLE ttCustomerAccount THEN 
        DO:
            BUTTON-Delete:SENSITIVE = FALSE.
            BUTTON-DeleteAccount:SENSITIVE = TRUE.
            TOGGLE-SelectAll:SENSITIVE = TRUE.
            BUTTON-UpdateAccount:SENSITIVE = TRUE.
            BUTTON-Details:SENSITIVE = TRUE.
        END.
        /*DO WITH FRAME {&FRAME-NAME}:    */
        /*    ENABLE BUTTON-DeleteAccount.*/
        /*                                */
        /*END.                            */
   
        CATCH e AS Progress.Lang.Error:
            
            /* DEFINE VARIABLE stest AS String NO-UNDO.         */
            /* stest = NEW string("Error in .w  ").             */
            /* // stest:append(THIS-OBJECT:GetClass():TypeName).*/
            /*  stest:append(" ") .                             */
            /*  stest:append(e:GetMessage(1)).                  */
            /*  MESSAGE stest:ToString().                       */
            MESSAGE SUBSTITUTE ("For CustId= &1 , &2 ",FILL-IN-CustId:SCREEN-VALUE ,e:GetMessage(1))
                VIEW-AS ALERT-BOX.

        END CATCH.

        FINALLY:
            
            IF VALID-OBJECT (oConnection) THEN DELETE OBJECT oConnection.

        END FINALLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Search wWin
ON F5 OF BUTTON-Search IN FRAME fMain /* Search */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Update wWin
ON CHOOSE OF BUTTON-Update IN FRAME fMain /* Update */
DO:
        DEFINE VARIABLE iCustId AS INTEGER NO-UNDO.
        iCustId = INT(FILL-IN-CustId:SCREEN-VALUE).
        
        RUN AddorUpdCust.w(INPUT TABLE ttCustomer,INPUT iCustId).
        
        APPLY "CHOOSE" TO BUTTON-Search.
    
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-UpdateAccount
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-UpdateAccount wWin
ON CHOOSE OF BUTTON-UpdateAccount IN FRAME fMain /* Update */
DO:
   
        DEFINE VARIABLE iAcctNum             AS INTEGER          NO-UNDO.
        DEFINE VARIABLE cAcctType            AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE lConnectionStatus    AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcCustData           AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection          AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson           AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oInputParametersJson AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oTTJson              AS JsonArray        NO-UNDO.
        DEFINE VARIABLE oUtility             AS ClientUtility    NO-UNDO.
        DEFINE VARIABLE l                    AS LOGICAL          NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        
        FOR EACH ttCustomerAccount
            WHERE ttCustomerAccount.SelectRow = TRUE EXCLUSIVE-LOCK:
           
            iAcctNum = ttCustomerAccount.AcctNum.
            cAcctType = ttCustomerAccount.AccountType.
            
            EMPTY TEMP-TABLE ttCustAccntDetails.

            lConnectionStatus = oConnection:getConnect().
            /*            MESSAGE "Connected = "lConnectionStatus*/
            /*                VIEW-AS ALERT-BOX.                 */
            IF cAcctType = "Savings Account" THEN
            DO:  
                oUtility:getInputParameterStructure(INPUT "getSavAcctDetails",INPUT "",INPUT iAcctNum, INPUT TEMP-TABLE ttEmpty:HANDLE, OUTPUT oInputJson).
                MESSAGE STRING(oInputJson:GetJsonText())
                    VIEW-AS ALERT-BOX.
            END.
            IF cAcctType = "Loan Account" THEN
            DO:  
                oUtility:getInputParameterStructure(INPUT "getLoanAcctDetails",INPUT "",INPUT iAcctNum, INPUT TEMP-TABLE ttEmpty:HANDLE, OUTPUT oInputJson).
                MESSAGE STRING(oInputJson:GetJsonText())
                    VIEW-AS ALERT-BOX.
            END.
            
        
            IF VALID-HANDLE (oConnection:hAppServerHandle) THEN
                RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcCustData).
            ELSE
                lConnectionStatus = oConnection:getDisconnect().
        
            lConnectionStatus = oConnection:getDisconnect().
            /*            MESSAGE "Disconnected = " lConnectionStatus*/
            /*                VIEW-AS ALERT-BOX.                     */
            MESSAGE STRING(lcCustData)
                VIEW-AS ALERT-BOX.    
            l= TEMP-TABLE ttCustAccntDetails:READ-JSON ("LONGCHAR",lcCustData,"Empty").
            /*            MESSAGE l                         */
            /*                VIEW-AS ALERT-BOX.            */
            /*            FIND FIRST ttCustAccntDetails.    */
            /*            MESSAGE ttCustAccntDetails.AcctNum*/
            /*                VIEW-AS ALERT-BOX.            */
            RUN UpdateAccount.w(INPUT TABLE ttCustAccntDetails ).
                 
        END.
       
        
        
       
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CustId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CustId wWin
ON VALUE-CHANGED OF FILL-IN-CustId IN FRAME fMain /* Customer Id */
DO:
        DEFINE VARIABLE iCustId AS INTEGER NO-UNDO.
        iCustId = INT(FILL-IN-CustId:SCREEN-VALUE).
        IF (iCustId <> 0 ) THEN
        DO:
        
            BUTTON-Search:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            BUTTON-Search:SENSITIVE = FALSE.
        END.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-SelectAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-SelectAll wWin
ON VALUE-CHANGED OF TOGGLE-SelectAll IN FRAME fMain /* SelectAll */
DO:
        DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
        DEFINE VARIABLE hBrowse AS HANDLE  NO-UNDO.
        DEFINE VARIABLE iCount  AS INTEGER NO-UNDO.
        DEFINE VARIABLE lRes    AS LOGICAL NO-UNDO.

        hBrowse = BROWSE-CustomerAcctDetails:HANDLE.

        IF TOGGLE-SelectAll:CHECKED = TRUE THEN
        DO:
            DO iCount = 1 TO NUM-RESULTS("BROWSE-CustomerAcctDetails"):
                lRes = hBrowse:SELECT-ROW (iCount).
                hColumn = hBrowse:GET-BROWSE-COLUMN (1).
                hColumn:CHECKED = TRUE.
                ttCustomerAccount.SelectRow = YES. 
            END.
        END.
        ELSE
        DO iCount = 1 TO NUM-RESULTS("BROWSE-CustomerAcctDetails"):
            lRes = hBrowse:SELECT-ROW (iCount).
            hColumn = hBrowse:GET-BROWSE-COLUMN (1).
            hColumn:CHECKED = FALSE.
            ttCustomerAccount.SelectRow = NO. 
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
  DISPLAY FILL-IN-CustId FILL-IN-FirstName FILL-IN-LastName FILL-IN-Address1 
          FILL-IN-Address2 FILL-IN-City FILL-IN-State FILL-IN-Country 
          FILL-IN-PostalCode COMBO-BOX-MaritalStatus TOGGLE-SelectAll 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-1 RECT-2 RECT-6 RECT-7 RECT-8 RECT-9 RECT-10 FILL-IN-CustId 
         BUTTON-AdvSearch BUTTON-Clear FILL-IN-FirstName FILL-IN-LastName 
         BUTTON-Add BUTTON-Update FILL-IN-Address1 FILL-IN-Address2 
         BUTTON-Delete FILL-IN-City FILL-IN-State FILL-IN-Country 
         FILL-IN-PostalCode BUTTON-AddAccount TOGGLE-SelectAll 
         BROWSE-CustomerAcctDetails BUTTON-DeleteAccount BUTTON-UpdateAccount 
         BUTTON-Details 
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
    IF SESSION:DEBUG-ALERT = TRUE THEN
    DO:
        SESSION:DEBUG-ALERT = FALSE.
    END.
    DO WITH FRAME {&FRAME-NAME}:
        
        BUTTON-Clear:SENSITIVE = FALSE.
        DISABLE BUTTON-Delete.
        DISABLE BUTTON-Update.
        DISABLE BUTTON-AddAccount.
        DISABLE BUTTON-DeleteAccount.
        DISABLE BUTTON-UpdateAccount.
        DISABLE BUTTON-Details.
        TOGGLE-SelectAll:SENSITIVE = FALSE.
       
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

