&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS wWin 
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

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER iCustId AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */
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
    
/*DEFINE TEMP-TABLE ttCustAccntDetails*/
/*    FIELD AccntType     AS CHARACTER*/
/*    FIELD AccntSubType  AS CHARACTER*/
/*    FIELD IFSCCode      AS CHARACTER*/
/*    FIELD RateofInt     AS DECIMAL  */
/*    FIELD LoanDuration  AS INTEGER  */
/*    FIELD TotalLoanAmt  AS INTEGER  */
/*    FIELD TransferLimit AS INTEGER. */

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

  Created: 02/10/22 -  8:55 am

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
&Scoped-define BROWSE-NAME BROWSE-FilteredCustomerDetails

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomer

/* Definitions for BROWSE BROWSE-FilteredCustomerDetails                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-FilteredCustomerDetails ttCustomer.CustId ttCustomer.FirstName ttCustomer.LastName ttCustomer.Mobile ttCustomer.EmailId   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-FilteredCustomerDetails   
&Scoped-define SELF-NAME BROWSE-FilteredCustomerDetails
&Scoped-define QUERY-STRING-BROWSE-FilteredCustomerDetails FOR EACH ttCustomer
&Scoped-define OPEN-QUERY-BROWSE-FilteredCustomerDetails OPEN QUERY {&SELF-NAME} FOR EACH ttCustomer.
&Scoped-define TABLES-IN-QUERY-BROWSE-FilteredCustomerDetails ttCustomer
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-FilteredCustomerDetails ttCustomer


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BROWSE-FilteredCustomerDetails}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 RECT-9 RECT-10 RECT-11 RECT-12 ~
FILL-IN-FirstName FILL-IN-LastName FILL-IN-MobileNo FILL-IN-EmailId ~
BROWSE-FilteredCustomerDetails BUTTON-Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-FirstName FILL-IN-LastName ~
FILL-IN-MobileNo FILL-IN-EmailId 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 14 BY 1.

DEFINE BUTTON BUTTON-Ok AUTO-END-KEY 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON BUTTON-Search 
     LABEL "Search" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-EmailId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Email Address" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FILL-IN-FirstName AS CHARACTER FORMAT "X(256)":U 
     LABEL "First Name" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LastName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Last Name" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FILL-IN-MobileNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mobile #" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     FGCOLOR 9  NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 7.75.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 6.5.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 3.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 99 BY 3.5.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 3.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-FilteredCustomerDetails FOR 
      ttCustomer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-FilteredCustomerDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-FilteredCustomerDetails wWin _FREEFORM
  QUERY BROWSE-FilteredCustomerDetails DISPLAY
      ttCustomer.CustId  FORMAT "99999"
    ttCustomer.FirstName FORMAT "x(30)"
    ttCustomer.LastName FORMAT "x(30)"
    ttCustomer.Mobile FORMAT "x(15)"
    ttCustomer.EmailId FORMAT "x(50)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80 BY 6.5 ROW-HEIGHT-CHARS .69 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FILL-IN-FirstName AT ROW 3.25 COL 15 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-LastName AT ROW 3.25 COL 56 COLON-ALIGNED WIDGET-ID 6
     BUTTON-Search AT ROW 3.25 COL 86 WIDGET-ID 12
     FILL-IN-MobileNo AT ROW 4.75 COL 15 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-EmailId AT ROW 4.75 COL 56 COLON-ALIGNED WIDGET-ID 10
     BROWSE-FilteredCustomerDetails AT ROW 7.5 COL 4 WIDGET-ID 200
     BUTTON-Ok AT ROW 7.75 COL 86 WIDGET-ID 14
     BUTTON-Cancel AT ROW 9 COL 86 WIDGET-ID 16
     "FILTER CUSTOMER DETAILS" VIEW-AS TEXT
          SIZE 36 BY 1.13 AT ROW 1.25 COL 33 WIDGET-ID 18
          FONT 5
     "Customer Details" VIEW-AS TEXT
          SIZE 21 BY .63 AT ROW 6.75 COL 4 WIDGET-ID 24
          FONT 2
     RECT-5 AT ROW 2.75 COL 3 WIDGET-ID 2
     RECT-9 AT ROW 3 COL 4 WIDGET-ID 20
     RECT-10 AT ROW 6.5 COL 3 WIDGET-ID 22
     RECT-11 AT ROW 7.5 COL 85 WIDGET-ID 26
     RECT-12 AT ROW 3 COL 85 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 118.88 BY 16.19
         CANCEL-BUTTON BUTTON-Cancel WIDGET-ID 100.


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
         TITLE              = "Customer Advance Search"
         HEIGHT             = 13.59
         WIDTH              = 103
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
/* BROWSE-TAB BROWSE-FilteredCustomerDetails FILL-IN-EmailId fMain */
/* SETTINGS FOR BUTTON BUTTON-Ok IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Search IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-FilteredCustomerDetails
/* Query rebuild information for BROWSE BROWSE-FilteredCustomerDetails
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttCustomer.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-FilteredCustomerDetails */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Customer Advance Search */
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
ON WINDOW-CLOSE OF wWin /* Customer Advance Search */
DO:
        /* This ADM code must be left here in order for the SmartWindow
           and its descendents to terminate properly on exit. */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-FilteredCustomerDetails
&Scoped-define SELF-NAME BROWSE-FilteredCustomerDetails
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-FilteredCustomerDetails wWin
ON MOUSE-SELECT-DBLCLICK OF BROWSE-FilteredCustomerDetails IN FRAME fMain
DO:

        iCustId = ttCustomer.CustId.
       
        IF iCustId = 0 THEN
        DO:
            MESSAGE "U have not selected any data..."
                VIEW-AS ALERT-BOX.
        END.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        CATCH e AS Progress.Lang.Error:
         //   UNDO, THROW NEW Progress.Lang.AppError("Customer Details Not Found...",1).
            MESSAGE SUBSTITUTE (e:GetMessage(1))
                VIEW-AS ALERT-BOX.

        END CATCH.
      //  APPLY "CHOOSE" TO BUTTON-Cancel.
        
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok wWin
ON CHOOSE OF BUTTON-Ok IN FRAME fMain /* Ok */
DO:
        iCustId = ttCustomer.CustId.
       
        IF iCustId = 0 THEN
        DO:
            MESSAGE "U have not selected any data..."
                VIEW-AS ALERT-BOX.
        END.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Search wWin
ON CHOOSE OF BUTTON-Search IN FRAME fMain /* Search */
DO:
        DEFINE VARIABLE iCustNum          AS INTEGER          NO-UNDO.
        DEFINE VARIABLE cCustName         AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcCustData        AS LONGCHAR         NO-UNDO. 
        DEFINE VARIABLE cWhereClause      AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE cFirstName        AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE cLastName         AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE cMobileNo         AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE cEmailId          AS CHARACTER        NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE lRet              AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
       
        EMPTY TEMP-TABLE ttCustomer.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        
        
        ASSIGN
            cFirstName = FILL-IN-FirstName:SCREEN-VALUE
            cLastName  = FILL-IN-LastName:SCREEN-VALUE
            cMobileNo  = FILL-IN-MobileNo:SCREEN-VALUE
            cEmailId   = FILL-IN-EmailId:SCREEN-VALUE.
       
            
        IF((cFirstName = "") AND (cLastName = "" ) AND (cMobileNo = "") AND (cEmailId = "")) THEN
        DO:
            MESSAGE "PLs Enter atleast one Field To Filter the Customer Details..."
                VIEW-AS ALERT-BOX.
        END.
        ELSE
        DO: 
            IF cFirstName <> "" THEN 
            DO: 
                cWhereClause = cWhereClause + (IF cWhereClause = "" THEN " Where " ELSE " AND " ) + " FirstName = " + quoter(cFirstName). 
            END. 
            IF cLastName <> "" THEN 
            DO: 
                cWhereClause = cWhereClause + (IF cWhereClause = "" THEN " Where " ELSE " AND " ) + " LastName = " + quoter(cLastName). 
            END. 
            IF cMobileNo <> "" THEN 
            DO: 
                cWhereClause = cWhereClause + (IF cWhereClause = "" THEN " Where " ELSE " AND " ) + " Mobile = " + quoter(cMobileNo). 
            END.
            IF cEmailId <> "" THEN 
            DO: 
                cWhereClause = cWhereClause + (IF cWhereClause = "" THEN " Where " ELSE " AND " ) + " EmailId = " + quoter(cEmailId). 
            END. 

            MESSAGE cWhereClause 
                VIEW-AS ALERT-BOX. 
           
            lConnectionStatus = oConnection:getConnect().
            /*            MESSAGE "Connected = "lConnectionStatus*/
            /*                VIEW-AS ALERT-BOX.                 */
                
            oUtility:getInputParameterStructure(INPUT "getCustomerDetailsByFilter", INPUT cWhereClause, INPUT 0, INPUT TEMP-TABLE ttCustomer:HANDLE,OUTPUT oInputJson).
            MESSAGE STRING(oInputJson:GetJsonText())
                VIEW-AS ALERT-BOX.
            IF VALID-HANDLE (oConnection:hAppServerHandle) THEN   
                RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcCustData).
            ELSE
                lConnectionStatus = oConnection:getDisconnect().
 //         RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT "getCustomerDetailsByFilter",INPUT iCustId,OUTPUT lcCustData,INPUT cWhereClause ,INPUT TABLE ttCustomer,INPUT TABLE ttCustAccntDetails,INPUT "",INPUT 0).
 
            lConnectionStatus = oConnection:getDisconnect().
            /*            MESSAGE "Disconnected = " lConnectionStatus*/
            /*                VIEW-AS ALERT-BOX.                     */
                
            MESSAGE STRING(lcCustData)
                VIEW-AS ALERT-BOX.
            lRet = TEMP-TABLE ttCustomer:READ-JSON ("LONGCHAR",lcCustData,"Empty").
            /*            MESSAGE lRet          */
            /*                VIEW-AS ALERT-BOX.*/
       
            OPEN QUERY BROWSE-FilteredCustomerDetails FOR EACH ttCustomer.
            IF  AVAILABLE ttCustomer THEN 
            DO:
                BUTTON-Ok:SENSITIVE = TRUE.
            END.
           
        END.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-EmailId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-EmailId wWin
ON VALUE-CHANGED OF FILL-IN-EmailId IN FRAME fMain /* Email Address */
DO:
        DEFINE VARIABLE oValid     AS Validation NO-UNDO.
        DEFINE VARIABLE lEmail AS LOGICAL    NO-UNDO.
        oValid = NEW Validation().
        lEmail = oValid:emailValidation(INPUT FILL-IN-EmailId:SCREEN-VALUE).
        IF ( lEmail = TRUE )THEN
        DO:
            FILL-IN-EmailId:FGCOLOR = 9.
            FILL-IN-EmailId:SIDE-LABEL-HANDLE:FGCOLOR = 0.
          //  BUTTON-Search:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            FILL-IN-EmailId:FGCOLOR = 12.
            FILL-IN-EmailId:SIDE-LABEL-HANDLE:FGCOLOR = 12.
          //  BUTTON-Search:SENSITIVE = FALSE.
        END.
        RUN validation.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FirstName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FirstName wWin
ON VALUE-CHANGED OF FILL-IN-FirstName IN FRAME fMain /* First Name */
DO:
        DEFINE VARIABLE oValid     AS Validation NO-UNDO.
        DEFINE VARIABLE lFirstName AS LOGICAL    NO-UNDO.
        oValid = NEW Validation().
        lFirstName = oValid:nameValidation(INPUT FILL-IN-FirstName:SCREEN-VALUE).
        IF ( lFirstName = TRUE )THEN
        DO:
            FILL-IN-FirstName:FGCOLOR = 9.
            FILL-IN-FirstName:SIDE-LABEL-HANDLE:FGCOLOR = 0.
          //  BUTTON-Search:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            FILL-IN-FirstName:FGCOLOR = 12.
            FILL-IN-FirstName:SIDE-LABEL-HANDLE:FGCOLOR = 12.
          //  BUTTON-Search:SENSITIVE = FALSE.
        END.
        RUN validation.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LastName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LastName wWin
ON VALUE-CHANGED OF FILL-IN-LastName IN FRAME fMain /* Last Name */
DO:
        DEFINE VARIABLE oValid     AS Validation NO-UNDO.
        DEFINE VARIABLE lLastName AS LOGICAL    NO-UNDO.
        oValid = NEW Validation().
        lLastName = oValid:nameValidation(INPUT FILL-IN-LastName:SCREEN-VALUE).
        IF ( lLastName = TRUE )THEN
        DO:
            FILL-IN-LastName:FGCOLOR = 9.
            FILL-IN-LastName:SIDE-LABEL-HANDLE:FGCOLOR = 0.
          //  BUTTON-Search:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            FILL-IN-LastName:FGCOLOR = 12.
            FILL-IN-LastName:SIDE-LABEL-HANDLE:FGCOLOR = 12.
          //  BUTTON-Search:SENSITIVE = FALSE.
        END.
        RUN validation.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MobileNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MobileNo wWin
ON VALUE-CHANGED OF FILL-IN-MobileNo IN FRAME fMain /* Mobile # */
DO:
    DEFINE VARIABLE oValid     AS Validation NO-UNDO.
        DEFINE VARIABLE lMobileNo AS LOGICAL    NO-UNDO.
        oValid = NEW Validation().
        lMobileNo = oValid:phoneNumValidation(INPUT FILL-IN-MobileNo:SCREEN-VALUE).
        IF ( lMobileNo = TRUE )THEN
        DO:
            FILL-IN-MobileNo:FGCOLOR = 9.
            FILL-IN-MobileNo:SIDE-LABEL-HANDLE:FGCOLOR = 0.
          //  BUTTON-Search:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            FILL-IN-MobileNo:FGCOLOR = 12.
            FILL-IN-MobileNo:SIDE-LABEL-HANDLE:FGCOLOR = 12.
          //  BUTTON-Search:SENSITIVE = FALSE.
        END.
        RUN validation.  
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
  DISPLAY FILL-IN-FirstName FILL-IN-LastName FILL-IN-MobileNo FILL-IN-EmailId 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-5 RECT-9 RECT-10 RECT-11 RECT-12 FILL-IN-FirstName 
         FILL-IN-LastName FILL-IN-MobileNo FILL-IN-EmailId 
         BROWSE-FilteredCustomerDetails BUTTON-Cancel 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validation wWin 
PROCEDURE validation :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFisrtName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLastName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEmail     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMobileNo  AS CHARACTER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}: 
        ASSIGN
            cFisrtName = FILL-IN-FirstName:SCREEN-VALUE
            cLastName  = FILL-IN-LastName:SCREEN-VALUE
            cEmail     = FILL-IN-EmailId:SCREEN-VALUE
            cMobileNo  = FILL-IN-MobileNo:SCREEN-VALUE.
    
        IF (FILL-IN-FirstName:FGCOLOR <> 12 AND FILL-IN-LastName:FGCOLOR <> 12 AND FILL-IN-EmailId:FGCOLOR <> 12 AND FILL-IN-MobileNo:FGCOLOR <> 12) 
            AND (cFisrtName <> "" OR cLastName <> "" OR cEmail <> "" OR cMobileNo <> "") THEN
        DO:
        
            BUTTON-Search:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            BUTTON-Search:SENSITIVE = FALSE.
        END.
    END.

    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

