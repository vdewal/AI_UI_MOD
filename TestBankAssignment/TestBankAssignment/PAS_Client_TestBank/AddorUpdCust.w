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

DEFINE TEMP-TABLE ttCustAccntDetails 
    FIELD AccntType     AS CHARACTER
    FIELD AccntSubType  AS CHARACTER
    FIELD IFSCCode      AS CHARACTER
    FIELD RateofInt     AS DECIMAL
    FIELD LoanDuration  AS INTEGER
    FIELD TotalLoanAmt  AS INTEGER
    FIELD TransferLimit AS INTEGER.
    
DEFINE TEMP-TABLE ttEmpty LIKE ttCustomer.
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER TABLE FOR ttCustomer.
DEFINE INPUT PARAMETER iCustId AS INTEGER NO-UNDO.

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

  Created: 02/09/22 - 11:22 am

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
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-7 RECT-11 FILL-IN-FirstName ~
FILL-IN-LastName FILL-IN-DOB COMBO-BOX-MaritalStatus FILL-IN-Address1 ~
FILL-IN-Address2 COMBO-BOX-Country COMBO-BOX-State COMBO-BOX-City ~
FILL-IN-PostalCode BUTTON-Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-FirstName FILL-IN-LastName ~
FILL-IN-DOB COMBO-BOX-MaritalStatus FILL-IN-Address1 FILL-IN-Address2 ~
COMBO-BOX-Country COMBO-BOX-State COMBO-BOX-City FILL-IN-PostalCode 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-ADD 
    LABEL "ADD" 
    SIZE 20 BY 1.25.

DEFINE BUTTON BUTTON-Cancel AUTO-END-KEY 
    LABEL "Cancel" 
    SIZE 20 BY 1.25.

DEFINE VARIABLE COMBO-BOX-City          AS CHARACTER 
    LABEL "City" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    DROP-DOWN
    SIZE 24 BY 1
    FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-Country       AS CHARACTER 
    LABEL "Country" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    DROP-DOWN
    SIZE 24 BY 1
    FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-MaritalStatus AS CHARACTER FORMAT "X(256)":U 
    LABEL "Marital Status" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "Single","Married","Divorced","Single Parent" 
    DROP-DOWN-LIST
    SIZE 24 BY 1
    FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-State         AS CHARACTER 
    LABEL "State" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    DROP-DOWN
    SIZE 24 BY 1
    FGCOLOR 9 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-Address1        AS CHARACTER FORMAT "X(256)":U 
    LABEL "Address Line 1" 
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE FILL-IN-Address2        AS CHARACTER FORMAT "X(256)":U 
    LABEL "Address Line 2" 
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE FILL-IN-DOB             AS DATE      FORMAT "99/99/99":U 
    LABEL "Date of Birth" 
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE FILL-IN-FirstName       AS CHARACTER FORMAT "X(256)":U 
    LABEL "First Name" 
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE FILL-IN-LastName        AS CHARACTER FORMAT "X(256)":U 
    LABEL "Last Name" 
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    FGCOLOR 9 NO-UNDO.

DEFINE VARIABLE FILL-IN-PostalCode      AS INTEGER   FORMAT "->,>>>,>>9":U INITIAL 0 
    LABEL "Postal Code" 
    VIEW-AS FILL-IN 
    SIZE 24 BY 1
    FGCOLOR 9 NO-UNDO.

DEFINE RECTANGLE RECT-11
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 82 BY 9.

DEFINE RECTANGLE RECT-6
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 80 BY 2.75.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 80 BY 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME gDialog
    FILL-IN-FirstName AT ROW 3.75 COL 16 COLON-ALIGNED WIDGET-ID 4
    FILL-IN-LastName AT ROW 3.75 COL 56 COLON-ALIGNED WIDGET-ID 6
    FILL-IN-DOB AT ROW 5 COL 16 COLON-ALIGNED WIDGET-ID 10
    COMBO-BOX-MaritalStatus AT ROW 5 COL 56 COLON-ALIGNED WIDGET-ID 12
    FILL-IN-Address1 AT ROW 7.5 COL 16 COLON-ALIGNED WIDGET-ID 16
    FILL-IN-Address2 AT ROW 7.5 COL 56 COLON-ALIGNED WIDGET-ID 18
    COMBO-BOX-Country AT ROW 8.75 COL 16 COLON-ALIGNED WIDGET-ID 42
    COMBO-BOX-State AT ROW 8.75 COL 56 COLON-ALIGNED WIDGET-ID 44
    COMBO-BOX-City AT ROW 10 COL 16 COLON-ALIGNED WIDGET-ID 46
    FILL-IN-PostalCode AT ROW 10 COL 56 COLON-ALIGNED WIDGET-ID 26
    BUTTON-ADD AT ROW 12 COL 19 WIDGET-ID 30
    BUTTON-Cancel AT ROW 12 COL 53 WIDGET-ID 32
    "Address Details :" VIEW-AS TEXT
    SIZE 25 BY .63 AT ROW 6.5 COL 3 WIDGET-ID 36
    FONT 2
    "Personal Details :" VIEW-AS TEXT
    SIZE 23 BY .63 AT ROW 2.75 COL 3 WIDGET-ID 38
    FONT 2
    "CustomerDetails" VIEW-AS TEXT
    SIZE 18 BY 1.25 AT ROW 1.25 COL 32 WIDGET-ID 34
    FONT 5
    RECT-6 AT ROW 3.5 COL 3 WIDGET-ID 2
    RECT-7 AT ROW 7.25 COL 3 WIDGET-ID 14
    RECT-11 AT ROW 2.5 COL 2 WIDGET-ID 40
    SPACE(1.74) SKIP(2.05)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "Add/Update Customer" WIDGET-ID 100.


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
    FRAME gDialog:SCROLLABLE = FALSE
    FRAME gDialog:HIDDEN     = TRUE.

/* SETTINGS FOR BUTTON BUTTON-ADD IN FRAME gDialog
   NO-ENABLE                                                            */
ASSIGN 
    FILL-IN-PostalCode:READ-ONLY IN FRAME gDialog = TRUE.

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
ON WINDOW-CLOSE OF FRAME gDialog /* Add/Update Customer */
    DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-ADD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-ADD gDialog
ON CHOOSE OF BUTTON-ADD IN FRAME gDialog /* ADD */
    DO:
        EMPTY TEMP-TABLE ttCustomer.
        
        DEFINE VARIABLE lConnectionStatus    AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcResult             AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection          AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson           AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oInputParametersJson AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oTTJson              AS JsonArray        NO-UNDO.
        DEFINE VARIABLE oUtility             AS ClientUtility    NO-UNDO.
        DEFINE VARIABLE lcInput              AS LONGCHAR         NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        
        
        IF {&SELF-NAME}:LABEL = "ADD" THEN
        DO:
            DEFINE VARIABLE lcAddedStatus AS LONGCHAR NO-UNDO.
            
           
            CREATE ttCustomer.
            ASSIGN
                ttCustomer.FirstName     = FILL-IN-FirstName:SCREEN-VALUE     
                ttCustomer.LastName      = FILL-IN-LastName:SCREEN-VALUE       
                ttCustomer.Address1      = FILL-IN-Address1:SCREEN-VALUE    
                ttCustomer.Address2      = FILL-IN-Address2:SCREEN-VALUE  
                ttCustomer.DateOfBirth   = DATE(FILL-IN-DOB:SCREEN-VALUE)
                ttCustomer.City          = COMBO-BOX-City:SCREEN-VALUE           
                ttCustomer.State         = COMBO-BOX-State:SCREEN-VALUE      
                ttCustomer.Country       = COMBO-BOX-Country:SCREEN-VALUE        
                ttCustomer.ZIPCode       = FILL-IN-PostalCode:SCREEN-VALUE 
                ttCustomer.MaritalStatus = COMBO-BOX-MaritalStatus:SCREEN-VALUE.
                
            lConnectionStatus = oConnection:getConnect().
            oUtility:getInputParameterStructure(INPUT "AddCustomerDetails",INPUT "", INPUT 0,INPUT TEMP-TABLE ttCustomer:HANDLE,OUTPUT oInputJson).
            MESSAGE STRING(oInputJson:GetJsonText())
                VIEW-AS ALERT-BOX.
                
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcResult).
            lConnectionStatus = oConnection:getDisconnect().
            MESSAGE STRING(lcResult)
                VIEW-AS ALERT-BOX.
                
            // APPLY "CHOOSE" TO BUTTON-Cancel.
            BUTTON-ADD:AUTO-END-KEY = TRUE.
             
        END.
        
        IF {&SELF-NAME}:LABEL = "UPDATE" THEN
        DO:
            DEFINE VARIABLE lcUpdatedStatus AS LONGCHAR  NO-UNDO.
            DEFINE VARIABLE cStatus         AS CHARACTER NO-UNDO.
            
            CREATE ttCustomer.
            ASSIGN
                ttCustomer.CustId        = iCustId
                ttCustomer.FirstName     = FILL-IN-FirstName:SCREEN-VALUE     
                ttCustomer.LastName      = FILL-IN-LastName:SCREEN-VALUE       
                ttCustomer.Address1      = FILL-IN-Address1:SCREEN-VALUE    
                ttCustomer.Address2      = FILL-IN-Address2:SCREEN-VALUE  
                ttCustomer.DateOfBirth   = DATE( FILL-IN-DOB:SCREEN-VALUE )
                ttCustomer.City          = COMBO-BOX-City:SCREEN-VALUE           
                ttCustomer.State         = COMBO-BOX-State:SCREEN-VALUE      
                ttCustomer.Country       = COMBO-BOX-Country:SCREEN-VALUE        
                ttCustomer.ZIPCode       = FILL-IN-PostalCode:SCREEN-VALUE 
                ttCustomer.MaritalStatus = COMBO-BOX-MaritalStatus:SCREEN-VALUE.
     
            lConnectionStatus = oConnection:getConnect().
            oUtility:getInputParameterStructure(INPUT "UpdateCustomerDetails", INPUT "", INPUT 0, INPUT TEMP-TABLE ttCustomer:HANDLE,OUTPUT oInputJson).
            /*  MESSAGE STRING(oInputJson:GetJsonText())
                  VIEW-AS ALERT-BOX. */
            oInputJson:WRITE(lcInput,TRUE,"UTF-8").
            MESSAGE STRING(lcInput)
                VIEW-AS ALERT-BOX.
         //   oInputJson:WriteFile("C:\Users\kishore.m\Documents\Progress\InputJsonTemp.txt").
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcResult).
            lConnectionStatus = oConnection:getDisconnect().  
            MESSAGE STRING(lcResult)
                VIEW-AS ALERT-BOX.
                
            // APPLY "CHOOSE" TO BUTTON-Cancel.
            BUTTON-ADD:AUTO-END-KEY = TRUE.
            
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-City
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-City gDialog
ON VALUE-CHANGED OF COMBO-BOX-City IN FRAME gDialog /* City */
    DO:
        DEFINE VARIABLE lConnectionStatus    AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcResult             AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection          AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson           AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oInputParametersJson AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oTTJson              AS JsonArray        NO-UNDO.
        DEFINE VARIABLE oUtility             AS ClientUtility    NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        
        lConnectionStatus = oConnection:getConnect().
        oUtility:getInputParameterStructure(INPUT "getPostalCode",INPUT COMBO-BOX-City:SCREEN-VALUE, INPUT 0,INPUT TEMP-TABLE ttEmpty:HANDLE,OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX.
        RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcResult).
        lConnectionStatus = oConnection:getDisconnect().
        MESSAGE STRING(lcResult)
            VIEW-AS ALERT-BOX.
        FILL-IN-PostalCode:SCREEN-VALUE = lcResult.
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Country
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Country gDialog
ON VALUE-CHANGED OF COMBO-BOX-Country IN FRAME gDialog /* Country */
    DO:
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcResult          AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        
        lConnectionStatus = oConnection:getConnect().
        oUtility:getInputParameterStructure(INPUT "getStateCode",INPUT COMBO-BOX-Country:SCREEN-VALUE, INPUT 0,INPUT TEMP-TABLE ttEmpty:HANDLE,OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX.
        RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcResult).
        lConnectionStatus = oConnection:getDisconnect().
        MESSAGE STRING(lcResult)
            VIEW-AS ALERT-BOX.
        COMBO-BOX-State:LIST-ITEMS = STRING(lcResult).
        COMBO-BOX-City:SCREEN-VALUE = "".
        FILL-IN-PostalCode:SCREEN-VALUE = "".
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-State
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-State gDialog
ON VALUE-CHANGED OF COMBO-BOX-State IN FRAME gDialog /* State */
    DO:
        DEFINE VARIABLE lConnectionStatus AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcResult          AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection       AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson        AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oUtility          AS ClientUtility    NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().
        lConnectionStatus = oConnection:getConnect().
        oUtility:getInputParameterStructure(INPUT "getCityCode",INPUT COMBO-BOX-State:SCREEN-VALUE, INPUT 0,INPUT TEMP-TABLE ttEmpty:HANDLE,OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX.
        IF VALID-HANDLE (oConnection:hAppServerHandle) THEN   
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcResult).
        ELSE
            MESSAGE "hi"
                VIEW-AS ALERT-BOX.
        lConnectionStatus = oConnection:getDisconnect().
        MESSAGE STRING(lcResult)
            VIEW-AS ALERT-BOX.
        COMBO-BOX-City:LIST-ITEMS = STRING(lcResult).
        FILL-IN-PostalCode:SCREEN-VALUE = "".
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FirstName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FirstName gDialog
ON VALUE-CHANGED OF FILL-IN-FirstName IN FRAME gDialog /* First Name */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LastName gDialog
ON VALUE-CHANGED OF FILL-IN-LastName IN FRAME gDialog /* Last Name */
    DO:
        DEFINE VARIABLE oValid    AS Validation NO-UNDO.
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
    DISPLAY FILL-IN-FirstName FILL-IN-LastName FILL-IN-DOB COMBO-BOX-MaritalStatus 
        FILL-IN-Address1 FILL-IN-Address2 COMBO-BOX-Country COMBO-BOX-State 
        COMBO-BOX-City FILL-IN-PostalCode 
        WITH FRAME gDialog.
    ENABLE RECT-6 RECT-7 RECT-11 FILL-IN-FirstName FILL-IN-LastName FILL-IN-DOB 
        COMBO-BOX-MaritalStatus FILL-IN-Address1 FILL-IN-Address2 
        COMBO-BOX-Country COMBO-BOX-State COMBO-BOX-City FILL-IN-PostalCode 
        BUTTON-Cancel 
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
   
    
    DO WITH FRAME {&frame-name}:
    
        DEFINE VARIABLE lConnectionStatus    AS LOGICAL          NO-UNDO.
        DEFINE VARIABLE lcResult             AS LONGCHAR         NO-UNDO.
        DEFINE VARIABLE oConnection          AS ServerConnection NO-UNDO.
        DEFINE VARIABLE oInputJson           AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oInputParametersJson AS JsonObject       NO-UNDO.
        DEFINE VARIABLE oTTJson              AS JsonArray        NO-UNDO.
        DEFINE VARIABLE oUtility             AS ClientUtility    NO-UNDO.
        
        oConnection = NEW ServerConnection().
        oUtility = NEW ClientUtility().   
        
        lConnectionStatus = oConnection:getConnect().
        oUtility:getInputParameterStructure(INPUT "getCountryCodes",INPUT "", INPUT 0,INPUT TEMP-TABLE ttEmpty:HANDLE,OUTPUT oInputJson).
        MESSAGE STRING(oInputJson:GetJsonText())
            VIEW-AS ALERT-BOX.
        RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcResult).
        lConnectionStatus = oConnection:getDisconnect().
        MESSAGE STRING(lcResult)
            VIEW-AS ALERT-BOX.  
        COMBO-BOX-Country:LIST-ITEMS = STRING(lcResult).
        FIND FIRST ttCustomer. 
        IF LOOKUP (ttCustomer.Country, COMBO-BOX-Country:LIST-ITEMS) > 0 THEN
            COMBO-BOX-Country:SCREEN-VALUE = ttCustomer.Country.
        
        FIND FIRST ttCustomer NO-LOCK.
        IF AVAILABLE ttCustomer THEN
        DO:
            BUTTON-ADD:SENSITIVE = TRUE.
            ASSIGN
                BUTTON-ADD:LABEL                     = "UPDATE"
                FILL-IN-FirstName:SCREEN-VALUE       = ttCustomer.FirstName
                FILL-IN-LastName:SCREEN-VALUE        = ttCustomer.LastName
                FILL-IN-Address1:SCREEN-VALUE        = ttCustomer.Address1
                FILL-IN-Address2:SCREEN-VALUE        = ttCustomer.Address2
                FILL-IN-DOB:SCREEN-VALUE             = STRING(ttCustomer.DateOfBirth)
                COMBO-BOX-MaritalStatus:SCREEN-VALUE = ttCustomer.MaritalStatus.
                
            lConnectionStatus = oConnection:getConnect().
            oUtility:getInputParameterStructure(INPUT "getStateCode",INPUT COMBO-BOX-Country:SCREEN-VALUE, INPUT 0,INPUT TEMP-TABLE ttEmpty:HANDLE,OUTPUT oInputJson).
            MESSAGE STRING(oInputJson:GetJsonText())
                VIEW-AS ALERT-BOX.
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcResult).
            lConnectionStatus = oConnection:getDisconnect().
            MESSAGE STRING(lcResult)
                VIEW-AS ALERT-BOX.  
            COMBO-BOX-State:LIST-ITEMS = STRING(lcResult).
            FIND FIRST ttCustomer. 
            IF LOOKUP (ttCustomer.State, COMBO-BOX-State:LIST-ITEMS) > 0 THEN
                COMBO-BOX-State:SCREEN-VALUE = ttCustomer.State.
           
            lConnectionStatus = oConnection:getConnect().
            oUtility:getInputParameterStructure(INPUT "getCityCode",INPUT COMBO-BOX-State:SCREEN-VALUE, INPUT 0,INPUT TEMP-TABLE ttEmpty:HANDLE,OUTPUT oInputJson).
            MESSAGE STRING(oInputJson:GetJsonText())
                VIEW-AS ALERT-BOX.
            RUN TestBankController.p ON oConnection:hAppServerHandle( INPUT oInputJson ,OUTPUT lcResult).
            lConnectionStatus = oConnection:getDisconnect().
            MESSAGE STRING(lcResult)
                VIEW-AS ALERT-BOX.
            COMBO-BOX-City:LIST-ITEMS = STRING(lcResult).
            FIND FIRST ttCustomer.
            IF LOOKUP (ttCustomer.City, COMBO-BOX-City:LIST-ITEMS) > 0 THEN
                COMBO-BOX-City:SCREEN-VALUE = ttCustomer.City.
                                                                                                                              
                        
            FILL-IN-PostalCode:SCREEN-VALUE = ttCustomer.ZIPCode.
                
                 
        END.
           
        
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
    DO WITH FRAME {&FRAME-NAME}:    
        IF (FILL-IN-FirstName:SCREEN-VALUE <> "" AND FILL-IN-FirstName:FGCOLOR <> 12 AND FILL-IN-LastName:FGCOLOR <> 12)THEN
        DO:
        
            BUTTON-ADD:SENSITIVE = TRUE.
        END.
        ELSE
        DO:
            BUTTON-ADD:SENSITIVE = FALSE.
        END.
    END.
    CATCH e AS Progress.Lang.Error:

    END CATCH.

    FINALLY:

    END FINALLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

