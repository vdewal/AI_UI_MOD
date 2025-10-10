
/*------------------------------------------------------------------------
    File        : EmiIdSeqTrg.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Kishore.M
    Created     : Wed Feb 09 19:03:08 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
TRIGGER PROCEDURE FOR CREATE OF LoanAccountDetail.
ASSIGN LoanAccountDetail.EMIID = NEXT-VALUE (EMIDSeq).