&SYSPARM TITLE 'VM COMPARATOR SERIES (1006-CMS) DELTA COMPARATOR       X
               (C) 1988 W. A. HUMPHRIES INCORPORATED'
         SPACE 3
***********************************************************************
***                                                                 ***
***             (C) 1988 W. A. HUMPHRIES INCORPORATED               ***
***                                                                 ***
***      THIS PROGRAM IS AN UNPUBLISHED TRADE SECRET WORK FULLY     ***
***      PROTECTED BY THE UNITED STATES COPYRIGHT LAWS AND IS       ***
***      A TRADE SECRET BELONGING TO THE COPYRIGHT OWNER.           ***
***                                                                 ***
***                          ** WARNING **                          ***
***                                                                 ***
***      THIS SOFTWARE IS PROTECTED BY U.S. COPYRIGHT LAW.          ***
***      UNAUTHORIZED REPRODUCTION AND/OR SALES MAY RESULT IN       ***
***      IMPRISONMENT OF UP TO ONE YEAR AND FINES OF UP TO $10,000  ***
***      (17 U.S.C. SECTION 506).  COPYRIGHT INFRINGERS MAY ALSO    ***
***      BE SUBJECT TO CIVIL LIABILITY.                             ***
***                                                                 ***
***********************************************************************
         EJECT
         COPY  WHMABC10
         EJECT
&WHSECT  CSECT
         WHMEQU10
         EJECT
*---------------------------------------------------------------------*
*        REGISTER USAGE  **  CONTROL VALUES                           *
*---------------------------------------------------------------------*
*
RBASE    EQU   R3                  BASE
RW2      EQU   R4                  WORKING STORAGE (2)
RBASE2   EQU   R5                  BASE (2)
RBASE4   EQU   R6                  BASE (4)
RSSP     EQU   R7                  SAVE STACK PTR
RSSL     EQU   R8                  SAVE STACK LENGTH
RSSU     EQU   R9                  SAVE STACK LIMIT
RWS      EQU   R10                 WORKING STORAGE
RNODE    EQU   R11                 LCS TREE NODE PTR
RBASE3   EQU   R12                 BASE (3)
*
         USING &WHSECT,RBASE,RBASE2,RBASE3,RBASE4
         USING WSSECT,RWS,RW2
         USING NODESECT,RNODE
*
*        RETURN CODES:
*        ------------
*
XNOCOMD  EQU   4                   FILES HAVE NO RECORDS IN COMMON
XIDEN    EQU   8                   FILES IDENTICAL - NO U/D OUTPUT
XFILERR  EQU   12                  RECORD COUNT ERRORS IN I/P FILES
XOPENERR EQU   16                  ERROR OPENING COMPARAND FILE(S)
XSTORERR EQU   20                  OUT OF STORAGE
XPARMERR EQU   24                  PARAMETER ERROR
XUNAUTH  EQU   28                  EXECUTION UNAUTHORIZED
         EJECT
*---------------------------------------------------------------------*
*        LCS STRING NODE MAP                                          *
*---------------------------------------------------------------------*
*
*
NODESECT DSECT
NODENEXT DS    AL3                 LATERAL PTR
NODEBACK DS    AL3                 BACK PTR
NODESUBA DS    HL2                 A-POINTER
NODESUBB DS    HL2                 B-POINTER
NODESECL EQU   *-NODESECT
*
*
*
*              *---------------------*
*              *  PTR TO NEXT (A,B)  |
*              *  WITH SAME LCS LEN  |
*              *---------------------*
*              *  PTR TO LAST (A,B)  |
*              *  WITH LCS LEN - 1   |
*              *----------*----------*
*              *  CO-OR A |  CO-OR B |
*              *  (INDEX) |  (INDEX) |
*              *----------*----------*
*
*
*
*---------------------------------------------------------------------*
         EJECT
*---------------------------------------------------------------------*
*        LCS NODE INDEX MAP                                           *
*                                                                     *
*        EACH ENTRY IN THIS ARRAY POINTS TO A NODE WHOSE              *
*        COORDINATES MAP THE N-TH SYMBOLS OF A COMMON SUBSEQUENCE,    *
*        WHERE 'N' IS THE INDEX (+1) INTO THE ARRAY.                  *
*---------------------------------------------------------------------*
*
*
LCSSECT  DSECT
LCSPTR   DS    AL3                 ADDR(1ST NODE ON LATERAL CHAIN)
LCSLAST  DS    AL3                 ADDR(LAST NODE ON LATERAL CHAIN)
LCSMARK  DS    HL2                 HIGHEST 'I', A(I) IN SET
LCSSECL  EQU   *-LCSSECT
*
*
*
*        *------------------------------*---------------*
*        * ==> PAIR / SUBSEQUENCE LEN 0 *  HIGHEST A(I) *
*        *------------------------------*---------------*
*        * ==> PAIR / SUBSEQUENCE LEN 1 *  HIGHEST A(I) *
*        *------------------------------*---------------*
*        * ==> PAIR / SUBSEQUENCE LEN 2 *  HIGHEST A(I) *
*        *------------------------------*---------------*
*
*
*                       .
*                       .
*                       .
*
*        *------------------------------*---------------*
*        * ==> PAIR / SUBSEQUENCE LEN N *  HIGHEST A(I) *
*        *------------------------------*---------------*
*
*
*
*---------------------------------------------------------------------*
         EJECT
*---------------------------------------------------------------------*
*        PAIR STACK MAP                                               *
*                                                                     *
*        THIS STACK IS USED TO BUILD FRONT-TO-BACK PAIRS              *
*        OF RECORDS FROM (A,B) FROM A BACK-TO-FRONT TRAVERSAL         *
*        OF THE NODE TREE.                                            *
*---------------------------------------------------------------------*
*
*
PAIRSECT DSECT
PAIRA    DS    H
PAIRB    DS    H
PAIRSECL EQU   *-PAIRSECT
*
*
*
*
*        *-----------*-----------*
*        | LCS 1 (A) | LCS 1 (B) |
*        *-----------*-----------*
*        | LCS 2 (A) | LCS 2 (B) |
*        *-----------*-----------*
*
*              .           .
*              .           .
*              .           .
*
*        *-----------*-----------*
*        | LCS N (A) | LCS N (B) |
*        *-----------*-----------*
*
*---------------------------------------------------------------------*
         EJECT
*---------------------------------------------------------------------*
*        SEQUENCE NUMBER FIELD (PRINT LINE) MAP                       *
*                                                                     *
*        SEQUENCE NUMBER LOCATIONS DEPENDS ON AMOUNT OF DATA          *
*        DISPLAYED, WHICH IN TURN DEPENDS ON LARGEST FILE LRECL       *
*---------------------------------------------------------------------*
*
*
SEQSECT  DSECT
SEQ1     DS    CL5,2C
SEQ2     DS    CL5,2C
SEQSECL  EQU   *-SEQSECT
         EJECT
*---------------------------------------------------------------------*
*        COMPARAND FILE CONTROL BLOCK                                 *
*---------------------------------------------------------------------*
*
FILSECT  DSECT
FILDCBA  DS    A                   DCB ADDRESS
FILCNT   DS    F                   CURRENT RECORD COUNT
FILSEQ   DS    F                   SEQUENCE # OF RCD => BY FILPTR
FILFIRST DS    A                   FIRST RECORD PTR
FILFIRS2 DS    A                   FIRST RECORD PTR FOR TYPE 2 ALG
FILLAST  DS    A                   LAST RECORD PTR
FILLAS2  DS    A                   LAST RECORD PTR FOR TYPE 2 ALG
FILCURRL DS    F                   CURRENT RECORD LENGTH
FILCURRA DS    A                   CURRENT RECORD ADDRESS
FILLRECL DS    F                   MAX RECORD LRECL
FILPTR   DS    A                   CURRENT RECORD PTR
FILRDLEN DS    F                   LENGTH OF RECORD DESCRIPTOR FIELD
FILSTORR DS    F                   TOTAL FILE STORAGE REQUIRED
FILFDSP  DS    F                   COMPARE FIELD DISPLACEMENT
FILFLEN  DS    F                   COMPARE FIELD LENGTH
FILDDNAM DS    CL8                 DDNAME S/A
FILDESCR DS    CL8                 LITERAL DESCRIBING FILE
FILDSN   DS    CL(L'JFCBDSNM)      DATASETNAME
         DS    0A
FILSECL  EQU   *-FILSECT           C/B LENGTH
         EJECT
         DSECT
         WHMTIM10
         EJECT
         DSECT
DLR      WHMDLR10
         EJECT
*---------------------------------------------------------------------*
*        WORKING STORAGE                                              *
*---------------------------------------------------------------------*
*
WSSECT   DSECT
*
WTPCB    WHMWTP11
         EJECT
RFMCB    WHMRFM11
         EJECT
ECXCB    WHMECX11
         EJECT
PLIST    WHMPPL10 (ID,OPNS,LLIM,STOR,BOXM,BBGN,BLEN,RBGN,RLEN,PAD,STR)
         EJECT
DDFCB    WHMDDF11
*
DW       DS    D                   W/A
GETMIN   DS    F                   GETMAIN MINIMUM LENGTH
GETMAX   DS    F                   GETMAIN MAXIMUM LENGTH
GETALLAD DS    A                   GETMAIN'D ADDRESS
GETALLLN DS    F                   GETMAIN'D LENGTH
*
GETMAINL GETMAIN VC,LA=0,MF=L,A=0
*
SYSUT1   DS    XL(L'$DCBQSI)
SYSUT2   DS    XL(L'$DCBQSI)
SYSSTAMP DS    XL(L'$DCBQSI)
SYSUPD DS    XL(L'$DCBQSO)
SYSLBN   DS    XL(L'$DCBQSO)
SYSLOG   DS    XL(L'$DCBQSO)
SYSHST   DS    XL(L'$DCBQSO)
*
SS       WHCSBL10 CAPACITY=7,SSP=RSSP,SSL=RSSL,SSX=ABEND4,SSU=RSSU
         EJECT
*---------------------------------------------------------------------*
*        WORKING STORAGE (CONTINUED)                                  *
*                                                                     *
*        STATISTICS RECORD                                            *
*---------------------------------------------------------------------*
*
STATREC  DS    0A
STATDATE DS    F                   BINARY DAYS-TO-DATE
STATTIME DS    F                   BINARY TIME (1/100 SEC)
STATBDSN DS    CL44                BASE FILE DSN
STATRDSN DS    CL44                REVISION FILE DSN
STATBRCD DS    F                   BASE FILE RECORDS
STATRRCD DS    F                   REVISION FILE RECORDS
STATLCSL DS    F                   LCS LENGTH
STATLCSN DS    F                   NUMBER OF SEQUENCES HAVING LCS
STATDBLK DS    F                   DELETIONS:  BLOCKS OF
STATDRCD DS    F                               RECORDS
STATIBLK DS    F                   INSERTIONS: BLOCKS OF
STATIRCD DS    F                               RECORDS
STATRECL EQU   *-STATREC
         EJECT
BSCCB    WHMBSC10
         EJECT
DATCB    WHMDAT11
         EJECT
*---------------------------------------------------------------------*
*        WORKING STORAGE                                              *
*---------------------------------------------------------------------*
*
SAVE     DS    18F                 S/A
SUBSAVE  DS    18F                 S/A FOR SUBROUTINE
REGCNT   DS    F                   ABEND W/A
AREG     DS    XL(16*4)            ABEND REG'S
COMPALGA DS    A                   COMPARISON ALG ADDRESS
CURHLINS DS    F                   CURRENT # HEADER LINES
RCDPRBLK DS    F                   W/A
INS1     DS    F                   INSERT SEGMENT - 1ST RECORD
DEL1     DS    F                   DELETE SEGMENT - 1ST RECORD
DEL2     DS    F                   .............  - LAST RECORD
BOXMARGN DS    F                   BOX SIDES MARGIN (O= NO BOX)
LARLRECL DS    A                   LARGEST POSS RCD / PER DCB INFO
PRECPTR  DS    A                   PRINT RECORD PTR
PSEQPTR  DS    A                   SEQUENCE NUMBERS PTR
LBOXPTR  DS    A                   LEFT COL BOX PTR
RBOXPTR  DS    A                   RIGHT ..........
LCSLEN   DS    F                   CURRENT LCS LENGTH
GENLLCS  DS    F                   LENGTH OF LCS PROCESSED BY GENLIST
NODSTORA DS    A                   NODE STORAGE ALLOCATED
DIAGSAVE DS    16F                 (DEBUGGING)
TYP2SAVE DS    5A                  W/A
LCSARRAD DS    A                   START OF LCS INDEX ARRAY
LCSL1PTR DS    A                   ADDRESS OF 2ND LCS ENTRY (LEN=1)
LCSLEVEL DS    A                   LAST ENTRY USED IN ARRAY
SMALLER  DS    A                   PTR TO SMALLER FILE FILCB
LARGER   DS    A                   PTR TO LARGER  FILE FILCB
LARPRINT DS    A                   LARGEST RECORD WE CAN PRINT
PAIRSAD  DS    A                   PAIRS STACK ADDRESS
PAIRSPTR DS    A                   CURRENT PAIRS ENTRY BEING PROC'D
PAIRSBND DS    F                   PAIRS ARRAY UPPER BOUND
PAIRSLN  DS    F                   PAIRS STACK LENGTH
EXITWORD DS    A                   OPEN EXIT WORD
CFIL1    DS    XL(FILSECL)         COMPARAND FILE C/B'S
CFIL2    DS    XL(FILSECL)         .
ALSTORA  DS    A                   ALGORITHM STORAGE ADDRESS
ALSTORRQ DS    A                   ----------------- REQUESTED
ALSTORL  DS    F                   ----------------- LENGTH
ALSTORLK DS    F                   ----------------- LENGTH IN K
ALSTORU  DS    F                   ----------------- USED
PARMADDR DS    F                   PARM REGISTER
LIST     DS    5A                  OPEN/CLOSE LIST
LINLIM   DS    F                   LINE LIMIT
PAGCOUNT DS    F                   PAGE NUMBER
LINCNT   DS    F                   CURRENT LINE COUNT
LBNCNT   DS    F                   RECORDS OUT
UPDCNT   DS    F                   UPDATE RCDS OUT
RETURNC  DS    F                   RETURN CODE
UPDRECLN DS    F                   MIN LRECL FOR UPDATE FILE
UPDRECAD DS    A                   UPDATE RECORD PTR
         EJECT
CAUCB    WHMCAU11
         EJECT
*---------------------------------------------------------------------*
*        WORKING STORAGE                                              *
*---------------------------------------------------------------------*
*
JFCBSTOR DS    0A
         IEFJFCBN
         EJECT
*---------------------------------------------------------------------*
*        WORKING STORAGE                                              *
*                                                                     *
*        -OTHER CONTROL BLOCKS                                        *
*---------------------------------------------------------------------*
*
CAHCB    WHMCAH11
*
CPUSTOR  WHMCPU10 MF=STORE,CAP=8
         EJECT
*---------------------------------------------------------------------*
*        WORKING STORAGE                                              *
*                                                                     *
*        -PRINT LINE LAYOUTS                                          *
*        -UPDATE RECORD LAYOUT                                        *
*---------------------------------------------------------------------*
*
PLINE    DS    CL133               PRINT DETAIL LINE
         ORG   PLINE
PCC      DS    CL1                 CARRIAGE CONTROL
PDATA    DS    CL(L'PLINE-L'PCC)   DATA AREA
PMSG     EQU   PLINE+(H4BGN-H4)
         ORG
*
UPDREC   DS    CL80                UPDATE OUTPUT RECORD
         ORG   UPDREC
UPDDEL   DS    CL(L'UDELLIT)
         ORG   UPDREC
UPDINS   DS    CL(L'UINSLIT)
         ORG
         EJECT
*---------------------------------------------------------------------*
*        WORKING STORAGE                                              *
*                                                                     *
*        -HEADER LAYOUTS                                              *
*---------------------------------------------------------------------*
*
H1A      DS    CL(L'PLINE)
         ORG   H1A+1
H1ATEXT  DS    CL(L'PLINE-1)
         ORG   ,
*
H1B      DS    CL(L'PLINE)
         ORG   H1B+1
H1BTEXT  DS    CL(L'PLINE-1)
         ORG   ,
*
H1C      DS    CL(L'PLINE)
         ORG   H1C+1
H1CTEXT  DS    CL(L'PLINE-1)
         ORG   ,
*
H2       DS    CL(L'PLINE)
         ORG   H2+1+(L'H2-1-H2LEN)/2
H2BGN    EQU   *
HDAT     DS    CL(HSLOTLEN),4C
HTIM     DS    CL(HSLOTLEN),4C
HREP     DS    CL(HSLOTLEN),4C
HOPN     DS    CL(HSLOTLEN),4C
HPAG     DS    CL(HSLOTLEN)
H2LEN    EQU   *-H2BGN
         ORG   ,
*
H3       DS    CL(L'PLINE)
         ORG   H3+1+(L'H3-1-H3LEN)/2
H3BGN    EQU   *
HBCL     DS    CL(HSLOTLEN),4C
HBLN     DS    CL(HSLOTLEN),4C
HRCL     DS    CL(HSLOTLEN),4C
HRLN     DS    CL(HSLOTLEN),4C
HPAD     DS    CL(HSLOTLEN)
H3LEN    EQU   *-H3BGN
         ORG   ,
         EJECT
*---------------------------------------------------------------------*
*        WORKING STORAGE                                              *
*                                                                     *
*        -HEADER LAYOUTS                                              *
*---------------------------------------------------------------------*
*
H4       DS    CL(L'PLINE)
         ORG   H4+1+(L'H4-1-H4LEN)/2
H4BGN    EQU   *
H4FORML  DS    CL4
HC1LIT   DS    CL(L'C1LIT)
HC1DSN   DS    CL(L'JFCBDSNM)
H4FORMC  DS    CL4
HC2LIT   DS    CL(L'C2LIT)
HC2DSN   DS    CL(L'JFCBDSNM)
H4FORMR  DS    CL4
H4LEN    EQU   *-H4BGN
         ORG   ,
*
H6       DS    CL(L'PLINE)         DATA SUBHEADINGS
H7       DS    CL(L'PLINE)
         EJECT
*---------------------------------------------------------------------*
*        WORKING STORAGE                                              *
*---------------------------------------------------------------------*
*
PROCESS  DS    BL1                 PROCESSING INDICATORS:
PROCSTOX EQU   1                   =1, STORAGE REQ'S EXCEED AMT SPEC'D
PROCEMPD EQU   2                   =1, ONE OR BOTH D/S EMPTY
PROCBOX  EQU   4                   =1, BOX CONSTRUCTION IN EFFECT
PROCCSWP EQU   8                   =1, (A,B) MEANINGS SWITCHED FOR ALG.
PROCLERR EQU   16                  =1, FILE RECORD LENGTH ERROR
PROCNCOM EQU   32                  =1, FILES HAVE NO RECORDS IN COMMON
PROCFIDN EQU   64                  =1, FILES ARE IDENTICAL
PROCPRTD EQU   128                 =1, PRINTING DATA
*
PROCESS2 DS    BL1                 PROCESSING INDICATORS (2):
PRO2UPDL EQU   1                   =1, ENTRY PUBLISHED IN UPDATE LIST
PRO2PFTR EQU   2                   =1, PRINT FOOTERS ON NEW PAGE
PRO2STRP EQU   4                   =1, STRIP TRAILING BLANKS
*
SVDAT    DS    CL8                 DATE
SVTIM    DS    CL8                 TIME
SVBCL    DS    CL8                 BASE/REVN COL/LEN S/A'S
SVBLN    DS    CL8                 .
SVRCL    DS    CL8                 .
SVRLN    DS    CL8                 .
SVPAG    DS    CL8                 .
BSARG    DS    XL(L'LCSMARK)       W/A FOR B-SEARCH
EDITWORK DS    CL12                EDIT W/A
COMPPAD  DS    CL1                 PAD CHARACTER FOR COMPARE
STRIPCHR DS    CL1                 TRAILING CHAR FOR STRIP
AC       DS    BL1                 ABEND CODE
WORK6    DS    CL6                 W/A
*
BOXCSET  WHMBXC10                  BOX CHARACTER SET
*
DSNCB    WHMDSN11
*
AUTH     WHCDAA10 LEN=5
*
LINELIST LINEDIT MF=L,MAXSUBS=8
*
PPGCB    WHMPPG11
*
REVWORK  DS    XL256               W/A FOR REVERSING CHAR STR
TRAILTAB DS    XL256               TRT FUNC FOR SCAN THRU TRAILCHR
OPNTAB   DS    XL256               OPTIONS TABLE
OPNON    EQU   1                   =1, OPTION SPECIFIED
*
         DS    0D
WSLEN    EQU   *-WSSECT
         EJECT
*---------------------------------------------------------------------*
*        INITIALIZATION                                               *
*                                                                     *
*        -PRIME REGISTERS                                             *
*        -GET WORKING STORAGE AND CLEAR                               *
*---------------------------------------------------------------------*
*
&WHSECT  CSECT
         WHCCSR10 PSN=1006-CMS,CYR=1988
*
         LR    RBASE,R15
         LA    RBASE2,4095(,RBASE)
         LA    RBASE2,1(,RBASE2)
         LA    RBASE3,4095(,RBASE2)
         LA    RBASE3,1(,RBASE3)
         LA    RBASE4,4095(,RBASE3)
         LA    RBASE4,1(,RBASE4)
*
         LR    R2,R1
*
         L     R0,=A(WSLEN)
         GETMAIN R,LV=(0)
         LR    RWS,R1
         LA    RW2,4095(,RWS)
         LA    RW2,0001(,RW2)
*
         LR    R0,RWS
         L     R1,=A(WSLEN)
         XR    R15,R15
         MVCL  R0,R14
*
         LA    R15,SAVE
         ST    R15,8(,R13)
         ST    R13,4(,R15)
         LR    R13,R15
*
         ST    R2,PARMADDR         SAVE R1
         EJECT
*---------------------------------------------------------------------*
*        INITIALIZATION                                               *
*                                                                     *
*        -PRIME SUBROUTINE LINK REGISTERS                             *
*        -BUILD OTHER REENTRANT CONSTANTS                             *
*---------------------------------------------------------------------*
*
         WHCSBP10
*
         MVI   PCC,$SPACE1         CLEAR PRINT LINE
         MVI   UPDREC,$BLANK
         MVC   UPDREC+1(L'UPDREC-1),UPDREC
         MVI   PDATA,$BLANK
         MVC   PDATA+1(L'PDATA-1),PDATA
         MVI   $WTPTEXT,$BLANK
         MVC   $WTPTEXT+1(L'$WTPTEXT-1),$WTPTEXT
         MVI   H1A,C'1'
         MVI   H1B,$BLANK
         MVI   H1C,$BLANK
         MVI   H2,C'0'
         MVI   H2+1,$BLANK
         MVC   H2+2(L'H2-2),H2+1
         MVI   H3,$BLANK
         MVC   H3+1(L'H3-1),H3
         MVI   H6,$BLANK
         MVC   H6+1(L'H6-1),H6
         MVI   H6,C'0'
         MVI   H7,$BLANK
         MVC   H7+1(L'H7-1),H7
         MVI   SCANB+$BLANK,4
         EJECT
*---------------------------------------------------------------------*
*        INITIALIZATION                                               *
*                                                                     *
*        -BUILD DCB'S                                                 *
*---------------------------------------------------------------------*
*
         MVC   SYSUT1,INDCB
         MVC   DCBDDNAM-IHADCB+SYSUT1,=CL8'SYSUT1'
         LA    R0,READCMPE
         STCM  R0,7,DCBEODA-IHADCB+SYSUT1
*
         MVC   SYSUT2,INDCB
         MVC   DCBDDNAM-IHADCB+SYSUT2,=CL8'SYSUT2'
         LA    R0,READCMPE
         STCM  R0,7,DCBEODA-IHADCB+SYSUT2
*
         MVC   SYSSTAMP,INDCB
         MVC   DCBDDNAM-IHADCB+SYSSTAMP,=CL8'SYSSTAMP'
         LA    R0,ABEND7
         STCM  R0,7,DCBEODA-IHADCB+SYSSTAMP
*
         MVC   SYSLOG,OUTDCB
         MVC   DCBDDNAM-IHADCB+SYSLOG,=CL8'SYSLOG'
         LA    R0,DCBXPRW
         STCM  R0,7,DCBEXLSA-IHADCB+SYSLOG
*
         MVC   SYSUPD,OUTDCB
         MVC   DCBDDNAM-IHADCB+SYSUPD,=CL8'SYSUPD'
         LA    R0,DCBXDLW
         STCM  R0,7,DCBEXLSA-IHADCB+SYSUPD
*
         MVC   SYSLBN,OUTDCB
         MVC   DCBDDNAM-IHADCB+SYSLBN,=CL8'SYSLBN'
         LA    R0,DCBXCIW
         STCM  R0,7,DCBEXLSA-IHADCB+SYSLBN
*
         MVC   SYSHST,OUTDCB
         MVC   DCBDDNAM-IHADCB+SYSHST,=CL8'SYSHST'
         LA    R0,DCBXSTW
         STCM  R0,7,DCBEXLSA-IHADCB+SYSHST
         EJECT
*---------------------------------------------------------------------*
*        AUTHORIZE USER                                               *
*---------------------------------------------------------------------*
*
         LA    R0,=CL8'SYSAUTH'    DCB FOR AUTH
*
         WHCCAU11 CB=CAUCB,DDNA=(R0),AUXL=L'AUTH,AUXA=AUTH,            X
               ERROR=UNAUTH,                                           X
               CPUA=CPUSTOR,CPUL=L'CPUSTOR,PID=1006,WTPA=WTPCB,        X
               PNMA=GENTITLE,PNML=L'GENTITLE
*
         WHCCAS11 CB=CAUCB,PRMA=PARMADDR,SAVA=SUBSAVE,ERROR=UNAUTH
         EJECT
         WHCCAH11 CB=CAHCB,CAUA=CAUCB,DATA=DATCB,ERROR=ABEND1,         X
               SAVA=SUBSAVE,                                           X
               H1A=H1ATEXT,H1L=L'H1ATEXT,                              X
               H2A=H1BTEXT,H2L=L'H1BTEXT,                              X
               H3A=H1CTEXT,H3L=L'H1CTEXT
         EJECT
*---------------------------------------------------------------------*
*        VALIDATE PLIST                                               *
*---------------------------------------------------------------------*
*
         WHCPPG11 REG1=PARMADDR,LOC=PLIST,CB=PPGCB,ERROR=PARMERR,      X
               CAP=$PPLITMS,ENV=CMS
*
         LA    R1,$PPLLLIM         CHECK LINE LIMIT
         BAL   R14,CHECKNUM        .
         BNP   PLLINX              ERROR
         ST    R15,LINLIM          .
*
         LA    R1,$PPLSTOR         GET ALGORITHM STORAGE SIZE
         BAL   R14,CHECKNUM        .
         BNP   PLSTORX             ERROR
         LR    R0,R15              SAVE FOR EDIT
         ST    R0,ALSTORLK         SAVE FOR MSG
         MH    R15,=Y($K)          IN 'K'
         ST    R15,ALSTORL         .
*
         LA    R1,$PPLBOXM         GET BOX MARGINS
         BAL   R14,CHECKNUM        .
         BM    PLBOXMX             ERROR
         ST    R15,BOXMARGN        SAVE
         EJECT
*---------------------------------------------------------------------*
*        VALIDATE AND ENCODE OPTION STRING                            *
*---------------------------------------------------------------------*
*
OPTCHK   DS    0H
         LA    R0,L'$PPLOPNS       SET UP ITERATION CNT
         LA    R15,$PPLOPNS
*
OPTCHK2  EQU   *
         XR    R1,R1               MARK OPTION ON IN TABLE
         IC    R1,0(,R15)          .
         LA    R1,OPNTAB(R1)       .
         OI    0(R1),OPNON         .
         LA    R15,1(,R15)         .
         BCT   R0,OPTCHK2          .
         EJECT
*---------------------------------------------------------------------*
*        VALIDATE AND ENCODE FIELD START LOCATIONS / LENGTH           *
*---------------------------------------------------------------------*
*
         LA    R1,$PPLBBGN         CHECK BASE FIELD BEGIN COLUMN #
         BAL   R14,CHECKNUM        .
         BNP   PLFDERR             ERROR
         BCTR  R15,0
         ST    R15,FILFDSP-FILSECT+CFIL1
         LA    R0,1(,R15)          EDIT IN HDR
         MVI   SVBCL,$BLANK        .
         MVC   SVBCL+1(L'SVBCL-1),SVBCL
         LA    R1,SVBCL            .
         BAL   R14,EDITNO          .
*
         MVC   SVBLN,$PPLBLEN
         CLC   =CL2'*',$PPLBLEN
         BE    QRBGN
         LA    R1,$PPLBLEN         CHECK BASE FIELD LENGTH VALUE
         BAL   R14,CHECKNUM        .
         BNP   PLFDERR             ERROR
         ST    R15,FILFLEN-FILSECT+CFIL1
         LR    R0,R15              EDIT IN HDR
         MVI   SVBLN,$BLANK        .
         MVC   SVBLN+1(L'SVBLN-1),SVBLN
         LA    R1,SVBLN            .
         BAL   R14,EDITNO          .
*
QRBGN    EQU   *
         LA    R1,$PPLRBGN         CHECK REVN FIELD BEGIN COLUMN #
         BAL   R14,CHECKNUM        .
         BNP   PLFDERR             ERROR
         BCTR  R15,0
         ST    R15,FILFDSP-FILSECT+CFIL2
         LA    R0,1(,R15)          EDIT IN HDR
         MVI   SVRCL,$BLANK        .
         MVC   SVRCL+1(L'SVRCL-1),SVRCL
         LA    R1,SVRCL            .
         BAL   R14,EDITNO          .
*
         MVC   SVRLN,$PPLRLEN
         CLC   =CL2'*',$PPLRLEN
         BE    QPAD
         LA    R1,$PPLRLEN         CHECK REVN FIELD LENGTH VALUE
         BAL   R14,CHECKNUM        .
         BNP   PLFDERR             ERROR
         ST    R15,FILFLEN-FILSECT+CFIL2
         LR    R0,R15              EDIT IN HDR
         MVI   SVRLN,$BLANK        .
         MVC   SVRLN+1(L'SVRLN-1),SVRLN
         LA    R1,SVRLN            .
         BAL   R14,EDITNO          .
         EJECT
*---------------------------------------------------------------------*
*        PARM VALIDATION                                              *
*                                                                     *
*        VALIDATE AND ENCODE PAD AND STRIP CHARACTERS                 *
*---------------------------------------------------------------------*
*
QPAD     EQU   *
         LA    R1,$PPLPAD+L'$PPLPAD
         TRT   $PPLPAD,SCANB
         LA    R0,$PPLPAD
         SR    R1,R0
         LR    R0,R1
*
         WHCECX11 ILEN=(R0),ILOC=$PPLPAD,OLEN=L'COMPPAD,OLOC=COMPPAD,  X
               CB=ECXCB,ERROR=PLPADERR
*
         CLC   $PPLSTR,=CL8'NONE'
         BE    TIME
*
         LA    R1,$PPLSTR+L'$PPLSTR
         TRT   $PPLSTR,SCANB
         LA    R0,$PPLSTR
         SR    R1,R0
         LR    R0,R1
*
         WHCECX11 ILEN=(R0),ILOC=$PPLSTR,OLEN=L'STRIPCHR,OLOC=STRIPCHR,X
               CB=ECXCB,ERROR=PLSTRERR
*
         OI    PROCESS2,PRO2STRP
         EJECT
*---------------------------------------------------------------------*
*        INITIALIZATION                                               *
*                                                                     *
*        READ TIME STAMP AND EDIT DATA IN HEADING                     *
*---------------------------------------------------------------------*
*
TIME     EQU   *
         WHCLST10 (SYSSTAMP,$OPENI),CB=LIST,SVC=$SVCOPN
*
         TM    DCBRECFM-IHADCB+SYSSTAMP,DCBRECV
         BO    ABEND6
         CLC   DCBLRECL-IHADCB+SYSSTAMP,=AL2($TIMCBL)
         BL    ABEND6
*
         GET   SYSSTAMP
*
         MVC   SVDAT,$TIMCDAT-$TIMCB(R1)
         MVC   SVTIM,$TIMCTIM-$TIMCB(R1)
         MVC   STATDATE,$TIMBDAT-$TIMCB(R1)
         MVC   STATTIME,$TIMBTIM-$TIMCB(R1)
         LA    R1,SYSSTAMP         CLOSE FILE
         BAL   R14,CLOSDCB
         EJECT
*---------------------------------------------------------------------*
*        INITIALIZATION                                               *
*                                                                     *
*        FILL IN PARAMETER SUBHEADINGS                                *
*---------------------------------------------------------------------*
*
         WHCDDF11 FILL=ONLY,                                           X
               OLEN=HSLOTLEN,CB=DDFCB,                                 X
               FLOC=HSLOTFIL,FLEN=L'HSLOTFIL
*
         WHCDDF11 LLOC=DATLIT,LLEN=L'DATLIT,RLOC=SVDAT,OLOC=HDAT,      X
               CB=DDFCB,RLEN=L'SVDAT,OPNS=(L,R)
*
         WHCDDF11 LLOC=TIMLIT,LLEN=L'TIMLIT,RLOC=SVTIM,OLOC=HTIM,      X
               CB=DDFCB,RLEN=L'SVTIM,OPNS=(L,R)
*
         WHCDDF11 LLOC=OPNLIT,LLEN=L'OPNLIT,RLOC=$PPLOPNS,             X
               RLEN=L'$PPLOPNS,                                        X
               OLOC=HOPN,OPNS=(L,R),CB=DDFCB
*
         WHCDDF11 LLOC=BCLLIT,LLEN=L'BCLLIT,RLOC=SVBCL,RLEN=L'SVBCL,   X
               OLOC=HBCL,OPNS=(L,R),CB=DDFCB
*
         WHCDDF11 LLOC=BLNLIT,LLEN=L'BLNLIT,RLOC=SVBLN,RLEN=L'SVBLN,   X
               OLOC=HBLN,OPNS=(L,R),CB=DDFCB
*
         WHCDDF11 LLOC=RCLLIT,LLEN=L'RCLLIT,RLOC=SVRCL,RLEN=L'SVRCL,   X
               OLOC=HRCL,OPNS=(L,R),CB=DDFCB
*
         WHCDDF11 LLOC=RLNLIT,LLEN=L'RLNLIT,RLOC=SVRLN,RLEN=L'SVRLN,   X
               OLOC=HRLN,OPNS=(L,R),CB=DDFCB
*
         WHCDDF11 LLOC=PADLIT,LLEN=L'PADLIT,RLOC=$PPLPAD,              X
               RLEN=2,                                                 X
               OLOC=HPAD,OPNS=(L,R),CB=DDFCB
         EJECT
*---------------------------------------------------------------------*
*        DO PARAMETER SUMMARY PROCESSING                              *
*                                                                     *
*        -FORCE NEW PAGE                                              *
*        -GET ALGORITHM STORAGE                                       *
*        -BUILD APPROPRIATE BOX CHARACTER SET TABLE                   *
*---------------------------------------------------------------------*
*
         L     R0,ALSTORL          GET ALGORITHM STORAGE
         ST    R0,GETMAX           BUILD GETMAIN LIST
         ST    R0,GETMIN
*
         GETMAIN VC,LA=GETMIN,A=GETALLAD,MF=(E,GETMAINL)
*
         LTR   R15,R15             STORAGE GOT?
         BNZ   ALSTOERR            NO...
         MVC   ALSTORA,GETALLAD    COPY ADDRESS
*
         TM    OPNTAB+C'Y',OPNON   3800 BOX CHAR SET?
         BO    BOX3800             YES...
         TM    OPNTAB+C'X',OPNON   1403 TEXT CHAR SET?
         BO    BOX1403T            YES...
         LA    R15,BXC1403         BUILD DEFAULT (TERM) BOX CHAR SET
         MVC   BOXCSET($BXCCBL),0(R15)
         B     OPENLOG             CONTINUE.
*
BOX3800  DS    0H
         LA    R15,BXC3800         3800 BOX CHAR SET
         MVC   BOXCSET($BXCCBL),0(R15)
         B     OPENLOG             CONTINUE.
*
BOX1403T DS    0H
         LA    R15,BXC1403T        1403 BOX CHAR SET
         MVC   BOXCSET($BXCCBL),0(R15)
         B     OPENLOG             CONTINUE.
         EJECT
*---------------------------------------------------------------------*
*        GENERAL OPEN PROCESSING                                      *
*                                                                     *
*        OPEN LOG/UPDATE FILES                                        *
*        SET UP FOR 010 REPORT PRINTING                               *
*---------------------------------------------------------------------*
*
OPENLOG  EQU   *
         WHCLST10 (SYSHST,$OPENO,                                      X
               SYSLOG,$OPENO),                                         X
               CB=LIST,SVC=$SVCOPN
*
         MVC   LINCNT,LINLIM
         MVI   H4,C'0'
         MVI   H4+1,$BLANK
         MVC   H4+2(L'H4-2),H4+1
         MVI   H4BGN,C'-'
         MVC   H4BGN+1(H4LEN-1),H4BGN
         MVC   H4+1+(L'H4-1-L'DIAGLIT)/2(L'DIAGLIT),DIAGLIT
         MVC   CURHLINS,=F'12'
*
         WHCDDF11 LLOC=REPLIT,LLEN=L'REPLIT,RLOC=REP010,RLEN=L'REP010, X
               OLOC=HREP,OPNS=(L,R),CB=DDFCB
*
         BAL   R14,LNSMSG          ISSUE LINE LIMIT MSG
         BAL   R14,STRMSG          ISSUE STRIP CHR  MSG
         EJECT
*---------------------------------------------------------------------*
*        GENERAL OPEN PROCESSING                                      *
*                                                                     *
*        -OPEN COMPARAND FILES                                        *
*---------------------------------------------------------------------*
*
* BUILD DCB PTR'S IN FILE CB'S:
*
         LA    R1,SYSUT1
         ST    R1,FILDCBA-FILSECT+CFIL1
         MVC   FILDESCR-FILSECT+CFIL1,=CL8'BASE'
         LA    R1,SYSUT2
         ST    R1,FILDCBA-FILSECT+CFIL2
         MVC   FILDESCR-FILSECT+CFIL2,=CL8'REVISION'
*
* OPEN COMPARAND FILES:
*
         LA    R1,CFIL1            OPEN BASE
         BAL   R14,OPENCMP         .
         BNZ   BASOPERR            . OPEN FAILED
         MVC   STATBDSN,FILDSN-FILSECT+CFIL1 DSN FOR STATS
*
         LA    R1,CFIL2            OPEN REVN
         BAL   R14,OPENCMP         .
         BNZ   REVOPERR            . OPEN FAILED
         MVC   STATRDSN,FILDSN-FILSECT+CFIL2 DSN FOR STATS
         B     OPENUPD             CONTINUE.
*
REVOPERR DS    0H
         L     R1,FILDCBA-FILSECT+CFIL1
         BAL   R14,CLOSDCB         CLOSE BASE
*
BASOPERR EQU   *
         BAL   R14,SEVMSG          YES---LOG AND EXIT.
         BAL   R14,FOOTEND
         BAL   R14,CLOSLOG         CLOSE LOG/HISTORY FILES
         B     END
         EJECT
*---------------------------------------------------------------------*
*        GENERAL OPEN PROCESSING                                      *
*                                                                     *
*        GET MIN LRECL FOR UPDATE FILE AND PUBLISH                    *
*        GETMAIN FOR UPDATE RECORD                                    *
*---------------------------------------------------------------------*
*
*        COMPUTE MINIMUM LRECL FOR DELTA FILE:
*
OPENUPD  DS    0H
         L     R0,FILLRECL-FILSECT+CFIL2 LEN OF LARGEST POSS INSERT TXT
         A     R0,=A($DLRTFXL)           .
         C     R0,=A($DLRMAXL)           OR LARGEST FIXED RECORD TYPE
         BNL   OPENUPD2                  .
         L     R0,=A($DLRMAXL)           .
*
OPENUPD2 EQU   *
         ST    R0,UPDRECLN
         MVC   PMSG(L'MSG027I),MSG027I
         LA    R1,PMSG+L'MSG027I
         BAL   R14,EDITNO
         BAL   R14,PRINT
*
         WHCLST10 (SYSLBN,$OPENO,                                      X
               SYSUPD,$OPENO),                                         X
               CB=LIST,SVC=$SVCOPN
*
         LA    R0,=CL8'SYSUPD'     DISPLAY DCB ATTR'S
         LA    R1,SYSUPD           .
         BAL   R14,DISPDCB         .
*
*        ENSURE RECFM V AND LRECL LARGE ENOUGH:
*
         TM    DCBRECFM-IHADCB+SYSUPD,DCBRECU
         BO    ABEND8
         TM    DCBRECFM-IHADCB+SYSUPD,DCBRECF
         BO    ABEND8
         LH    R0,DCBLRECL-IHADCB+SYSUPD
         C     R0,UPDRECLN
         BL    ABEND8
*
         L     R0,UPDRECLN
         GETMAIN R,LV=(0)
         ST    R1,UPDRECAD
         B     PICKALG
*
MSG027I  DC    C'&WHRN.027I MINIMUM UPDATE FILE LRECL IS '
         EJECT
*---------------------------------------------------------------------*
*        GENERAL OPEN PROCESSING                                      *
*                                                                     *
*        (OPEN COMPARAND FILES)                                       *
*                                                                     *
*        -PICK ALGORITHM FOR COMPARISON                               *
*---------------------------------------------------------------------*
*
PICKALG  DS    0H
         LA    R0,TYPE1            ASSUME TYPE 1 ALGORITHM
         ST    R0,COMPALGA         .
*
* IF BOTH FILES RECFM F, THEIR LRECL'S ARE SAME AND .LT. 256,
* IF BOTH FILES FIELD LENGTH AND DISP ARE SAME,
* USE ALGORITHM 2...
*
         TM    DCBRECFM-IHADCB+SYSUT1,DCBRECV
         BO    TYP1MSG
         TM    DCBRECFM-IHADCB+SYSUT2,DCBRECV
         BO    TYP1MSG
         CLC   DCBLRECL-IHADCB+SYSUT1,DCBLRECL-IHADCB+SYSUT2
         BNE   TYP1MSG
         CLC   DCBLRECL-IHADCB+SYSUT1,=H'256'
         BNL   TYP1MSG
         CLC   FILFDSP-FILSECT+CFIL1,FILFDSP-FILSECT+CFIL2
         BNE   TYP1MSG
         CLC   FILFLEN-FILSECT+CFIL1,FILFLEN-FILSECT+CFIL2
         BNE   TYP1MSG
*
         LA    R0,TYPE2            USE TYPE 2 ALGORITHM
         ST    R0,COMPALGA         .
         MVC   PMSG(L'T2MSG),T2MSG
         BAL   R14,PRINT
         B     READ
*
TYP1MSG  DS    0H
         MVC   PMSG(L'T1MSG),T1MSG
         BAL   R14,PRINT
         B     READ
*
T1MSG    DC    C'&WHRN.001I TYPE I COMPARISON ALGORITHM WILL BE USED'
T2MSG    DC    C'&WHRN.002I TYPE II COMPARISON ALGORITHM WILL BE USED'
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - STAGE 1                                          *
*                                                                     *
*        READ ALL RECORD DATA FROM SYSUT1/SYSUT2 INTO STORAGE         *
*---------------------------------------------------------------------*
*
READ     DS    0H
         LA    R1,CFIL1            READ DATA FROM COMPARAND-1
         BAL   R14,READCMP
         MVC   STATBRCD,FILCNT-FILSECT+CFIL1
         BAL   R14,CLOSCMP
         LA    R1,CFIL2            READ DATA FROM COMPARAND-2
         BAL   R14,READCMP
         MVC   STATRRCD,FILCNT-FILSECT+CFIL2
         BAL   R14,CLOSCMP
*
         TM    PROCESS,PROCLERR    LENGTH ERROR?
         BO    READERR             YES...
         TM    PROCESS,PROCSTOX    STORAGE ERROR?
         BZ    STAGE1A             NO, CHECK FOR EMPTY D/S'S.
*
         MVC   PMSG(L'MSGSEXH1),MSGSEXH1
         BAL   R14,PRINT
*
READERR  EQU   *
         BAL   R14,STORSTAT
         LA    R1,XSTORERR         SET R/C
         BAL   R14,SETRCODE        .
         BAL   R14,STORSTAT
         BAL   R14,SEVMSG
         BAL   R14,FOOTEND
         BAL   R14,CLOSUPD         CLOSE UPDATE/LIBRARIAN FILES
         BAL   R14,CLOSLOG         CLOSE LOG/HISTORY FILES
         B     END
*
MSGSEXH1 DC    C'&WHRN.003E STORAGE POOL EXHAUSTED'
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - STAGE 1                                          *
*                                                                     *
*        -CHECK FOR EMPTY DATASETS                                    *
*        -CHECK FOR EXCESSIVELY LARGE DATASETS                        *
*---------------------------------------------------------------------*
*
STAGE1A  DS    0H
         NC    FILCNT-FILSECT+CFIL1,FILCNT-FILSECT+CFIL1
         BZ    STAGE1B
         NC    FILCNT-FILSECT+CFIL2,FILCNT-FILSECT+CFIL2
         BNZ   STAGE1C
*
STAGE1B  EQU   *
         MVC   PMSG(L'MSGEMPT1),MSGEMPT1 ISSUE MSG / SET RC
         BAL   R14,PRINT
         LA    R1,XFILERR
         BAL   R14,SETRCODE
         BAL   R14,STORSTAT
         BAL   R14,SEVMSG
         BAL   R14,FOOTEND
         BAL   R14,CLOSUPD         CLOSE UPDATE/LIBRARIAN FILES
         BAL   R14,CLOSLOG         CLOSE LOG/HISTORY FILES
         B     END
*
STAGE1C  DS    0H
         CLC   FILCNT-FILSECT+CFIL1,=F'65535'
         BH    STAGE1D
         CLC   FILCNT-FILSECT+CFIL2,=F'65535'
         BNH   STAGE1E
*
STAGE1D  EQU   *
         MVC   PMSG(L'MSGHIGH1),MSGHIGH1 ISSUE MSG / SET RC
         BAL   R14,PRINT
         LA    R1,XFILERR
         BAL   R14,SETRCODE
         BAL   R14,STORSTAT
         BAL   R14,SEVMSG
         BAL   R14,FOOTEND
         BAL   R14,CLOSUPD         CLOSE UPDATE/LIBRARIAN FILES
         BAL   R14,CLOSLOG         CLOSE LOG/HISTORY FILES
         B     END
*
MSGEMPT1 DC    C'&WHRN.004E ONE OR BOTH COMPARAND FILES ARE EMPTY'
MSGHIGH1 DC    C'&WHRN.005E ONE OR BOTH COMPARAND FILES CONTAIN TOO MANX
               Y RECORDS'
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - STAGE 1                                          *
*                                                                     *
*        -FIND OUT WHICH FILE SMALLER / LONGER                        *
*        -ALLOCATE LCS INDEX ARRAY                                    *
*        -HANDLE ALLOCATION ERROR                                     *
*---------------------------------------------------------------------*
*
STAGE1E  DS    0H
         LA    R0,CFIL1            ASSUME 1 SMALLER / 2 LARGER
         ST    R0,SMALLER
         LA    R0,CFIL2
         ST    R0,LARGER
         CLC   FILCNT-FILSECT+CFIL1,FILCNT-FILSECT+CFIL2
         BNH   STAGE1F
         XC    SMALLER,LARGER      NOT TRUE - SWAP
         XC    LARGER,SMALLER
         XC    SMALLER,LARGER
         OI    PROCESS,PROCCSWP    SHOW SWAPPED
*
STAGE1F  EQU   *
         BAL   R14,ALIGN           ENSURE ARRAY ALLOCATED ON DW
         L     R14,SMALLER         LARGEST LCS = SMALLEST REC/CNT
         LA    R1,1                ADD 1 FOR LCS SET 0
         A     R1,FILCNT-FILSECT(,R14)
         MH    R1,=Y(LCSSECL)      .
         BAL   R14,ALLOC           .
         BNZ   LCSPREP             ARRAY GOT ...
*
         MVC   PMSG(L'MSGLCSS1),MSGLCSS1 ISSUE MSG / SET RC
         BAL   R14,PRINT
         LA    R1,XSTORERR
         BAL   R14,SETRCODE
         BAL   R14,STORSTAT
         BAL   R14,SEVMSG
         BAL   R14,FOOTEND
         BAL   R14,CLOSUPD         CLOSE UPDATE/LIBRARIAN FILES
         BAL   R14,CLOSLOG         CLOSE LOG/HISTORY FILES
         B     END
*
MSGLCSS1 DC    C'&WHRN.006E LCS ARRAY ALLOCATION FAILED'
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - LCS ARRAY GOT                                    *
*                                                                     *
*        -BUILD ARRAY POINTERS                                        *
*        -BUILD A(MAX) AS MARKER IN S(0)                              *
*        -CALL COMPARISON ALGORITHM AND CHECK RESULTS                 *
*---------------------------------------------------------------------*
*
LCSPREP  DS    0H
         ST    R15,LCSARRAD        SAVE ARRAY ADDR
         ST    R15,LCSLEVEL        ALSO = 1ST ENTRY USED
         LA    R0,LCSSECL(,R15)    GENERATE LEN=1 ENTRY PTR
         ST    R0,LCSL1PTR         FOR LATER
         L     R14,SMALLER         POINT FILCB
         MVC   LCSMARK-LCSSECT(L'LCSMARK,R15),FILCNT+L'FILCNT-L'LCSMARKX
               -FILSECT(R14)
         BAL   R14,ALIGN           ENSURE NODE ALLOC'NS START ALIGNED
         L     R15,COMPALGA        GET COMPARISON CODE ADDR
         BALR  R14,R15             PERFORM
         BZ    LCSGOT              LCS GOT.
*
         MVC   PMSG(L'MSGNSRQ1),MSGNSRQ1 ISSUE MSG / SET RC
         L     R0,FILSEQ-FILSECT+CFIL1
         LA    R1,PMSG+L'MSGNSRQ1
         BAL   R14,EDITNO
         MVC   0(L'MSGNSRQ2,R15),MSGNSRQ2
         L     R0,FILSEQ-FILSECT+CFIL2
         LA    R1,L'MSGNSRQ2(,R15)
         BAL   R14,EDITNO
         MVC   0(L'MSGNSRQ3,R15),MSGNSRQ3
         BAL   R14,PRINT
*
         LA    R1,XSTORERR
         BAL   R14,SETRCODE
         BAL   R14,NODESTAT
         BAL   R14,STORSTAT
         BAL   R14,SEVMSG
         BAL   R14,FOOTEND
         BAL   R14,CLOSUPD         CLOSE UPDATE/LIBRARIAN FILES
         BAL   R14,CLOSLOG         CLOSE LOG/HISTORY FILES
         B     END
*
MSGNSRQ1 DC    C'&WHRN.007E NODE STORAGE REQUEST FAILED ('
MSGNSRQ2 DC    C':'
MSGNSRQ3 DC    C')'
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - LCS FOUND / NODES CONSTRUCTED                    *
*                                                                     *
*        -FIND OUT S(K) OF A(N) --- LONGEST LCS                       *
*        -IF LCS 0, SET R/C AND ISSUE MSG (NO RCDS IN COMMON)         *
*---------------------------------------------------------------------*
*
LCSGOT   DS    0H
         L     R14,SMALLER
         L     R1,FILCNT-FILSECT(,R14)
         BAL   R14,FINDSET
         ST    R15,LCSLEN
         ST    R15,STATLCSL
         LTR   R15,R15
         BNZ   LCSGOT1A
         LA    R1,XNOCOMD          SET SEVERITY IF NO LCS
         BAL   R14,SETRCODE
         OI    PROCESS,PROCNCOM
         MVC   PMSG(L'MSGNCOM1),MSGNCOM1
         BAL   R14,PRINT
         B     LCSGOT2
*
LCSGOT1A DS    0H
*
* GET # OF SEQUENCES AT THIS LENGTH:
*
         L     R1,LCSLEN           POINT TO NODE ANCHOR
         MH    R1,=Y(LCSSECL)      .
         A     R1,LCSARRAD         .
         ICM   R1,7,LCSPTR-LCSSECT(R1)
         LA    R0,1                COUNTER
*
LCSGOT1B EQU   *
         ICM   R1,7,NODENEXT-NODESECT(R1)
         BZ    LCSGOT1C
         AH    R0,=H'1'
         B     LCSGOT1B
*
LCSGOT1C DS    0H
         ST    R0,STATLCSN
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - LCS FOUND / NODES CONSTRUCTED                    *
*                                                                     *
*        -SET R/C & ISSUE MSG IF FILES IDENTICAL                      *
*        -COMPLETE STATISTICS MSG'S                                   *
*        -IF NO LCS, SKIP PAIRS ALLOC AND WRITE REPORT                *
*---------------------------------------------------------------------*
*
LCSGOT2  EQU   *
         L     R1,SMALLER          SET SEVERITY IF FILES EQUAL
         L     R2,LARGER           .
         CLC   FILCNT-FILSECT(L'FILCNT,R1),FILCNT-FILSECT(R2)
         BNE   LCSGOT3
         CLC   LCSLEN,FILCNT-FILSECT(R1)
         BNE   LCSGOT3
*
         LA    R1,XIDEN
         BAL   R14,SETRCODE
         OI    PROCESS,PROCFIDN
         MVC   PMSG(L'MSGFIDN1),MSGFIDN1
         BAL   R14,PRINT
*
LCSGOT3  EQU   *
         BAL   R14,LCSLMSG         ISSUE LCS LEN MSG
         BAL   R14,NODESTAT
         BAL   R14,STORSTAT
         BAL   R14,SEVMSG
         BAL   R14,FOOTEND
*
         NC    LCSLEN,LCSLEN       DO NOT GET PAIRS ARRAY IF LCS 0
         BZ    REPORT
         B     ALLOCP
*
MSGFIDN1 DC    C'&WHRN.008I FILES ARE IDENTICAL'
MSGNCOM1 DC    C'&WHRN.009I FILES HAVE NO RECORDS IN COMMON'
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - LCS FOUND / NODES CONSTRUCTED                    *
*                                                                     *
*        -LCS FOUND (NON-0 LENGTH)                                    *
*        -ALLOCATE PAIRS STACK TO ORDER FOR PROCESSING                *
*        -HANDLE ALLOCATION FAILURE                                   *
*---------------------------------------------------------------------*
*
* ALLOCATE PAIR STACK:
*
ALLOCP   DS    0H
         BAL   R14,ALIGN           ALIGN
         L     R1,LCSLEN           ALLOCATE STACK
         MH    R1,=Y(PAIRSECL)     .
         BAL   R14,ALLOC           .
         BZ    PAIERROR            CANNOT...
         ST    R1,PAIRSLN
         ST    R15,PAIRSAD
         LA    R0,0(R1,R15)
         ST    R0,PAIRSBND
         B     REPORT
*
PAIERROR DS    0H
         MVC   PMSG(L'MSGPAIS1),MSGPAIS1 ISSUE MSG / SET RC
         BAL   R14,PRINT
         LA    R1,XSTORERR
         BAL   R14,SETRCODE
         BAL   R14,LCSLMSG
         BAL   R14,NODESTAT
         BAL   R14,STORSTAT
         BAL   R14,SEVMSG
         BAL   R14,FOOTEND
         BAL   R14,CLOSUPD         CLOSE UPDATE/LIBRARIAN FILES
         BAL   R14,CLOSLOG         CLOSE LOG/HISTORY FILES
         B     END
*
MSGPAIS1 DC    C'&WHRN.010E PAIR ARRAY ALLOCATION FAILED'
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - LCS FOUND / NODES CONSTRUCTED                    *
*                                                                     *
*        -GET LARGEST TEXT SIZE WE CAN PRINT                          *
*        -GENERATE PRINT LINE PTR FOR TEXT/SEQUENCE NUMBERS           *
*---------------------------------------------------------------------*
*
REPORT   DS    0H
         TM    OPNTAB+C'B',OPNON   MAKE BOX PROC'G PENDING IF OPN SPEC
         BZ    REPORT1             .
         OI    PROCESS,PROCBOX     .
*
REPORT1  EQU   *
         L     R1,FILLRECL-FILSECT+CFIL1
         C     R1,FILLRECL-FILSECT+CFIL2
         BNL   REPORT2
         L     R1,FILLRECL-FILSECT+CFIL2
*
REPORT2  EQU   *
         ST    R1,LARLRECL         SAVE LARGEST LRECL
         ST    R1,LARPRINT         BUILD PRINT LRECL FROM SMALLER
         LA    R2,L'PDATA-SEQSECL  OF LARGEST POSS RCD / PLINE MAX
         CR    R2,R1               .
         BNL   REPORT2B            .
         ST    R2,LARPRINT         .
*
REPORT2B EQU   *
         SR    R2,R1               GET SPACE LEFT OVER IN PLINE
         BNM   REPORT3             .
         XR    R2,R2               .
*
REPORT3  EQU   *
         SRL   R2,1                CALCULATE CENTERED REC POS IN PLINE
         LR    R1,R2               (SAVE EXTRA SPACE ON EITHER SIDE)
         LA    R2,PDATA+SEQSECL(R2)
         ST    R2,PRECPTR          .
         ST    R2,PSEQPTR          ASSUME WE'LL USE FOR FINDING SEQ LOC
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - LCS FOUND / NODES CONSTRUCTED                    *
*                                                                     *
*        -GENERATE BOX POINTERS IF POSSIBLE                           *
*        -UPDATE SEQUENCE NUMBERS PTR IF BOXES OK                     *
*        -RESET BOX INDICATOR IF WE CANNOT HANDLE                     *
*        -GENERATE UPDATE LISTING                                     *
*---------------------------------------------------------------------*
*
         C     R1,BOXMARGN         TRY TO RESERVE UP TO X BYTES ON
         BNH   REPORT4             EITHER SIDE
         L     R1,BOXMARGN
*
REPORT4  EQU   *
         LTR   R1,R1               ANY EXTRA SPACE?
         BZ    REPORT5             NO EXTRA SPACE - NO BOX
         LNR   R0,R1               BUILD LEFT POSITION
         A     R0,PRECPTR          .
         ST    R0,LBOXPTR          .
         ST    R0,PSEQPTR          . NEW SEQ FIND VALUE
         LR    R0,R1               BUILD RIGHT POSITION
         A     R0,PRECPTR          .
         A     R0,LARPRINT         .
         BCTR  R0,0                .
         ST    R0,RBOXPTR          .
         B     REPORT6
*
REPORT5  DS    0H
         NI    PROCESS,255-PROCBOX BOX PROCESSING IMPOSSIBLE
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - LCS FOUND / NODES CONSTRUCTED                    *
*                                                                     *
*        -GENERATE DATA SUBHEADING TEXT                               *
*        -GENERATE UPDATE LISTING                                     *
*---------------------------------------------------------------------*
*
REPORT6  EQU   *
         L     R1,PSEQPTR          FINALIZE SEQ PTR
         SH    R1,=Y(SEQSECL)      .
         ST    R1,PSEQPTR          .
         LA    R0,PLINE            FIND POSITION IN DATA SUBHEADINGS
         SR    R1,R0               .
         LA    R2,H6(R1)           .
         MVC   SEQ1-SEQSECT(L'SEQ1,R2),=CL(L'SEQ1)'B/SEQ'
         MVC   SEQ2-SEQSECT(L'SEQ2,R2),=CL(L'SEQ2)'R/SEQ'
         LA    R2,H7(R1)
         MVC   SEQ1-SEQSECT(L'SEQ1,R2),=CL(L'SEQ1)'-----'
         MVC   SEQ2-SEQSECT(L'SEQ2,R2),=CL(L'SEQ2)'-----'
*
         L     R1,PRECPTR
         LA    R0,PLINE
         SR    R1,R0
         LA    R2,H6(R1)
         L     R1,LARPRINT
         BCTR  R1,0
         LA    R15,SCALE1
         TM    OPNTAB+C'T',OPNON
         BZ    REPORT7
         LA    R15,SCALE2
*
REPORT7  EQU   *
         EX    R1,H7MOVE
*
         L     R1,PRECPTR
         LA    R0,PLINE
         SR    R1,R0
         LA    R2,H7(R1)
         L     R1,LARPRINT
         BCTR  R1,0
         EX    R1,H7ZERO
         EX    R1,H7INSR
         EJECT
*---------------------------------------------------------------------*
*        MAIN LINE - LCS FOUND / NODES CONSTRUCTED                    *
*                                                                     *
*        WRITE UPDATE FILE HEADER RCD                                 *
*        GENERATE TEXT LISTING                                        *
*        WRITE STATS RCD                                              *
*---------------------------------------------------------------------*
*
         L     R1,UPDRECAD         BUILD UPDATE HDR
         LA    R0,$DLRHLEN
         SLL   R0,2*8
         STCM  R0,15,$DLRFRDW-$DLRREC(R1)
         MVI   $DLRFTY-$DLRREC(R1),$DLRFTYH
         MVC   $DLRHLVL-$DLRREC(L'$DLRHLVL,R1),=C'&WHLV1.&WHLV2'
         MVC   $DLRHBDS-$DLRREC(L'$DLRHBDS,R1),STATBDSN
         MVC   $DLRHBRC-$DLRREC(L'$DLRHBRC,R1),STATBRCD
         MVC   $DLRHRDS-$DLRREC(L'$DLRHRDS,R1),STATRDSN
         MVC   $DLRHRRC-$DLRREC(L'$DLRHRRC,R1),STATRRCD
         BAL   R14,WRITUPD         WRITE UPDATE HDR
*
         L     R1,LCSLEN           GENERATE UPDATE LISTING FOR LCSLEN
         BAL   R14,GENLIST         .
*
         PUT   SYSHST,STATREC      ISSUE STATS RCD
*
         BAL   R14,CLOSUPD         CLOSE UPDATE/LIBRARIAN FILES
         BAL   R14,CLOSLOG         CLOSE LOG/HISTORY FILES
         B     END
*
H7MOVE   MVC   0(0,R2),0(R15)
H7ZERO   XC    0(0,R2),0(R2)
H7INSR   TR    0(0,R2),=C'-'
         EJECT
*---------------------------------------------------------------------*
*        END-OF-PROCESS                                               *
*                                                                     *
*        CONFIRM AUTHORIZATION                                        *
*---------------------------------------------------------------------*
*
END      DS    0H
         WHCTAA10 ERR=ABEND5       CONFIRM AUTHORIZATION
*
         B     FREE
         EJECT
*---------------------------------------------------------------------*
*        ERROR EXITS                                                  *
*---------------------------------------------------------------------*
*
PLLINX   DS    0H
         WTO   '&WHRN.011E LINE LIMIT PARAMETER INVALID',ROUTCDE=11
         B     PARMERR
*
PLSTORX  DS    0H
         WTO   '&WHRN.012E STORAGE PARAMETER INVALID',ROUTCDE=11
         B     PARMERR
*
PLBOXMX  DS    0H
         WTO   '&WHRN.013E BOX MARGIN PARAMETER INVALID',ROUTCDE=11
         B     PARMERR
         EJECT
*---------------------------------------------------------------------*
*        ERROR EXITS               (CONTINUED)                        *
*---------------------------------------------------------------------*
*
PLPADERR DS    0H
         WTO   '&WHRN.029E COMPARE-PAD CHARACTER INVALID',ROUTCDE=11
         B     PARMERR
*
PLSTRERR DS    0H
         WTO   '&WHRN.030E STRIP CHARACTER INVALID',                   X
               ROUTCDE=11
         B     PARMERR
         EJECT
*---------------------------------------------------------------------*
*        ERROR EXITS               (CONTINUED)                        *
*---------------------------------------------------------------------*
*
ALSTOERR DS    0H
         WTO   '&WHRN.014E REQUIRED STORAGE UNAVAILABLE',ROUTCDE=11
         B     PARMERR
*
PLFDERR  DS    0H
         WTO   '&WHRN.015E BASE/REVISION FIELD DEFINITION ERROR',      X
               ROUTCDE=11
         B     PARMERR
*
PARMERR  EQU   *
         LA    R1,XPARMERR
         BAL   R14,SETRCODE
         B     FREE
         EJECT
*---------------------------------------------------------------------*
*        UNAUTHORIZED USE EXIT                                        *
*---------------------------------------------------------------------*
*
UNAUTH   DS    0H
         LA    R1,XUNAUTH          SET UNAUTH R/C
         BAL   R14,SETRCODE        .
         B     FREE                EXIT.
         EJECT
*---------------------------------------------------------------------*
*        COMMON TERMINATION                                           *
*---------------------------------------------------------------------*
*
FREE     DS    0H
         ICM   R1,15,ALSTORA       STORAGE ALLOCATED?
         BZ    FREEUPD             NO...
         L     R0,ALSTORL          .
         FREEMAIN R,LV=(0),A=(1)
*
FREEUPD  EQU   *
         ICM   R1,15,UPDRECAD      STORAGE ALLOCATED?
         BZ    FREEWS              NO...
         L     R0,UPDRECLN         .
         FREEMAIN R,LV=(0),A=(1)
*
FREEWS   EQU   *
         L     R13,4(,R13)
         L     R2,RETURNC
         LR    R1,RWS
         L     R0,=A(WSLEN)
*
         FREEMAIN R,LV=(0),A=(1)
*
         LR    R15,R2
*
         RETURN (14,12),RC=(15)
         EJECT
*---------------------------------------------------------------------*
*        ABENDS                                                       *
*---------------------------------------------------------------------*
*
ABEND1   DS    0H
         MVI   AC,1                ERROR BUILDING HEADINGS
         B     ABENDX
*
ABEND2   DS    0H
         MVI   AC,2                BINARY SEARCH ERROR
         B     ABENDX
*
ABEND3   DS    0H
         MVI   AC,3                ALG. UNEXPECTED EVENT
         B     ABENDX
*
ABEND4   DS    0H
         MVI   AC,4                SAVE STACK EXHAUST
         B     ABENDX
*
ABEND5   DS    0H
         MVI   AC,5                AUTH ERROR
         B     ABENDX
*
ABEND6   DS    0H
         MVI   AC,6                SYSSTAMP FORMAT ERROR
         B     ABENDX
*
ABEND7   DS    0H
         MVI   AC,7                SYSSTAMP EOF
         B     ABENDX
*
ABEND8   DS    0H
         MVI   AC,8                SYSUPD DCB ATTR ERROR
         B     ABENDX
         EJECT
*---------------------------------------------------------------------*
*        ABENDS (CONTINUED)                                           *
*---------------------------------------------------------------------*
*
ABENDX   DS    0H
         STM   R0,R15,AREG
*
*        LA    R2,AREG
*        MVC   REGCNT,=F'-1'
*
ABENDX2  EQU   *
*        LA    R0,1
*        A     R0,REGCNT
*        ST    R0,REGCNT
*        L     R14,0(,R2)
*
*        LINEDIT TEXT='R..: ........',                                 X
               COMP=NO,MF=(E,LINELIST),RENT=YES,DOT=NO,                X
               SUB=(DECA,REGCNT,HEX,(R14))
*
*        LA    R2,4(,R2)
*        LA    R0,AREG+L'AREG
*        CR    R2,R0
*        BL    ABENDX2
*
         XR    R1,R1
         IC    R1,AC
*
         ABEND (1),DUMP
         EJECT
*---------------------------------------------------------------------*
*        TYPE 1 COMPARISON ALGORITHM                                  *
*                                                                     *
*        -COMPARE B(J) WITH A(N), A(N-1), ... A(1)                    *
*---------------------------------------------------------------------*
*
TYPE1    WHCSBI10
*
* GET FIRST B-RECORD:
*
         L     R1,LARGER
         MVC   FILPTR-FILSECT(L'FILPTR,R1),FILFIRST-FILSECT(R1)
         MVC   FILSEQ-FILSECT(L'FILSEQ,R1),=F'1'
         BAL   R14,URECLEN         .
         ST    R15,FILCURRL-FILSECT(,R1)
*
* GET LAST A-RECORD:
*
TYP1BGNA EQU   *
         L     R1,SMALLER
         MVC   FILPTR-FILSECT(L'FILPTR,R1),FILLAST-FILSECT(R1)
         MVC   FILSEQ-FILSECT(L'FILSEQ,R1),FILCNT-FILSECT(R1)
         BAL   R14,URECLEN         .
         ST    R15,FILCURRL-FILSECT(,R1)
         EJECT
*---------------------------------------------------------------------*
*        TYPE 1 COMPARISON ALGORITHM                                  *
*                                                                     *
*        -COMPARE A(I) WITH B(J) VIA CLCL                             *
*---------------------------------------------------------------------*
*
* FIRST GENERATE CLCL PTR'S FOR REC-A:
*
TYP1NXTA EQU   *
         L     R2,SMALLER
         L     R0,FILPTR-FILSECT(,R2)
         AL    R0,FILRDLEN-FILSECT(,R2)
         AL    R0,FILFDSP-FILSECT(,R2)
         L     R1,FILCURRL-FILSECT(,R2)
         SL    R1,FILFDSP-FILSECT(,R2)
         NC    FILFLEN-FILSECT(L'FILFLEN,R2),FILFLEN-FILSECT(R2)
         BZ    TYP1RECB
         CL    R1,FILFLEN-FILSECT(,R2)
         BNH   TYP1RECB
         L     R1,FILFLEN-FILSECT(,R2)
*
* THEN GENERATE CLCL PTR'S FOR REC-B:
*
TYP1RECB EQU   *
         L     R2,LARGER
         L     R14,FILPTR-FILSECT(,R2)
         AL    R14,FILRDLEN-FILSECT(,R2)
         AL    R14,FILFDSP-FILSECT(,R2)
         L     R15,FILCURRL-FILSECT(,R2)
         SL    R15,FILFDSP-FILSECT(,R2)
         NC    FILFLEN-FILSECT(L'FILFLEN,R2),FILFLEN-FILSECT(R2)
         BZ    TYP1PAD
         CL    R15,FILFLEN-FILSECT(,R2)
         BNH   TYP1PAD
         L     R15,FILFLEN-FILSECT(,R2)
*
* PAD CHARACTER:
*
TYP1PAD  EQU   *
         ICM   R15,8,COMPPAD
*
* COMPARE:
*
         CLCL  R0,R14
         BNE   TYP1NEXT
*
         BAL   R14,MATCH           PERFORM MATCH PROCESSING
         BNZ   TYP1EXIT            (ERROR)
         EJECT
*---------------------------------------------------------------------*
*        TYPE 1 COMPARISON ALGORITHM                                  *
*                                                                     *
*        -NO MATCH OR MATCH CLEANUP: POINT TO NEXT A(I)               *
*        -IF ALL A(I) PROCESSED, GET NEXT B(J) AND RESTART A-SCAN     *
*---------------------------------------------------------------------*
*
TYP1NEXT EQU   *
         L     R1,SMALLER          A-RCD
         CLC   FILPTR-FILSECT(L'FILPTR,R1),FILFIRST-FILSECT(R1)
         BE    TYP1NEWB            END OF BACKWARD 'A' COMPARE
         BAL   R14,BACKPTR         .
         B     TYP1NXTA            KEEP COMPARING...
*
TYP1NEWB DS    0H
         L     R1,LARGER           B-RCD
         CLC   FILPTR-FILSECT(L'FILPTR,R1),FILLAST-FILSECT(R1)
         BE    TYP1FIN             END OF COMPARE CYCLE...
         BAL   R14,FORWPTR         .
         B     TYP1BGNA            RESTART COMPARE.
*
TYP1FIN  DS    0H
         XR    R15,R15             SHOW OK
*
TYP1EXIT EQU   *
         WHCSBX10 PASS=CC
         EJECT
*---------------------------------------------------------------------*
*        TYPE 2 COMPARISON ALGORITHM                                  *
*                                                                     *
*        -COMPARE B(J) WITH A(N), A(N-1), ... A(1)                    *
*---------------------------------------------------------------------*
*
TYPE2    WHCSBI10
*
* MAINTAIN THROUGHOUT SUBROUTINE:
*
         L     R1,LARGER           B-RCD
         L     R0,FILLRECL-FILSECT(,R1)
         ICM   R2,15,FILFLEN-FILSECT(R1)
         BNZ   TYP21
         LR    R2,R0
*
TYP21    EQU   *
         BCTR  R2,0                MAINTAIN THROUGHOUT FOR 'EX'
         MVC   FILPTR-FILSECT(L'FILPTR,R1),FILFIRST-FILSECT(R1)
         MVC   FILSEQ-FILSECT(L'FILSEQ,R1),=F'1'
         MVC   FILCURRL-FILSECT(L'FILCURRL,R1),FILLRECL-FILSECT(R1)
         L     R14,FILFIRS2-FILSECT(,R1)
*
TYP2BGNA EQU   *
         L     R1,SMALLER
         MVC   FILPTR-FILSECT(L'FILPTR,R1),FILLAST-FILSECT(R1)
         MVC   FILSEQ-FILSECT(L'FILSEQ,R1),FILCNT-FILSECT(R1)
         MVC   FILCURRL-FILSECT(L'FILCURRL,R1),FILLRECL-FILSECT(R1)
         L     R15,FILLAS2-FILSECT(,R1)
         EJECT
*---------------------------------------------------------------------*
*        TYPE 1 COMPARISON ALGORITHM                                  *
*                                                                     *
*        -COMPARE A(I) WITH B(J) VIA EX/CLC                           *
*---------------------------------------------------------------------*
*
* ON EQUAL COMPARE, UPDATE FILE C/B PTR'S
*
TYP2NXTA EQU   *
         EX    R2,TYP2CLC
         BNE   TYP2NEXT
*
         STM   R14,R2,TYP2SAVE     SAVE STATUS
*
         L     R2,LARGER
         SL    R14,FILFDSP-FILSECT(,R2)
         ST    R14,FILPTR-FILSECT(,R2)
         XR    R0,R0
         LR    R1,R14
         S     R1,FILFIRST-FILSECT(,R2)
         D     R0,FILLRECL-FILSECT(,R2)
         LA    R1,1(,R1)
         ST    R1,FILSEQ-FILSECT(,R2)
*
         L     R2,SMALLER
         SL    R15,FILFDSP-FILSECT(,R2)
         ST    R15,FILPTR-FILSECT(,R2)
         XR    R0,R0
         LR    R1,R15
         S     R1,FILFIRST-FILSECT(,R2)
         D     R0,FILLRECL-FILSECT(,R2)
         LA    R1,1(,R1)
         ST    R1,FILSEQ-FILSECT(,R2)
*
         BAL   R14,MATCH           PERFORM MATCH PROCESSING
         BNZ   TYP2EXIT            (ERROR)
*
         LM    R14,R2,TYP2SAVE     RESTORE STATUS
         EJECT
*---------------------------------------------------------------------*
*        TYPE 2 COMPARISON ALGORITHM                                  *
*                                                                     *
*        -NO MATCH OR MATCH CLEANUP: POINT TO NEXT A(I)               *
*        -IF ALL A(I) PROCESSED, GET NEXT B(J) AND RESTART A-SCAN     *
*---------------------------------------------------------------------*
*
TYP2NEXT EQU   *
         L     R1,SMALLER
         CL    R15,FILFIRS2-FILSECT(,R1)
         BE    TYP2NEWB            END OF BACKWARD 'A' COMPARE
         SLR   R15,R0              BACK SPACE A RECORD
         B     TYP2NXTA            KEEP COMPARING...
*
TYP2NEWB DS    0H
         L     R1,LARGER
         CL    R14,FILLAS2-FILSECT(,R1)
         BE    TYP2FIN             END OF COMPARE CYCLE...
         ALR   R14,R0              POINT TO NEXT RECORD
         B     TYP2BGNA            RESTART COMPARE.
*
TYP2FIN  DS    0H
         XR    R15,R15             SHOW OK
*
TYP2EXIT EQU   *
         WHCSBX10 PASS=CC
*
TYP2CLC  CLC   0(0,R14),0(R15)
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: MATCH         (PROCESS RECORD MATCH)             *
*                                                                     *
*        -MATCH: FIND LCS SETS THAT A(I) AND A(I-1) BELONG TO         *
*                IF SAME A(I) BELONGS IN NEXT SET                     *
*---------------------------------------------------------------------*
*
MATCH    WHCSBI10
         L     R1,SMALLER
         L     R1,FILSEQ-FILSECT(,R1)
         BAL   R14,FINDSET
         LR    R2,R15              SAVE SET ID
         SH    R1,=H'1'            SEE IF LAST (I-1) IN SAME SET
         BAL   R14,FINDSET         .
         CLR   R2,R15              ?
         BNE   MATCHFIN            NOT...UNIMPORTANT MATCH.
*
*        TM    OPNTAB+C'1',OPNON
*        BZ    MATCH1
*
*        STM   R0,R15,DIAGSAVE
*        L     R1,SMALLER
*        L     R0,FILSEQ-FILSECT(,R1)
*        L     R1,LARGER
*        L     R14,FILSEQ-FILSECT(,R1)
*        LA    R2,1(,R2)
*
*        LINEDIT TEXT='==> MATCH (A,B): (.....,.....) LCS=.....',      X
               DOT=NO,RENT=YES,MF=(E,LINELIST),                        X
               SUB=(DEC,(R0),DEC,(R14),DEC,(R2))
*
*        LM    R0,R15,DIAGSAVE
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: MATCH         (PROCESS RECORD MATCH)             *
*                                                                     *
*        -MATCH: FIND LCS SETS THAT A(I) AND A(I-1) BELONG TO         *
*                IF SAME A(I) BELONGS IN NEXT SET                     *
*---------------------------------------------------------------------*
*
MATCH1   EQU   *
         LR    R2,R15
         MH    R2,=Y(LCSSECL)      POINT TO INDEX ENTRY
         AL    R2,LCSARRAD         .
         CL    R2,LCSLEVEL         NEW LCS HEIGHT REACHED?
         BL    MATCH2              NO...
*
         MVC   LCSMARK-LCSSECT+LCSSECL(L'LCSMARK,R2),LCSMARK-LCSSECT(R2X
               )
         XC    LCSPTR-LCSSECT+LCSSECL(L'LCSPTR,R2),LCSPTR-LCSSECT+LCSSEX
               CL(R2)
         XC    LCSLAST-LCSSECT+LCSSECL(L'LCSLAST,R2),LCSLAST-LCSSECT+LCX
               SSECL(R2)
         LA    R0,LCSSECL(,R2)
         ST    R0,LCSLEVEL         U/D LAST LCS ENTRY PTR
*
MATCH2   EQU   *
         STH   R1,LCSMARK-LCSSECT(,R2)  MARKER = MARKER - 1
         LA    R2,LCSSECL(,R2)
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: MATCH         (PROCESS RECORD MATCH)             *
*                                                                     *
*        (MATCH: LCS SETS ARE UPDATED)                                *
*                                                                     *
*        -BUILD NODE DATA (EXCEPT FOR BACKCHAIN)                      *
*        -CHAIN TO TREE                                               *
*---------------------------------------------------------------------*
*
         LA    R1,NODESECL         GET NODE STORAGE
         BAL   R14,ALLOC           .
         BZ    MATCHERR            UNAVAILABLE....
         A     R1,NODSTORA         TALLY
         ST    R1,NODSTORA         .
         LR    RNODE,R15           =>
         XC    NODESECT(NODESECL),NODESECT
         L     R1,SMALLER
         MVC   NODESUBA,FILSEQ-FILSECT+L'FILSEQ-L'NODESUBA(R1)
         L     R1,LARGER
         MVC   NODESUBB,FILSEQ-FILSECT+L'FILSEQ-L'NODESUBB(R1)
*
* CHAIN NEW NODE TO LCS INDEX ARRAY (OR LATERAL PTR):
*
         ICM   R1,7,LCSLAST-LCSSECT(R2) U/D ANCHOR IF REQUIRED
         BNZ   MATCH3              .
         STCM  RNODE,7,LCSPTR-LCSSECT(R2)
         STCM  RNODE,7,LCSLAST-LCSSECT(R2)
         B     MATCH4              END UPDATE.
*
MATCH3   EQU   *
         STCM  RNODE,7,NODENEXT-NODESECT(R1)
         STCM  RNODE,7,LCSLAST-LCSSECT(R2) U/D LAST PTR
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: MATCH         (PROCESS RECORD MATCH)             *
*                                                                     *
*        (MATCH: LCS SETS ARE UPDATED)                                *
*                                                                     *
*        -BACK CHAIN NODE JUST ENTERED TO A S(K-1) NODE               *
*         WHOSE I, A(I) IS LARGEST OF ALL BELONGING TO S(K-1)         *
*         BUT LESS THAN I, A(I), JUST ENTERED.                        *
*---------------------------------------------------------------------*
*
* IF LCS IS GREATER THAN ONE, LOOK UP LAST HIT FOR (LCS-1)
* TO CHAIN DOWN STRUCTURE:
*
MATCH4   EQU   *
         CL    R2,LCSL1PTR         LCS 1?
         BE    MATCHFIN            YES, NO BACK CHAIN.
         SH    R2,=Y(LCSSECL)      LOOK AT LCS-1 CHAIN
         LR    R14,RNODE           SAVE CURRENT NODE PTR
         ICM   RNODE,7,LCSPTR-LCSSECT(R2) GET NODE(S)
*
*        TM    OPNTAB+C'3',OPNON
*        BZ    MATCH4B
*
*        LR    R15,RLCS
*        S     R15,LCSARRAD
*        XR    R14,R14
*        D     R14,=A(LCSSECL)
*        LR    R14,R15
*
*        LINEDIT TEXT='NODE CHAIN STARTS AT ...... (LCS .....)',       X
               DOT=NO,MF=(E,LINELIST),RENT=YES,                        X
               SUB=(HEX,(RNODE),DEC,(R14))
*
MATCH4B  EQU   *
*
MATCH5   EQU   *
*        TM    OPNTAB+C'3',OPNON
*        BZ    MATCH5B
*
*        STM   R0,R15,DIAGSAVE
*        XR    R0,R0
*        ICM   R0,3,NODESUBA
*        L     R1,SMALLER
*        L     R14,FILSEQ-FILSECT(,R1)
*
*        LINEDIT TEXT='COMPARE NODE A(.....) : FILE A(.....)',         X
               DOT=NO,RENT=YES,MF=(E,LINELIST),                        X
               SUB=(DEC,(R0),DEC,(R14))
*
*        LM    R0,R15,DIAGSAVE
*
MATCH5B  EQU   *
         L     R1,SMALLER
         CLC   NODESUBA,FILSEQ-FILSECT+L'FILSEQ-L'NODESUBA(R1)
         BL    MATCH7              POSSIBLE PREDECESSOR? ... YES
         ICM   RNODE,7,NODENEXT    INSPECT NEXT NODE
         BNZ   MATCH5B             GOT ONE...
         B     ABEND3              NO - UNEXPECTED
*
MATCH7   DS    0H
         STCM  RNODE,7,NODEBACK-NODESECT(R14)
*
MATCHFIN EQU   *
         XR    R15,R15             SHOW SUCCESSFUL PROCESS
         B     MATCHX              END
*
MATCHERR DS    0H
         LA    R15,4               SHOW ERROR
*
MATCHX   EQU   *
         WHCSBX10 PASS=CC
         EJECT
*---------------------------------------------------------------------*
*        PRINT A DETAIL LINE                                          *
*---------------------------------------------------------------------*
*
PRINT    WHCSBI10
         XR    R0,R0               TEST IF NEW PAGE NEEDED
         IC    R0,PCC              .
         A     R0,LINCNT           .
         C     R0,LINLIM           .
         BNH   PRINT2              . NOT NEEDED
         TM    PROCESS2,PRO2PFTR   PRINT FOOTERS?
         BZ    PRINT1A             NO...
         BAL   R14,FOOTCON         YES...
*
PRINT1A  EQU   *
         OI    PROCESS2,PRO2PFTR   PRINT FOOTERS SUBSEQUENTLY
         LA    R0,1                COUNT PAGES
         A     R0,PAGCOUNT         .
         ST    R0,PAGCOUNT         .
         MVI   SVPAG,$BLANK
         MVC   SVPAG+1(L'SVPAG-1),SVPAG
         LA    R1,SVPAG            EDIT
         BAL   R14,EDITNO          .
*
         WHCDDF11 LLOC=PAGLIT,LLEN=L'PAGLIT,RLOC=SVPAG,RLEN=L'SVPAG,   X
               OLOC=HPAG,OPNS=(L,R),CB=DDFCB
*
         PUT   SYSLOG,H1A
*
         PUT   SYSLOG,H1B
*
         PUT   SYSLOG,H1C
*
         L     R0,=A(H1D)
         PUT   SYSLOG,(0)
         EJECT
*---------------------------------------------------------------------*
*        PRINT A DETAIL LINE                                          *
*---------------------------------------------------------------------*
*
         PUT   SYSLOG,H2
*
         PUT   SYSLOG,H3
*
         PUT   SYSLOG,H4
         EJECT
*---------------------------------------------------------------------*
*        PRINT A DETAIL LINE                                          *
*---------------------------------------------------------------------*
*
         TM    PROCESS,PROCPRTD    PRINTING DATA?
         BZ    PRINT1B             NO...
*
         PUT   SYSLOG,H6           ISSUE DATA SUBHEADINGS
*
*        PUT   SYSLOG,H7
*
PRINT1B  EQU   *
         MVC   LINCNT,CURHLINS     UPDATE LINE COUNT
         MVI   PCC,$SPACE1         SPACE ONE NEXT LINE
*
PRINT2   EQU   *
         BAL   R14,PRINTUNC        PRINT RECORD
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        PRINT A LINE UNCONDITIONALLY                                 *
*        (DO NOT TEST LINE COUNTER)                                   *
*---------------------------------------------------------------------*
*
PRINTUNC WHCSBI10
         XR    R0,R0               TALLY LINES
         IC    R0,PCC              .
         A     R0,LINCNT           .
         ST    R0,LINCNT           .
         TR    PCC,=C'+ 0-'
*
         PUT   SYSLOG,PLINE        OUTPUT RECORD
*
         MVI   PCC,$SPACE1         CLEAR
         MVI   PDATA,$BLANK
         MVC   PDATA+1(L'PDATA-1),PDATA
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*           VALIDATE AND ENCODE A NUMERIC STRING                      *
*            ...WITH TRAILING BLANKS                                  *
*                                                                     *
*            R1 => STRING START ADDR                                  *
*---------------------------------------------------------------------*
*
CHECKNUM WHCSBI10
         LR    R15,R1
         LA    R1,8(,R15)
         TRT   0(8,R15),SCANB
         LR    R14,R1
         SR    R14,R15
         BZ    CHECKNUX
         BCTR  R14,0
         EX    R14,CHECKNI1
         BNZ   CHECKNUX
         EX    R14,CHECKNI2
         CVB   R15,DW
         B     CHECKNR
*
CHECKNUX DS    0H
         LH    R15,=H'-1'
*
CHECKNR  EQU   *
         WHCSBX10 PASS=CC
*
CHECKNI1 TRT   0(0,R15),NUMTAB
CHECKNI2 PACK  DW,0(0,R15)
         EJECT
*---------------------------------------------------------------------*
*        SET HIGHEST RETURN CODE PASSED                               *
*---------------------------------------------------------------------*
*
SETRCODE WHCSBI10
         C     R1,RETURNC          KEEP HIGHER R/C PASSED
         BNH   SETRCODX            .
         ST    R1,RETURNC          .
*
SETRCODX EQU   *
         WHCSBX10                     .
         EJECT
*---------------------------------------------------------------------*
*        READ AND STORE COMPARAND FILE DATA                           *
*                                                                     *
*        -ISSUE GET                                                   *
*        -SAVE RCD LEN/ADDR                                           *
*        -TALLY                                                       *
*---------------------------------------------------------------------*
*
READCMP  WHCSBI10
*
READCMP1 EQU   *
         WHCSBR10 R1               RESTORE FILCB PTR
         L     R2,FILDCBA-FILSECT(,R1) DCB ADDR
*
         GET   (R2)
*
         LR    R15,R1              COPY RCD PTR
         LH    R14,DCBLRECL-IHADCB(,R2)
         TM    DCBRECFM-IHADCB(R2),DCBRECU
         BO    READCMP2
         TM    DCBRECFM-IHADCB(R2),DCBRECV
         BZ    READCMP2
         SH    R14,=Y($RDWLEN)
         LA    R15,$RDWLEN(,R15)
*
READCMP2 EQU   *
         WHCSBR10 R1               RESTORE FILCB PTR
         ST    R15,FILCURRA-FILSECT(,R1) RCD PTR
         ST    R14,FILCURRL-FILSECT(,R1) AND LENGTH
         LA    R15,1               COUNT
         A     R15,FILCNT-FILSECT(,R1)
         ST    R15,FILCNT-FILSECT(,R1)
         EJECT
*---------------------------------------------------------------------*
*        READ AND STORE COMPARAND FILE DATA                           *
*                                                                     *
*        -CHECK THAT RECORD LENGTH NOT LESS THAN COLUMN BEGIN         *
*---------------------------------------------------------------------*
*
         CLC   FILCURRL-FILSECT(L'FILCURRL,R1),FILFDSP-FILSECT(R1)
         BH    READCMP0
*
         MVC   PMSG(L'MSGFLNX1),MSGFLNX1
         L     R0,FILCNT-FILSECT(,R1)
         LA    R1,PMSG+L'MSGFLNX1
         BAL   R14,EDITNO
         MVC   0(L'MSGFLNX2,R15),MSGFLNX2
         LA    R15,L'MSGFLNX2(,R15)
         WHCSBR10 R1               RESTORE FILCB PTR
         MVC   0(L'FILDESCR,R15),FILDESCR-FILSECT(R1)
         LA    R1,L'FILDESCR(,R15)
         TRT   0(L'FILDESCR,R15),SCANB
         MVC   0(L'MSGFLNX3,R1),MSGFLNX3
         BAL   R14,PRINT
         LA    R1,XFILERR
         BAL   R14,SETRCODE
         OI    PROCESS,PROCLERR
         B     READCMP1
*
MSGFLNX1 DC    C'&WHRN.016E RECORD ('
MSGFLNX2 DC    C') IN '
MSGFLNX3 DC    C' FILE IS SHORTER THAN COLUMN SPECIFICATION'
         EJECT
*---------------------------------------------------------------------*
*        READ AND STORE COMPARAND FILE DATA                           *
*                                                                     *
*        -COMPUTE STORAGE REQ'D, TALLY, AND ALLOCATE                  *
*                                                                     *
*                                                                     *
*---------------------------------------------------------------------*
*
READCMP0 DS    0H
         LR    R2,R1               FILCB PTR
         L     R1,FILCURRL-FILSECT(,R2) GET AMOUNT OF STORAGE FOR RCD
         A     R1,FILRDLEN-FILSECT(,R2) (PUT R/D FRONT AND BACK)
         A     R1,FILRDLEN-FILSECT(,R2)
         LR    R0,R1               TALLY TOTAL REQUIRED
         A     R0,FILSTORR-FILSECT(,R2)
         ST    R0,FILSTORR-FILSECT(,R2)
         BAL   R14,ALLOC           .
         BZ    READCMP1            (OUT OF STORAGE - DO NOT STORE)
         ST    R15,FILLAST-FILSECT(,R2) SAVE ADDRESS
         CLC   =F'1',FILCNT-FILSECT(R2) U/D FIRST FIELD IF REQ'D
         BNE   READCMP3            .
         ST    R15,FILFIRST-FILSECT(,R2)
*
READCMP3 EQU   *
         L     R1,FILRDLEN-FILSECT(,R2) BUILD RCD DESCR BYTES IF REQ'D
         SH    R1,=H'1'            .
         BM    READCMP4            .
         LA    R14,FILCURRL+L'FILCURRL-FILSECT(R2) BUILD 1ST R/D
         S     R14,FILRDLEN-FILSECT(,R2)
         EX    R1,READCMPR         .
         A     R15,FILRDLEN-FILSECT(,R2)
         A     R15,FILCURRL-FILSECT(,R2)
         EX    R1,READCMPR         .
*
READCMP4 EQU   *
         L     R14,FILLAST-FILSECT(,R2) GET ALLOCATION PTR
         A     R14,FILRDLEN-FILSECT(,R2)
         L     R15,FILCURRL-FILSECT(,R2) GET LENGTH
         LR    R1,R15              COPY FOR MVCL
         L     R0,FILCURRA-FILSECT(,R2)
         MVCL  R14,R0
         B     READCMP1
*
READCMPR MVC   0(0,R15),0(R14)     BUILD 1ST R/D
         EJECT
*---------------------------------------------------------------------*
*        READ AND STORE COMPARAND FILE DATA                           *
*                                                                     *
*        END -OF FILE:                                                *
*                                                                     *
*        -BUILD FIELDS FOR TYPE 2 ALGORITHM IN CASE WE USE IT         *
*---------------------------------------------------------------------*
*
READCMPE DS    0H
         WHCSBR10 R1               RESTORE FILCB PTR
         L     R0,FILFIRST-FILSECT(,R1) UPDATE TEST 'START' FIELD
         A     R0,FILFDSP-FILSECT(,R1)  FOR ALG 2
         ST    R0,FILFIRS2-FILSECT(,R1)
         L     R0,FILLAST-FILSECT(,R1) UPDATE TEST 'LAST' FIELD
         A     R0,FILFDSP-FILSECT(,R1) FOR ALG 2
         ST    R0,FILLAS2-FILSECT(,R1)
         EJECT
*---------------------------------------------------------------------*
*        READ AND STORE COMPARAND FILE DATA                           *
*                                                                     *
*        END -OF FILE:                                                *
*                                                                     *
*        -IF NO ERRORS READING COMPARAND DATA, RETURN 0               *
*        -OTHERWISE, RETURN 4                                         *
*---------------------------------------------------------------------*
*
         MVC   PMSG(L'MSGRCNT1),MSGRCNT1
         MVC   PMSG+L'MSGRCNT1(L'FILDESCR),FILDESCR-FILSECT(R1)
         LA    R1,PMSG+L'MSGRCNT1+L'FILDESCR
         TRT   PMSG+L'MSGRCNT1(L'FILDESCR),SCANB
         MVC   0(L'MSGRCNT2,R1),MSGRCNT2
         LR    R2,R1               SAVE EDIT CURSOR
         WHCSBR10 R1               RESTORE FILCB PTR
         L     R0,FILCNT-FILSECT(,R1)
         LA    R1,L'MSGRCNT2(,R2)  NEW CURSOR
         BAL   R14,EDITNO
         MVC   0(L'MSGRCNT3,R15),MSGRCNT3
         BAL   R14,PRINT
*
         MVC   PMSG(L'MSGISRQ1),MSGISRQ1
         WHCSBR10 R1               RESTORE FILCB PTR
         MVC   PMSG+L'MSGISRQ1(L'FILDESCR),FILDESCR-FILSECT(R1)
         LA    R1,PMSG+L'MSGISRQ1+L'FILDESCR
         TRT   PMSG+L'MSGISRQ1(L'FILDESCR),SCANB
         MVC   0(L'MSGISRQ2,R1),MSGISRQ2
         LR    R2,R1               SAVE CURSOR
         WHCSBR10 R1               RESTORE FILCB PTR
         L     R0,FILSTORR-FILSECT(,R1)
         AH    R0,=Y($K-1)
         SRL   R0,10
         LA    R1,L'MSGISRQ2(,R2)
         BAL   R14,EDITNO
         MVC   0(L'MSGISRQ3,R15),MSGISRQ3
         BAL   R14,PRINT
*
READCMPX EQU   *
         WHCSBX10
*
MSGRCNT1 DC    C'&WHRN.017I '
MSGRCNT2 DC    C' FILE CONTAINS '
MSGRCNT3 DC    C' RECORD(S)'
*
MSGISRQ1 DC    C'&WHRN.018I STORAGE REQUIRED FOR '
MSGISRQ2 DC    C' FILE DATA: '
MSGISRQ3 DC    C'K'
         EJECT
*---------------------------------------------------------------------*
*        OPEN  COMPARAND FILE                                         *
*                                                                     *
*        -ISSUE RDJFCB TO GET DSN / CHECK FOR ERROR                   *
*        -ISSUE OPEN / CHECK FOR ERROR                                *
*---------------------------------------------------------------------*
*
OPENCMP  WHCSBI10
*
* BUILD RDJFCB JFCB PTR:
*
         MVI   EXITWORD,X'87'
         LA    R0,JFCBSTOR
         STCM  R0,7,EXITWORD+1
         L     R2,FILDCBA-FILSECT(,R1)
         MVC   FILDDNAM-FILSECT(L'FILDDNAM,R1),DCBDDNAM-IHADCB(R2)
         LA    R0,EXITWORD
         STCM  R0,7,DCBEXLSA-IHADCB(R2)
*
         WHCLST10 ((R2),$OPENI),CB=LIST,SVC=$SVCRDJ
*
         LTR   R15,R15             CHECK RDJFCB R/C
         BNZ   OPENCMPF
*
* RESET EXIT LIST PTR AND OPEN:
*
         XC    DCBEXLSA-IHADCB(L'DCBEXLSA,R2),DCBEXLSA-IHADCB(R2)
*
         WHCLST10 ((R2),$OPENI),CB=LIST,SVC=$SVCOPN
*
         TM    DCBOFLGS-IHADCB(R2),DCBOFOPN CHK SUCCESSFUL OPEN FLAGS
         BZ    OPENCMPF
         EJECT
*---------------------------------------------------------------------*
*        OPEN  COMPARAND FILE                                         *
*                                                                     *
*        -DETERMINE WHAT LENGTH RECORD DESCRIPTOR FIELD TO USE        *
*         (0 FOR RECFM F FILES)                                       *
*---------------------------------------------------------------------*
*
* DECIDE WHAT LENGTH BYTE TO USE (0 ASSUMED):
*
         TM    DCBRECFM-IHADCB(R2),DCBRECU
         BO    OPENCRFU            RECFM U...
         TM    DCBRECFM-IHADCB(R2),DCBRECF
         BO    OPENCRFF            FIXED = 0
         LH    R0,DCBLRECL-IHADCB(,R2) RECFM V...
         SH    R0,=Y($RDWLEN)
         WHCSBR10 R1               RESTORE FILCB PTR
         ST    R0,FILLRECL-FILSECT(,R1)
         MVC   FILRDLEN-FILSECT(L'FILRDLEN,R1),=F'2'
         CL    R0,=F'255'
         BH    OPENCMSG
         MVC   FILRDLEN-FILSECT(L'FILRDLEN,R1),=F'1'
         B     OPENCMSG
*
OPENCRFF DS    0H
         WHCSBR10 R1               RESTORE FILCB PTR
         LH    R0,DCBLRECL-IHADCB(,R2)
         ST    R0,FILLRECL-FILSECT(,R1)
         B     OPENCMSG
*
OPENCRFU DS    0H
         WHCSBR10 R1               RESTORE FILCB PTR
         LH    R0,DCBBLKSI-IHADCB(,R2) RECFM U...
         ST    R0,FILLRECL-FILSECT(,R1)
         MVC   FILRDLEN-FILSECT(L'FILRDLEN,R1),=F'2'
         CL    R0,=F'255'
         BH    OPENCMSG
         MVC   FILRDLEN-FILSECT(L'FILRDLEN,R1),=F'1'
         EJECT
*---------------------------------------------------------------------*
*        OPEN  COMPARAND FILE                                         *
*                                                                     *
*        -ISSUE INFORMATIVE MESSAGES                                  *
*---------------------------------------------------------------------*
*
OPENCMSG EQU   *
         MVC   FILDSN-FILSECT(L'FILDSN,R1),JFCBDSNM SAVE COPY OF DSN
         MVC   PMSG(L'MSGIDSN1),MSGIDSN1
         MVC   PMSG+L'MSGIDSN1(L'FILDESCR),FILDESCR-FILSECT(R1)
         LA    R1,PMSG+L'MSGIDSN1+L'FILDESCR
         TRT   PMSG+L'MSGIDSN1(L'FILDESCR),SCANB
         MVC   0(L'MSGIDSN2,R1),MSGIDSN2
         LA    R15,L'MSGIDSN2(,R1)
         WHCSBR10 R1               RESTORE FILCB PTR
         MVC   0(L'FILDDNAM,R15),FILDDNAM-FILSECT(R1)
         LA    R1,L'FILDDNAM(,R15)
         TRT   0(L'FILDDNAM,R15),SCANB
         MVC   0(L'MSGIDSN3,R1),MSGIDSN3
         LA    R15,L'MSGIDSN3(,R1)
*
         WHCDSN11 SLOC=JFCBDSNM,OLOC=(R15),CB=DSNCB,PRMA=PARMADDR
*
         MVC   0(L'MSGIDSN4,R15),MSGIDSN4
         BAL   R14,PRINT
         WHCSBR10 R1               RESTORE FILCB FOR DISPDCB
         LA    R0,FILDDNAM-FILSECT(,R1)
         L     R1,FILDCBA-FILSECT(,R1)
         BAL   R14,DISPDCB
         XR    R15,R15
         B     OPENCMPX
*
MSGIDSN1 DC    C'&WHRN.019I DATASET NAME OF '
MSGIDSN2 DC    C' FILE ('
MSGIDSN3 DC    C') IS '''
MSGIDSN4 DC    C''''
         EJECT
*---------------------------------------------------------------------*
*        OPEN  COMPARAND FILE                                         *
*                                                                     *
*        -OPEN FAILED / ISSUE MESSAGE AND RETURN CODE                 *
*---------------------------------------------------------------------*
*
OPENCMPF DS    0H
         MVC   PMSG(L'MSGOERR1),MSGOERR1
         WHCSBR10 R1               RESTORE FILCB PTR
         MVC   PMSG+L'MSGOERR1(L'FILDDNAM),FILDDNAM-FILSECT(R1)
         LA    R1,PMSG+L'MSGOERR1+L'FILDDNAM
         TRT   PMSG+L'MSGOERR1(L'FILDDNAM),SCANB
         MVC   0(L'MSGOERR2,R1),MSGOERR2
         BAL   R14,PRINT
         LA    R1,XOPENERR
         BAL   R14,SETRCODE
         LA    R15,4
*
OPENCMPX EQU   *
         WHCSBX10 PASS=CC
*
MSGOERR1 DC    C'&WHRN.020E ERROR OPENING DDNAME ('
MSGOERR2 DC    C')'
         EJECT
*---------------------------------------------------------------------*
*        CLOSE COMPARAND FILES                                        *
*---------------------------------------------------------------------*
*
CLOSCMP  WHCSBI10
         L     R1,FILDCBA-FILSECT(,R1)
         BAL   R14,CLOSDCB
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        EDIT NUMERIC STRING                                          *
*---------------------------------------------------------------------*
*
EDITNO   WHCSBI10
         CVD   R0,DW               EDIT NUMERIC FIELD
         LR    R2,R1               SAVE START ADDR
         MVC   EDITWORK,=XL12'402020202020202020202120'
         LA    R1,EDITWORK+L'EDITWORK-1
         EDMK  EDITWORK,DW+2       .
         BNM   EDITNO1             .
         BCTR  R1,0                (-)
         MVI   0(R1),C'-'          .
*
EDITNO1  EQU   *
         LA    R15,EDITWORK+L'EDITWORK-1
         SLR   R15,R1              .
         EX    R15,EDITNOMV        BUILD NUMBER
         LA    R15,1(R2,R15)       NEXT LOCATION
         WHCSBX10 PASS=R15            RETURN.
*
EDITNOMV MVC   0(0,R2),0(R1)       MOVE VAR LEN FIELD
         EJECT
*---------------------------------------------------------------------*
*        ALLOCATE STORAGE FROM POOL                                   *
*---------------------------------------------------------------------*
*
ALLOC    WHCSBI10
         LR    R0,R1               TOTAL REQUESTS
         A     R0,ALSTORRQ         .
         ST    R0,ALSTORRQ         .
*
         TM    PROCESS,PROCSTOX    STORAGE EXHAUST?
         BO    ALLOCEXH            YES...
*
         LR    R0,R1               MAKE COPY OF AMT REQUESTED
         A     R0,ALSTORU          TOTAL USED TO-DATE
         C     R0,ALSTORL          .GT. WHAT WE HAVE?
         BH    ALLOCEXH            YES---FINIS.
         L     R15,ALSTORU         COPY FOR NEW ADDRESS
         ST    R0,ALSTORU          NEW 'USED'
         A     R15,ALSTORA         PASS BACK LOC'N
         B     ALLOCX
*
ALLOCEXH DS    0H
         OI    PROCESS,PROCSTOX    SHOW EXHAUST
         XR    R15,R15             PASS BACK R/C
*
ALLOCX   EQU   *
         WHCSBX10 PASS=CC
         EJECT
*---------------------------------------------------------------------*
*        FREE DCB BUFFERS                                             *
*---------------------------------------------------------------------*
*
FREEPOOL WHCSBI10
         FREEPOOL (1)
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        FIND SET THAT 'I' OF A(I) IS IN                              *
*---------------------------------------------------------------------*
*
FINDSET  WHCSBI10
         STCM  R1,3,BSARG
         MVC   $BSCARRA,LCSARRAD   BUILD SEARCH PARAM'S
         L     R1,LCSLEVEL         GET COUNT
         S     R1,LCSARRAD         .
         XR    R0,R0               .
         D     R0,=A(LCSSECL)      .
         AH    R1,=H'1'            .
         ST    R1,$BSCARRC         .
*
         WHCBSC10 ARRL=LCSSECL,KEYD=LCSMARK-LCSSECT,                   X
               KEYL=L'LCSMARK,ARGL=L'BSARG,ARGA=BSARG,                 X
               CB=BSCCB
*
         B     *+4(R15)
         B     FINDSGOT            ARGUMENT FOUND
         B     FINDSLOW            ARG N/F - LOWER ENTRY PASSED BACK
         B     FINDSNF             NOT FOUND
         B     ABEND2
         EJECT
*---------------------------------------------------------------------*
*        FIND SET THAT 'I' OF A(I) IS IN                              *
*---------------------------------------------------------------------*
*
FINDSGOT DS    0H
         L     R15,$BSCFNDA        GET ADDRESS AND CONVERT TO LCS LEN
         B     FINDSCAL            .
*
FINDSLOW DS    0H
         L     R15,$BSCFNDA        GET ADDRESS AND CONVERT TO LCS LEN
         LA    R15,LCSSECL(,R15)
*
FINDSCAL EQU   *
         S     R15,LCSARRAD
         XR    R14,R14
         D     R14,=A(LCSSECL)
         B     FINDSETX
*
FINDSNF  DS    0H
         XR    R15,R15
*
FINDSETX EQU   *
         WHCSBX10 PASS=R15
         EJECT
*---------------------------------------------------------------------*
*        ISSUE STORAGE STATISTICS                                     *
*---------------------------------------------------------------------*
*
STORSTAT WHCSBI10
         MVC   PMSG(L'MSGSTOS1),MSGSTOS1
         L     R0,ALSTORL
         SRL   R0,10
         LA    R1,PMSG+L'MSGSTOS1
         BAL   R14,EDITNO
         MVC   0(L'MSGSTOS2,R15),MSGSTOS2
         L     R0,ALSTORRQ
         AH    R0,=Y($K-1)
         SRL   R0,10
         LA    R1,L'MSGSTOS2(,R15)
         BAL   R14,EDITNO
         MVC   0(L'MSGSTOS3,R15),MSGSTOS3
         SRDL  R0,32
         MH    R1,=H'100'
         D     R0,ALSTORLK
         LR    R0,R1
         LA    R1,L'MSGSTOS3(,R15)
         BAL   R14,EDITNO
         MVC   0(L'MSGSTOS4,R15),MSGSTOS4
         BAL   R14,PRINT
         WHCSBX10
*
MSGSTOS1 DC    C'&WHRN.021I STORAGE ALLOCATED: '
MSGSTOS2 DC    C'K --- LAST STORAGE REQUEST TOTAL: '
MSGSTOS3 DC    C'K ('
MSGSTOS4 DC    C'% USED)'
         EJECT
*---------------------------------------------------------------------*
*        ISSUE SEVERITY LEVEL MESSAGE                                 *
*---------------------------------------------------------------------*
*
SEVMSG   WHCSBI10
         MVC   PMSG(L'MSGSEV1),MSGSEV1
         L     R0,RETURNC
         LA    R1,PMSG+L'MSGSEV1
         BAL   R14,EDITNO
         MVC   0(L'MSGSEV2,R15),MSGSEV2
         BAL   R14,PRINT
         WHCSBX10
*
MSGSEV1  DC    C'&WHRN.022I HIGHEST SEVERITY LEVEL SET WAS '
MSGSEV2  DC    C' '
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: LNSMSG                                           *
*                                                                     *
*        ISSUE LINES-PER-PAGE MESSAGE                                 *
*---------------------------------------------------------------------*
*
LNSMSG   WHCSBI10
         MVC   PMSG(L'MSG024A),MSG024A
         L     R0,LINLIM
         LA    R1,PMSG+L'MSG024A
         BAL   R14,EDITNO
         BAL   R14,PRINT
         WHCSBX10
*
MSG024A  DC    C'&WHRN.024I LINE LIMIT: '
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: STRMSG                                           *
*                                                                     *
*        ISSUE STRIP CHARACTER MESSAGE                                *
*---------------------------------------------------------------------*
*
STRMSG   WHCSBI10
         MVC   PMSG(L'MSG031A),MSG031A
         MVC   PMSG+L'MSG031A(L'$PPLSTR),$PPLSTR
         BAL   R14,PRINT
         WHCSBX10
*
MSG031A  DC    C'&WHRN.031I STRIP CHARACTER SPECIFIED: '
         EJECT
*---------------------------------------------------------------------*
*        ISSUE NODE STORAGE STATISTICS                                *
*---------------------------------------------------------------------*
*
NODESTAT WHCSBI10
         MVC   PMSG(L'MSGNSTO1),MSGNSTO1
         L     R0,NODSTORA
         AH    R0,=Y($K-1)
         SRL   R0,10
         LA    R1,PMSG+L'MSGNSTO1
         BAL   R14,EDITNO
         MVC   0(L'MSGNSTO2,R15),MSGNSTO2
         BAL   R14,PRINT
         WHCSBX10
*
MSGNSTO1 DC    C'&WHRN.023I LAST NODE STORAGE REQUEST TOTAL: '
MSGNSTO2 DC    C'K'
         EJECT
*---------------------------------------------------------------------*
*        ISSUE LCS LENGTH MESSAGE                                     *
*---------------------------------------------------------------------*
*
LCSLMSG  WHCSBI10
         MVC   PMSG(L'MSGLCSL1),MSGLCSL1
         L     R0,STATLCSL
         LA    R1,PMSG+L'MSGLCSL1
         BAL   R14,EDITNO
         MVC   0(L'MSGLCSL2,R15),MSGLCSL2
         BAL   R14,PRINT
         NC    STATLCSL,STATLCSL
         BZ    LCSLMSGX
         MVC   PMSG(L'MSGLCSC1),MSGLCSC1
         L     R0,STATLCSN
         LA    R1,PMSG+L'MSGLCSC1
         BAL   R14,EDITNO
         MVC   0(L'MSGLCSC2,R15),MSGLCSC2
         BAL   R14,PRINT
*
LCSLMSGX EQU   *
         WHCSBX10
*
MSGLCSL1 DC    C'&WHRN.025I LARGEST SEQUENCE OF RECORDS THAT MATCH IN BX
               OTH FILES IS '
MSGLCSL2 DC    C' '
*
MSGLCSC1 DC    C'&WHRN.026I THERE ARE '
MSGLCSC2 DC    C' DIFFERENT SEQUENCES OF THIS LENGTH'
         EJECT
*---------------------------------------------------------------------*
*        ADJUST STORAGE PTRS TO ALLOCATE NEXT REQUEST ON DW           *
*---------------------------------------------------------------------*
*
ALIGN    WHCSBI10
         L     R1,ALSTORU          GET STORAGE USED
         AH    R1,=H'7'            ROUND UP TO DW
         SRL   R1,3                .
         SLL   R1,3                .
         S     R1,ALSTORU          .
         BZ    ALIGNX
         BAL   R14,ALLOC
*
ALIGNX   EQU   *
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        BUILD DSN IN LOCATION POINTED TO BY R1                       *
*---------------------------------------------------------------------*
*
DSN      WHCSBI10
         LR    R2,R1               => OUTPUT LOCATION
*
         WHCDSN11 SLOC=(R0),OLOC=(R2),CB=DSNCB,PRMA=PARMADDR
*
         WHCSBX10 PASS=R15
         EJECT
*---------------------------------------------------------------------*
*        BUILD PAIRSTACK FROM LCS WHOSE LAST NODE ENTRY               *
*        IS PASSED IN R1                                              *
*---------------------------------------------------------------------*
*
BPAIR    WHCSBI10
         L     R2,PAIRSAD          => LST ENTRY IN STACK
         A     R2,PAIRSLN          .
         SH    R2,=Y(PAIRSECL)     .
*
BPAIR2   EQU   *
         MVC   PAIRA-PAIRSECT(L'PAIRA,R2),NODESUBA-NODESECT+L'NODESUBA-X
               L'PAIRA(R1)
         MVC   PAIRB-PAIRSECT(L'PAIRB,R2),NODESUBB-NODESECT+L'NODESUBB-X
               L'PAIRB(R1)
*
         TM    PROCESS,PROCCSWP    A/B SWAPPED?
         BZ    BPAIR2B             NO...
*
         XC    PAIRA-PAIRSECT(L'PAIRA,R2),PAIRB-PAIRSECT(R2)
         XC    PAIRB-PAIRSECT(L'PAIRB,R2),PAIRA-PAIRSECT(R2)
         XC    PAIRA-PAIRSECT(L'PAIRA,R2),PAIRB-PAIRSECT(R2)
*
BPAIR2B  EQU   *
*        TM    OPNTAB+C'2',OPNON
*        BZ    BPAIR3
*        STM   R0,R15,DIAGSAVE
*        XR    R0,R0
*        ICM   R0,3,NODESUBA-NODESECT(R1)
*        XR    R2,R2
*        ICM   R2,3,NODESUBB-NODESECT(R1)
*
*        LINEDIT TEXT='==> BUILD PAIR (A,B): (.....,.....)',           X
               DOT=NO,RENT=YES,MF=(E,LINELIST),                        X
               SUB=(DEC,(R0),DEC,(R2))
*
*        LM    R0,R15,DIAGSAVE
*
BPAIR3   EQU   *
         SH    R2,=Y(PAIRSECL)     BACKWARDS THROUGH STACK
         ICM   R1,7,NODEBACK-NODESECT(R1)
         BNZ   BPAIR2
*
BPAIRX   EQU   *
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        GENERATE LISTING OF CHANGED RECORDS                          *
*                                                                     *
*        -PRIME FILE C/B'S TO POINT AT FIRST RECORD                   *
*        -TAKE APPROPRIATE PATH FOR LCS LENGTH 0 OR NOT 0             *
*                                                                     *
*        R1: LENGTH OF LCS TO BE PROCESSED                            *
*---------------------------------------------------------------------*
*
GENLIST  WHCSBI10
         ST    R1,GENLLCS          SAVE ARGUMENT
         LTR   R1,R1               LCS LEN 0?
         BZ    GENL0               YES --- DO NOT BUILD PAIRS.
         MH    R1,=Y(LCSSECL)
         A     R1,LCSARRAD
         ICM   R1,7,LCSPTR-LCSSECT(R1)
         BAL   R14,BPAIR
*
GENL0    EQU   *
         MVC   LINCNT,LINLIM       FORCE NEW PAGE
         NI    PROCESS2,255-(PRO2PFTR) NO FOOTERS 1ST TIME
         MVI   H4BGN,C'-'          NEW DETAIL SUBHEADING
         MVC   H4BGN+1(H4LEN-1),H4BGN
         MVC   HC1LIT,C1LIT        .
         MVC   HC2LIT,C2LIT        .
         LA    R0,FILDSN-FILSECT+CFIL1 BUILD DSN'S IN SUBHEADING
         LA    R1,HC1DSN           .
         BAL   R14,DSN             .
         MVC   0(L'DSNTRML,R15),DSNTRML
         LA    R0,FILDSN-FILSECT+CFIL2
         LA    R1,HC2DSN           .
         BAL   R14,DSN             .
         MVC   0(L'DSNTRML,R15),DSNTRML
         MVC   CURHLINS,=F'14'
*
         WHCDDF11 LLOC=REPLIT,LLEN=L'REPLIT,RLOC=REP020,RLEN=L'REP020, X
               OLOC=HREP,OPNS=(L,R),CB=DDFCB
*
         OI    PROCESS,PROCPRTD    SHOW DATA BEING PRINTED
         MVC   FILPTR-FILSECT+CFIL1,FILFIRST-FILSECT+CFIL1
         MVC   FILSEQ-FILSECT+CFIL1,=F'1'
         MVC   FILPTR-FILSECT+CFIL2,FILFIRST-FILSECT+CFIL2
         MVC   FILSEQ-FILSECT+CFIL2,=F'1'
         LA    R1,CFIL1
         BAL   R14,URECLEN
         ST    R15,FILCURRL-FILSECT+CFIL1
         LA    R1,CFIL2
         BAL   R14,URECLEN
         ST    R15,FILCURRL-FILSECT+CFIL2
*
         NC    GENLLCS,GENLLCS     LCS 0?
         BZ    GENL2I              YES...
         EJECT
*---------------------------------------------------------------------*
*        GENERATE LISTING OF CHANGED RECORDS                          *
*                                                                     *
*        -POINT TO BEGINNING OF PAIRS ARRAY                           *
*        -GENERATE INSERT/DELETES                                     *
*---------------------------------------------------------------------*
*
         MVC   PAIRSPTR,PAIRSAD    START AT BEGINNING
*
GENL1    EQU   *
         L     R2,PAIRSPTR         CHECK FOR DELETIONS FROM BASE
         XR    R1,R1               GET FIRST COMMON FILE-1 #
         ICM   R1,3,PAIRB-PAIRSECT(R2)
         BAL   R14,GENINS          GENERATE INSERT LISTING
         ICM   R1,3,PAIRA-PAIRSECT(R2)
         BAL   R14,GENDEL          GENERATE DELETE LISTING
*
* NOW WE HAVE PROCESSED ALL GAPS LEADING UP TO COMMON SET OF
* RECORDS, SO PUBLISH COMMON SET:
*
         TM    OPNTAB+C'L',OPNON   (IF LIST OPTION)
         BZ    GENL1B              .
         LA    R1,CFIL1            INSERT COMMON DATA (CFIL1/2 SAME)
         BAL   R14,MOVEDATA        .
         BAL   R14,EDSEQ1          .
         BAL   R14,EDSEQ2          .
         BAL   R14,PRINT           .
         OI    PROCESS2,PRO2UPDL   SHOW ENTRY IN LISTING
*
* U/D RECORDS PTR'S AND GET NEXT SET OF COMMON RECORDS:
*
GENL1B   EQU   *
         LA    R1,CFIL1            UPDATE FILE PTR'S
         BAL   R14,FORWPTR         .
         LA    R1,CFIL2            .
         BAL   R14,FORWPTR         .
         LA    R2,PAIRSECL         GET NEXT SET OF COMMON RECORDS
         A     R2,PAIRSPTR         .
         ST    R2,PAIRSPTR         .
         C     R2,PAIRSBND         .
         BL    GENL1               .
         EJECT
*---------------------------------------------------------------------*
*        GENERATE LISTING OF CHANGED RECORDS                          *
*                                                                     *
*        -GENERATE CHANGE LISTING FOR RECORDS PASSED LAST COMMON      *
*         SET                                                         *
*---------------------------------------------------------------------*
*
GENL2I   EQU   *
         CLC   FILSEQ-FILSECT+CFIL2,FILCNT-FILSECT+CFIL2
         BH    GENL2D              FILE 2 COMPLETELY PROCESSED
         LA    R1,1                PUSH LIMIT 1 OVER RECORD CNT
         A     R1,FILCNT-FILSECT+CFIL2
         BAL   R14,GENINS          GENERATE INSERT'S
*
GENL2D   EQU   *
         CLC   FILSEQ-FILSECT+CFIL1,FILCNT-FILSECT+CFIL1
         BH    GENL3               FILE 1 COMPLETELY PROCESSED
         LA    R1,1                PUSH LIMIT 1 OVER RECORD CNT
         A     R1,FILCNT-FILSECT+CFIL1
         BAL   R14,GENDEL          GENERATE DELETE'S
*
GENL3    EQU   *
         TM    PROCESS2,PRO2UPDL   UPDATE LISTING EMPTY?
         BZ    GENLX               YES...
         BAL   R14,FOOTEND
*
GENLX    EQU   *
         NI    PROCESS,255-PROCPRTD RESET DATA PRINT INDICATOR
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        GENERATE 'DELETES' LISTING FOR RECORDS STARTING              *
*        WITH THAT CURRENTLY POINTED TO BY CFIL1 UP TO                *
*        BUT NOT INCLUDING THAT WHOSE SEQUENCE # IS PASSED IN R1      *
*---------------------------------------------------------------------*
*
GENDEL   WHCSBI10
         XC    RCDPRBLK,RCDPRBLK
         LR    R0,R1               SAVE COPY
         C     R1,FILSEQ-FILSECT+CFIL1
         BE    GENDELX
         LA    R1,1                COUNT DEL BLKS
         A     R1,STATDBLK         .
         ST    R1,STATDBLK         .
         BAL   R14,BGNDLBOX
         MVC   DEL1,FILSEQ-FILSECT+CFIL1
*
GENDEL2  EQU   *
         MVC   DEL2,FILSEQ-FILSECT+CFIL1
         LA    R1,CFIL1            INSERT DATA IN DELETE BOX
         BAL   R14,INSDLBOX        .
         BAL   R14,FORWPTR         UPDATE REC PTR FOR FILE 1
         LA    R14,1               COUNT DEL RCDS
         A     R14,STATDRCD        .
         ST    R14,STATDRCD        .
         LA    R14,1               COUNT RCDS IN BLK
         A     R14,RCDPRBLK        .
         ST    R14,RCDPRBLK        .
         C     R0,FILSEQ-FILSECT+CFIL1
         BNE   GENDEL2
         BAL   R14,ENDDLBOX
*
         MVC   UPDDEL,UDELLIT      BUILD UPDATE DELETE RECORD
         L     R0,DEL1
         LA    R1,UPDDEL+L'UPDDEL
         BAL   R14,EDITNO
         L     R0,DEL2
         C     R0,DEL1
         BE    GENDEL3
         MVI   0(R15),C','
         LA    R1,1(,R15)
         BAL   R14,EDITNO
*
GENDEL3  EQU   *
         BAL   R14,WRITLBN
         L     R1,UPDRECAD         BUILD UPDATE RCD AND ISSUE
         LA    R0,$DLRDLEN
         SLL   R0,2*8
         STCM  R0,15,$DLRFRDW-$DLRREC(R1)
         MVI   $DLRFTY-$DLRREC(R1),$DLRFTYD
         MVC   $DLRDSQ1-$DLRREC(L'$DLRDSQ1,R1),DEL1
         MVC   $DLRDSQ2-$DLRREC(L'$DLRDSQ2,R1),DEL2
         BAL   R14,WRITUPD
*
GENDELX  EQU   *
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: GENINS                                           *
*                                                                     *
*        PROCESS INSERT BLOCK FOR RECORDS STARTING                    *
*        WITH THAT CURRENTLY POINTED TO BY CFIL2 UP TO                *
*        BUT NOT INCLUDING THAT WHOSE SEQUENCE # IS PASSED IN R1      *
*---------------------------------------------------------------------*
*
GENINS   WHCSBI10
         XC    RCDPRBLK,RCDPRBLK   RESET RCD PER BLOCK
         C     R1,FILSEQ-FILSECT+CFIL2
         BE    GENINSX
         LA    R1,1                COUNT INS BLKS
         A     R1,STATIBLK         .
         ST    R1,STATIBLK         .
         BAL   R14,BGNINBOX
*
*--------BUILD LIBRARIAN FILE INSERT RECORD---------------------------*
         MVC   UPDINS,UINSLIT      BUILD UPDATE INSERT RECORD
         L     R0,FILSEQ-FILSECT+CFIL1
         SH    R0,=H'1'
         ST    R0,INS1             SAVE
         BZ    GENINS0
         LA    R1,UPDINS+L'UPDINS
         BAL   R14,EDITNO
         BAL   R14,WRITLBN
         B     GENINS2
GENINS0  DS    0H
         MVC   UPDINS+L'UPDINS(L'LIBFIRST),LIBFIRST
         BAL   R14,WRITLBN
*---------------------------------------------------------------------*
*
*
*--------BUILD UPDATE FILE INSERT RECORD------------------------------*
GENINS2  EQU   *
         L     R1,UPDRECAD
         LA    R0,$DLRILEN
         SLL   R0,2*8
         STCM  R0,15,$DLRFRDW-$DLRREC(R1)
         MVI   $DLRFTY-$DLRREC(R1),$DLRFTYI
         MVC   $DLRISQ1-$DLRREC(L'$DLRDSQ1,R1),INS1
         BAL   R14,WRITUPD
*---------------------------------------------------------------------*
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: GENINS (CONTINUED)                               *
*                                                                     *
*        PROCESS TEXT RECORD IN AN INSERT BLOCK                       *
*---------------------------------------------------------------------*
*
GENINS2B EQU   *
         LA    R1,CFIL2            INSERT DATA IN LISTING
         BAL   R14,INSINBOX        .
*
*--------BUILD TEXT RECORD FOR LIBRARIAN FILE-------------------------*
         LA    R1,L'UPDREC         BUILD DATA INTO UPDATE REC
         C     R1,FILCURRL-FILSECT+CFIL2
         BNH   GENINS3
         L     R1,FILCURRL-FILSECT+CFIL2
GENINS3  EQU   *
         BCTR  R1,0
         L     R14,FILPTR-FILSECT+CFIL2
         A     R14,FILRDLEN-FILSECT+CFIL2
         EX    R1,GENINSM
         BAL   R14,WRITLBN
*---------------------------------------------------------------------*
*
*--------BUILD INSERT TEXT FOR UPDATE FILE----------------------------*
         L     R0,FILCURRL-FILSECT+CFIL2 GET TEXT LEN/ADDR
         L     R1,FILPTR-FILSECT+CFIL2   .
         A     R1,FILRDLEN-FILSECT+CFIL2 .
         TM    PROCESS2,PRO2STRP         STRIP?
         BZ    GENINS3A                  NO.
         BAL   R14,STRIPT                YES---DO SO
         LR    R0,R15                    COPY NEW LEN
*
GENINS3A EQU   *
         L     R2,UPDRECAD               BUILD REC
         LA    R14,$DLRTFXL              . LEN BYTE
         AR    R14,R0                    .
         SLL   R14,2*8                   .
         STCM  R14,15,$DLRFRDW-$DLRREC(R2)
         MVI   $DLRFTY-$DLRREC(R2),$DLRFTYT
         LR    R14,R1                    . MVCL TEXT
         LR    R15,R0                    .
         LA    R0,$DLRTREC-$DLRREC(,R2)  .
         LR    R1,R15                    .
         MVCL  R0,R14                    .
         BAL   R14,WRITUPD               WRITE UPD REC
*---------------------------------------------------------------------*
         EJECT
*---------------------------------------------------------------------*
*        GENERATE 'INSERTS' LISTING    (CONTINUED)                    *
*---------------------------------------------------------------------*
*
         LA    R1,CFIL2            U/D REC PTR
         BAL   R14,FORWPTR         .
         LA    R14,1               COUNT INS RCDS
         A     R14,STATIRCD        .
         ST    R14,STATIRCD        .
         LA    R14,1               COUNT RCD PER BLOK
         A     R14,RCDPRBLK        .
         ST    R14,RCDPRBLK        .
         WHCSBR10 R1               RESTORE UPPER BND
         C     R1,FILSEQ-FILSECT+CFIL2
         BNE   GENINS2B
         BAL   R14,ENDINBOX
*
GENINSX  EQU   *
         WHCSBX10
*
GENINSM  MVC   UPDREC(0),0(R14)
         EJECT
*---------------------------------------------------------------------*
*        START UP AN 'INSERT' BOX                                     *
*---------------------------------------------------------------------*
*
BGNINBOX WHCSBI10
         BAL   R14,KBRES           GET MIN 'LINES REMAINING' REQUIRED
         A     R15,LINCNT
         C     R15,LINLIM
         BL    BGNINB1
         ST    R15,LINCNT
*
BGNINB1  EQU   *
*        MVI   PCC,$SPACE2
         TM    OPNTAB+C'M',OPNON   ACTION MSG?
         BZ    BGNINB2             NO...
         MVC   PMSG(L'INSMSG),INSMSG
         LR    R0,R1               EDIT BLK # IN MSG
         LA    R1,PMSG+L'INSMSG
         BAL   R14,EDITNO
         MVC   0(L'INSMSG2,R15),INSMSG2
         BAL   R14,PRINT           PRINT LINE
*
BGNINB2  EQU   *
         LA    R1,$BXCH            BUILD BOX TOP
         BAL   R14,CBOXTOP
*
BGNINBX  EQU   *
         WHCSBX10
*
INSMSG   DC    C'===> INSERTION '
INSMSG2  DC    C':'
         EJECT
*---------------------------------------------------------------------*
*        START UP A 'DELETE' BOX                                      *
*---------------------------------------------------------------------*
*
BGNDLBOX WHCSBI10
         BAL   R14,KBRES           GET MIN 'LINES REMAINING' REQUIRED
         A     R15,LINCNT
         C     R15,LINLIM
         BL    BGNDLB1
         ST    R15,LINCNT
*
BGNDLB1  EQU   *
*        MVI   PCC,$SPACE2
         TM    OPNTAB+C'M',OPNON   ACTION MSG?
         BZ    BGNDLB2             NO...
         MVC   PMSG(L'DELMSG),DELMSG
         LR    R0,R1               EDIT BLK # IN MSG
         LA    R1,PMSG+L'DELMSG
         BAL   R14,EDITNO
         MVC   0(L'DELMSG2,R15),DELMSG2
         BAL   R14,PRINT           PRINT LINE
*
BGNDLB2  EQU   *
         LA    R1,$BXCH            BUILD BOX TOP IF REQUIRED
         BAL   R14,CBOXTOP         .
*
BGNDLBX  EQU   *
         WHCSBX10
*
DELMSG   DC    C'===> DELETION '
DELMSG2  DC    C':'
         EJECT
*---------------------------------------------------------------------*
*        END AN 'INSERT' BOX                                          *
*---------------------------------------------------------------------*
*
ENDINBOX WHCSBI10
         LA    R1,$BXCH            BOX BOTTOM IF NECESSARY
         BAL   R14,CBOXBOT
         TM    OPNTAB+C'M',OPNON   ACTION MSG?
         BZ    ENDINB2             NO...
         MVC   PMSG(L'ENDINMSG),ENDINMSG
         L     R0,STATIBLK
         LA    R1,PMSG+L'ENDINMSG
         BAL   R14,EDITNO
         MVC   0(L'ENDINMS2,R15),ENDINMS2
         L     R0,RCDPRBLK
         LA    R1,L'ENDINMS2(,R15)
         BAL   R14,EDITNO
         MVC   0(L'ENDINMS3,R15),ENDINMS3
         BAL   R14,PRINT           PRINT LINE
*
ENDINB2  EQU   *
*        MVI   PCC,$SPACE2
*
ENDINBX  EQU   *
         WHCSBX10
*
ENDINMSG DC    C'===> END INSERTION '
ENDINMS2 DC    C', RECORD(S) = '
ENDINMS3 DC    C' '
         EJECT
*---------------------------------------------------------------------*
*        END A 'DELETE' BOX                                           *
*---------------------------------------------------------------------*
*
ENDDLBOX WHCSBI10
         LA    R1,$BXCH            BOX BOTTOM IF NECESSARY
         BAL   R14,CBOXBOT
         TM    OPNTAB+C'M',OPNON   ACTION MSG?
         BZ    ENDDLB2             NO...
         MVC   PMSG(L'ENDDLMSG),ENDDLMSG
         L     R0,STATDBLK
         LA    R1,PMSG+L'ENDDLMSG
         BAL   R14,EDITNO
         MVC   0(L'ENDDLMS2,R15),ENDDLMS2
         L     R0,RCDPRBLK
         LA    R1,L'ENDDLMS2(,R15)
         BAL   R14,EDITNO
         MVC   0(L'ENDDLMS3,R15),ENDDLMS3
         BAL   R14,PRINT           PRINT LINE
*
ENDDLB2  EQU   *
*        MVI   PCC,$SPACE2
*
ENDDLBX  EQU   *
         WHCSBX10
*
ENDDLMSG DC    C'===> END DELETION '
ENDDLMS2 DC    C', RECORD(S) = '
ENDDLMS3 DC    C' '
         EJECT
*---------------------------------------------------------------------*
*        INSERT A DATA LINE IN AN 'INSERT' BOX                        *
*---------------------------------------------------------------------*
*
INSINBOX WHCSBI10
*LINEDIT TEXT='LINCNT: ..... AT SEQ: ......',                          X
               SUB=(DECA,LINCNT,DECA,FILSEQ-FILSECT+CFIL2),RENT=NO
         BAL   R14,KRES            GET RESERVED LINES
         A     R15,LINCNT          .
         C     R15,LINLIM          END BOX AND RESTART
         BL    INSINB1             .
*
         LA    R1,$BXCH            YES, END BOX IF REQUIRED
         BAL   R14,CBOXBOT         .
         TM    OPNTAB+C'M',OPNON   ISSUING MESSAGES?
         BZ    INSINBA
         MVC   PMSG(L'NINSMSG),NINSMSG
         BAL   R14,PRINT
*
INSINBA  EQU   *
         MVC   LINCNT,LINLIM       FORCE TOF
         TM    OPNTAB+C'M',OPNON   MSGS?
         BZ    INSINB0             NO...
         MVC   PMSG(L'CINSMSG),CINSMSG
         L     R0,STATIBLK
         LA    R1,PMSG+L'CINSMSG
         BAL   R14,EDITNO
         MVC   0(L'CINSMSG2,R15),CINSMSG2
         BAL   R14,PRINT
*
INSINB0  EQU   *
         LA    R1,$BXCH            RESTART BOX IF REQUIRED
         BAL   R14,CBOXTOP         .
*
INSINB1  EQU   *
         LA    R1,CFIL2            MOVE DATA IN PRINT LINE
         BAL   R14,MOVEDATA        .
         BAL   R14,EDSEQ2          .
         BAL   R14,CCOL            COLUMNS IF NECESSARY
         BAL   R14,PRINT           PRINT LINE
         OI    PROCESS2,PRO2UPDL   SHOW ENTRY IN LISTING
         WHCSBX10
*
CINSMSG  DC    C'===> INSERTION '
CINSMSG2 DC    C', CONTINUED:'
NINSMSG  DC    C'===> INSERTED RECORDS CONTINUED ON NEXT PAGE'
         EJECT
*---------------------------------------------------------------------*
*        INSERT A DATA LINE IN AN 'DELETE' BOX                        *
*---------------------------------------------------------------------*
*
INSDLBOX WHCSBI10
         BAL   R14,KRES            GET RESERVED LINES
         A     R15,LINCNT          .
         C     R15,LINLIM          END BOX AND RESTART
         BL    INSDLB1             .
*
         LA    R1,$BXCH            YES, END BOX
         BAL   R14,CBOXBOT         .
         TM    OPNTAB+C'M',OPNON   ISSUING MESSAGES?
         BZ    INSDLBA
         MVC   PMSG(L'NDELMSG),NDELMSG
         BAL   R14,PRINT
*
INSDLBA  EQU   *
         MVC   LINCNT,LINLIM       FORCE TOF
         TM    OPNTAB+C'M',OPNON   MSGS?
         BZ    INSDLB0             NO...
         MVC   PMSG(L'CDELMSG),CDELMSG
         L     R0,STATDBLK
         LA    R1,PMSG+L'CDELMSG
         BAL   R14,EDITNO
         MVC   0(L'CDELMSG2,R15),CDELMSG2
         BAL   R14,PRINT
*
INSDLB0  EQU   *
         LA    R1,$BXCH            RESTART BOX IF REQUIRED
         BAL   R14,CBOXTOP         .
*
INSDLB1  EQU   *
         LA    R1,CFIL1            MOVE DATA IN PRINT LINE
         BAL   R14,MOVEDATA        .
         BAL   R14,EDSEQ1          .
         BAL   R14,CCOL            COLUMNS IF NECESSARY
         BAL   R14,PRINT           PRINT LINE
         OI    PROCESS2,PRO2UPDL   SHOW ENTRY IN LISTING
         WHCSBX10
*
CDELMSG  DC    C'===> DELETION '
CDELMSG2 DC    C', CONTINUED:'
NDELMSG  DC    C'===> DELETED RECORDS CONTINUED ON NEXT PAGE'
         EJECT
*---------------------------------------------------------------------*
*        UPDATE RECORD POINTER IN FILE C/B                            *
*                                                                     *
*        ROUTINE POINTS FILE C/B TO NEXT RECORD IN BUFFER,            *
*        UPDATING: FILPTR                                             *
*                  FILSEQ                                             *
*                  FILCURRL                                           *
*---------------------------------------------------------------------*
*
FORWPTR  WHCSBI10
         L     R14,FILPTR-FILSECT(,R1)
         A     R14,FILCURRL-FILSECT(,R1)
         A     R14,FILRDLEN-FILSECT(,R1)
         A     R14,FILRDLEN-FILSECT(,R1)
         ST    R14,FILPTR-FILSECT(,R1)
         LA    R0,1
         A     R0,FILSEQ-FILSECT(,R1)
         ST    R0,FILSEQ-FILSECT(,R1)
         BAL   R14,URECLEN
         ST    R15,FILCURRL-FILSECT(,R1)
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        BACKSPACE THE RECORD POINTER IN A FILE C/B                   *
*                                                                     *
*        ROUTINE POINTS FILE C/B TO PREV RECORD IN BUFFER,            *
*        UPDATING: FILPTR                                             *
*                  FILSEQ                                             *
*                  FILCURRL                                           *
*---------------------------------------------------------------------*
*
BACKPTR  WHCSBI10
         L     R14,FILPTR-FILSECT(,R1)
         S     R14,FILRDLEN-FILSECT(,R1)
         XR    R15,R15
         L     R2,FILRDLEN-FILSECT(,R1)
         SLL   R2,2
         LA    R2,BACKINSR(R2)
         EX    0,0(,R2)
         ST    R15,FILCURRL-FILSECT(,R1)
         SR    R14,R15
         S     R14,FILRDLEN-FILSECT(,R1)
         ST    R14,FILPTR-FILSECT(,R1)
         L     R2,FILSEQ-FILSECT(,R1)
         BCTR  R2,0
         ST    R2,FILSEQ-FILSECT(,R1)
         WHCSBX10
*
BACKINSR L     R15,FILLRECL-FILSECT(,R1)
         ICM   R15,1,0(R14)
         ICM   R15,3,0(R14)
         EJECT
*---------------------------------------------------------------------*
*        UPDATE RECORD LENGTH FIELD FOR RECORD => BY FILE C/B         *
*                                                                     *
*        R1: FILE CONTROL BLOCK                                       *
*---------------------------------------------------------------------*
*
URECLEN  WHCSBI10
         XR    R15,R15
         L     R14,FILPTR-FILSECT(,R1)
         L     R2,FILRDLEN-FILSECT(,R1)
         SLL   R2,2
         LA    R2,URECINSR(R2)
         EX    0,0(,R2)
         WHCSBX10 PASS=R15
*
URECINSR L     R15,FILLRECL-FILSECT(,R1)
         ICM   R15,1,0(R14)
         ICM   R15,3,0(R14)
         EJECT
*---------------------------------------------------------------------*
*        BUILD BOX COLUMNS CONDITIONALLY                              *
*---------------------------------------------------------------------*
*
CCOL     WHCSBI10
         TM    PROCESS,PROCBOX
         BZ    CCOLX
         L     R2,LBOXPTR
         MVC   0(L'$BXCV,R2),$BXCV
         L     R2,RBOXPTR
         MVC   0(L'$BXCV,R2),$BXCV
*
CCOLX    EQU   *
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        GENERATE BOX TOP CONDITIONALLY                               *
*                                                                     *
*        R1 ==> HORIZONTAL RULE CHARACTER                             *
*---------------------------------------------------------------------*
*
CBOXTOP  WHCSBI10
         TM    PROCESS,PROCBOX     BOXING?
         BZ    CBOXTOPX            NO...
         BAL   R14,BOXTOP          .
         BAL   R14,PRINT           PRINT LINE
*
CBOXTOPX EQU   *
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        BUILD BOX TOP                                                *
*                                                                     *
*        R1 => HORIZONTAL RULE CHARACTER                              *
*---------------------------------------------------------------------*
*
BOXTOP   WHCSBI10
         L     R2,LBOXPTR
         MVC   0(L'$BXCUL,R2),$BXCUL BUILD BOX
         L     R14,RBOXPTR         .
         MVC   0(L'$BXCUR,R14),$BXCUR
         SR    R14,R2              .
         SH    R14,=H'2'           .
         EX    R14,BOXTOPZ         .
         EX    R14,BOXTOPT         .
*
BOXTOPX  EQU   *
         WHCSBX10
*
BOXTOPZ  XC    1(0,R2),1(R2)
BOXTOPT  TR    1(0,R2),0(R1)
         EJECT
*---------------------------------------------------------------------*
*        GENERATE BOX BOTTOM CONDITIONALLY                            *
*                                                                     *
*        R1 ==> HORIZONTAL RULE CHARACTER                             *
*---------------------------------------------------------------------*
*
CBOXBOT  WHCSBI10
         TM    PROCESS,PROCBOX     BOXING?
         BZ    CBOXBOTX            NO...
         BAL   R14,BOXBOT          .
         BAL   R14,PRINT           PRINT LINE
*
CBOXBOTX EQU   *
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        BUILD BOX BOTTOM                                             *
*                                                                     *
*        R1 => HORIZONTAL RULE CHARACTER                              *
*---------------------------------------------------------------------*
*
BOXBOT   WHCSBI10
         L     R2,LBOXPTR
         MVC   0(L'$BXCLL,R2),$BXCLL
         L     R14,RBOXPTR         .
         MVC   0(L'$BXCLR,R14),$BXCLR
         SR    R14,R2              .
         SH    R14,=H'2'           .
         EX    R14,BOXBOTZ         .
         EX    R14,BOXBOTT         .
*
BOXBOTX  EQU   *
         WHCSBX10
*
BOXBOTZ  XC    1(0,R2),1(R2)
BOXBOTT  TR    1(0,R2),0(R1)
         EJECT
*---------------------------------------------------------------------*
*        MOVE DATA OF CURRENT RECORD IN FILE C/B TO PRINT LINE        *
*                                                                     *
*        R1 ==> FILE C/B                                              *
*---------------------------------------------------------------------*
*
MOVEDATA WHCSBI10
         L     R14,PRECPTR         MOVE AS MUCH OF DATA INTO PLINE
         L     R15,FILPTR-FILSECT(,R1) AS POSSIBLE
         A     R15,FILRDLEN-FILSECT(,R1)
         L     R2,FILCURRL-FILSECT(,R1)
         C     R2,LARPRINT         .
         BNH   MOVE3               .
         L     R2,LARPRINT         .
*
MOVE3    EQU   *
         BCTR  R2,0                .
         EX    R2,MOVEMOVE         .
         TM    OPNTAB+C'U',OPNON   XLATE TO UPPERCASE IF REQUIRED
         BZ    MOVE4               .
         L     R15,=A(UPPERCAS)    .
         EX    R2,MOVEUPPR         .
*
MOVE4    EQU   *
         L     R15,=A(XLATE)       TRANSLATE UNPRINTABLES
         EX    R2,MOVETRAN         .
         WHCSBX10
*
MOVEMOVE MVC   0(0,R14),0(R15)
MOVEUPPR TR    0(0,R14),0(R15)
MOVETRAN TR    0(0,R14),0(R15)
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: WRITLBN                                          *
*                                                                     *
*        WRITE LIBRARIAN UPDATE RECORD                                *
*---------------------------------------------------------------------*
*
WRITLBN  WHCSBI10
         PUT   SYSLBN,UPDREC
*
         LA    R0,1                COUNT
         A     R0,LBNCNT           .
         ST    R0,LBNCNT           .
         MVI   UPDREC,$BLANK       CLEAR
         MVC   UPDREC+1(L'UPDREC-1),UPDREC
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: WRITUPD                                          *
*                                                                     *
*        WRITE UPDATE RECORD                                          *
*---------------------------------------------------------------------*
*
WRITUPD  WHCSBI10
         L     R0,UPDRECAD
         PUT   SYSUPD,(0)
*
         LA    R0,1                COUNT
         A     R0,UPDCNT           .
         ST    R0,UPDCNT           .
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        EDIT SEQUENCE NUMBER 1                                       *
*---------------------------------------------------------------------*
*
EDSEQ1   WHCSBI10
         L     R0,FILSEQ-FILSECT+CFIL1
         CVD   R0,DW
         MVC   WORK6,=XL6'402020202120'
         ED    WORK6,DW+5
         L     R2,PSEQPTR
         MVC   SEQ1-SEQSECT(L'SEQ1,R2),WORK6+L'WORK6-L'SEQ1
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        EDIT SEQUENCE NUMBER 2                                       *
*---------------------------------------------------------------------*
*
EDSEQ2   WHCSBI10
         L     R0,FILSEQ-FILSECT+CFIL2
         CVD   R0,DW
         MVC   WORK6,=XL6'402020202120'
         ED    WORK6,DW+5
         L     R2,PSEQPTR
         MVC   SEQ2-SEQSECT(L'SEQ2,R2),WORK6+L'WORK6-L'SEQ2
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        CALCULATE # LINES THAT NEED TO BE RESERVED AT BOTTOM         *
*        OF PAGE WHEN BUILDING BOXES                                  *
*---------------------------------------------------------------------*
*
KRES     WHCSBI10
         XR    R15,R15             START WITH 0
         TM    OPNTAB+C'M',OPNON   ISSUING MESSAGES?
         BZ    KRES2               NO...
         LA    R15,1(,R15)
*
KRES2    EQU   *
         TM    PROCESS,PROCBOX     BOXING?
         BZ    KRESX               NO...
         LA    R15,1(,R15)
*
KRESX    EQU   *
         WHCSBX10 PASS=R15
         EJECT
*---------------------------------------------------------------------*
*        CALCULATE # LINES THAT NEED TO BE REMAINING BEFORE           *
*        STARTING UP BOX, IN ORDER TO PREVENT GENERATING              *
*        EMPTY BOXES                                                  *
*---------------------------------------------------------------------*
*
KBRES    WHCSBI10
         XR    R15,R15             CLEAR
         TM    OPNTAB+C'M',OPNON   ISSUING MESSAGES?
         BZ    KBRES2              NO...
         LA    R15,2(,R15)         -1STMSG (LASTMSG MAY APPEAR LATER)
*
KBRES2   EQU   *
         TM    PROCESS,PROCBOX     BOXING?
         BZ    KBRESX              NO...
         LA    R15,2(,R15)         -BOXTOP -BOXBOTTOM
*
KBRESX   EQU   *
         WHCSBX10 PASS=R15
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: FOOTEND                                          *
*                                                                     *
*        WRITE OUT END-FOOTER LINES                                   *
*---------------------------------------------------------------------*
*
FOOTEND  WHCSBI10
         PUT   SYSLOG,FOOTR1
         PUT   SYSLOG,FOOTR1E
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: FOOTCON                                          *
*                                                                     *
*        WRITE OUT CONTINUATION-FOOTER LINES                          *
*---------------------------------------------------------------------*
*
FOOTCON  WHCSBI10
         PUT   SYSLOG,FOOTR1
         PUT   SYSLOG,FOOTR1C
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: DISPDCB                                          *
*                                                                     *
*        DISPLAY DCB ATTRIBUTES                                       *
*                                                                     *
*        (I) R0:  DDNAME                                              *
*            R1:  DCB                                                 *
*---------------------------------------------------------------------*
*
DISPDCB  WHCSBI10
         MVC   PMSG(L'MSG028I1),MSG028I1
         LR    R2,R0               SAVE DDNAME PTR
         LR    R14,R1              SAVE DCB    PTR
         MVC   PMSG+L'MSG028I1(L'DCBDDNAM),0(R2)
         LA    R1,PMSG+L'MSG028I1+L'DCBDDNAM
         TRT   PMSG+L'MSG028I1(L'DCBDDNAM),SCANB
         MVC   0(L'MSG028I2,R1),MSG028I2
         LR    R2,R14              USE R2 FOR DCB
         LA    R15,L'MSG028I2(,R1) NEXT BYTE IN MSG
*
         WHCRFM11 SLOC=DCBRECFM-IHADCB(,R2),OLOC=(R15),CB=RFMCB
*
         MVC   0(L'MSG028I3,R15),MSG028I3
         LA    R1,L'MSG028I3(,R15)
         LH    R0,DCBLRECL-IHADCB(,R2)
         BAL   R14,EDITNO
         MVC   0(L'MSG028I4,R15),MSG028I4
         LA    R1,L'MSG028I4(,R15)
         LH    R0,DCBBLKSI-IHADCB(,R2)
         BAL   R14,EDITNO
         BAL   R14,PRINT
         WHCSBX10 PASS=R15
*
MSG028I1 DC    C'&WHRN.028I DCB DATA FOR ('
MSG028I2 DC    C'): RECFM='
MSG028I3 DC    C' LRECL='
MSG028I4 DC    C' BLKSIZE='
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: STRIPT                                           *
*                                                                     *
*        GET LENGTH OF STRING AFTER TRAILING CHAR'S STRIPPED          *
*                                                                     *
*        (I) R0:  STRING LENGTH                                       *
*            R1:  STRING ADDR                                         *
*        (O) R15: LENGTH OF STRING WITH TRAILING CHAR'S REMOVED       *
*---------------------------------------------------------------------*
*
STRIPT   WHCSBI10
         LR    R15,R0              TAIL CURSOR (BYTE FOLLOWING SCANSEG)
         AR    R15,R1              .
*
STRIPT2  EQU   *
         LR    R14,R0              GET LESSER OF LEN REM AND 256
         CH    R14,=Y(L'REVWORK)   .
         BNH   STRIPT3             .
         LH    R14,=Y(L'REVWORK)   .
STRIPT3  EQU   *
         SR    R15,R14             GET ADDR OF NEXT SOURCE SEG
         LA    R2,REVTAB+L'REVTAB  POINT TO APPROP DISP IN MASK
         SR    R2,R14              .
         BCTR  R14,0               FOR EX
         EX    R14,STRIPTMV        BUILD REV MASK
         EX    R14,STRIPTTR        REVERSE STRING
         LA    R1,REVWORK+1(R14)   SCAN FOR NON-TRAILCHR
         EX    R14,STRIPTSC        .
         LA    R14,1(,R14)         RESTORE SCAN LEN
         LA    R2,REVWORK          GET # TRAILCHR FOUND
         SR    R1,R2               .
         CR    R1,R14              SCAN ENTIRE STR?
         BNE   STRIPT4             NO--THRU.
         SR    R15,R1              NEW TAIL CURSOR
         SR    R0,R1               NEW LEN REMAINING
         BNZ   STRIPT2             > 0--RESTART SCAN.
         LA    R15,1               REC ALL TRAILCHR---PASS BACK 1.
         B     STRIPTX             .
*
STRIPTMV MVC   REVWORK(0),0(R2)
STRIPTTR TR    REVWORK(0),0(R15)
STRIPTSC TRT   REVWORK(0),TRAILTAB
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: STRIPT (CONTINUED)                               *
*                                                                     *
*        GET LENGTH OF STRING AFTER TRAILING CHAR'S STRIPPED          *
*---------------------------------------------------------------------*
*
STRIPT4  DS    0H
         AR    R15,R14             TAIL CURSOR + SCAN ATTEMPT LEN
         SR    R15,R1                          - MINUS TRAILCHR FND
         WHCSBR10 R1                           - SCAN START ADDR
         SR    R15,R1              GIVES LEN - TRAILCHR
*
STRIPTX  EQU   *
         WHCSBX10 PASS=R15
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: CLOSLOG                                          *
*                                                                     *
*        CLOSE LOG AND HISTORY FILES                                  *
*---------------------------------------------------------------------*
*
CLOSLOG  WHCSBI10
         LA    R1,SYSLOG
         BAL   R14,CLOSDCB
         LA    R1,SYSHST
         BAL   R14,CLOSDCB
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: CLOSUPD                                          *
*                                                                     *
*        CLOSE UPDATE AND LIBRARIAN FILES                             *
*---------------------------------------------------------------------*
*
CLOSUPD  WHCSBI10
         LA    R1,SYSUPD
         BAL   R14,CLOSDCB
         LA    R1,SYSLBN
         BAL   R14,CLOSDCB
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        SUBROUTINE: CLOSDCB                                          *
*                                                                     *
*        CLOSE DCB                                                    *
*                                                                     *
*        (I) R1:  DCB ADDR                                            *
*---------------------------------------------------------------------*
*
CLOSDCB  WHCSBI10
         LR    R2,R1               COPY DCB PTR
*
         WHCLST10 ((R2),$CLOSD),CB=LIST,SVC=$SVCCLS
*
         LR    R1,R2               FREE BUFFERS
         BAL   R14,FREEPOOL        .
         WHCSBX10
         EJECT
*---------------------------------------------------------------------*
*        DCB EXIT FOR STATS FILE                                      *
*                                                                     *
*        FORCE RECFM=F LRECL/BLKSIZE FOR STT IF NO DCB ATTR SPECIFIED *
*---------------------------------------------------------------------*
*
DCBXSTW  DC    0A(0),X'85',AL3(DCBXST)
*
DCBXST   DS    0H
         NC    DCBRECFM-IHADCB(L'DCBRECFM,R1),DCBRECFM-IHADCB(R1)
         BNZR  R14
         NC    DCBBLKSI-IHADCB(L'DCBBLKSI,R1),DCBBLKSI-IHADCB(R1)
         BNZR  R14
         NC    DCBLRECL-IHADCB(L'DCBLRECL,R1),DCBLRECL-IHADCB(R1)
         BNZR  R14
         OI    DCBRECFM-IHADCB(R1),DCBRECF
         MVC   DCBLRECL-IHADCB(L'DCBLRECL,R1),=Y(STATRECL)
         MVC   DCBBLKSI-IHADCB(L'DCBBLKSI,R1),=Y(STATRECL)
         BR    R14
         EJECT
*---------------------------------------------------------------------*
*        DCB EXIT FOR PRINT FILES                                     *
*                                                                     *
*        FORCE RECFM=F LRECL/BLKSIZE FOR PRT IF NO DCB ATTR SPECIFIED *
*---------------------------------------------------------------------*
*
DCBXPRW  DC    0A(0),X'85',AL3(DCBXPR)
*
DCBXPR   DS    0H
         NC    DCBRECFM-IHADCB(L'DCBRECFM,R1),DCBRECFM-IHADCB(R1)
         BNZR  R14
         NC    DCBBLKSI-IHADCB(L'DCBBLKSI,R1),DCBBLKSI-IHADCB(R1)
         BNZR  R14
         NC    DCBLRECL-IHADCB(L'DCBLRECL,R1),DCBLRECL-IHADCB(R1)
         BNZR  R14
         OI    DCBRECFM-IHADCB(R1),DCBRECF
         MVC   DCBLRECL-IHADCB(L'DCBLRECL,R1),=Y(L'PLINE)
         MVC   DCBBLKSI-IHADCB(L'DCBBLKSI,R1),=Y(L'PLINE)
         BR    R14
         EJECT
*---------------------------------------------------------------------*
*        DCB EXIT FOR CARD-IMAGE FILES                                *
*                                                                     *
*        FORCE RECFM=F LRECL/BLKSIZE FOR C/I IF NO DCB ATTR SPECIFIED *
*---------------------------------------------------------------------*
*
DCBXCIW  DC    0A(0),X'85',AL3(DCBXCI)
*
DCBXCI   DS    0H
         NC    DCBRECFM-IHADCB(L'DCBRECFM,R1),DCBRECFM-IHADCB(R1)
         BNZR  R14
         NC    DCBBLKSI-IHADCB(L'DCBBLKSI,R1),DCBBLKSI-IHADCB(R1)
         BNZR  R14
         NC    DCBLRECL-IHADCB(L'DCBLRECL,R1),DCBLRECL-IHADCB(R1)
         BNZR  R14
         OI    DCBRECFM-IHADCB(R1),DCBRECF
         MVC   DCBLRECL-IHADCB(L'DCBLRECL,R1),=Y(L'UPDREC)
         MVC   DCBBLKSI-IHADCB(L'DCBBLKSI,R1),=Y(L'UPDREC)
         BR    R14
         EJECT
*---------------------------------------------------------------------*
*        DCB EXIT FOR DELTA FILE                                      *
*                                                                     *
*        IF NO RECFM, SPECIFY 'V'                                     *
*        IF NO LRECL, SPECIFY LARGEST EXPECTED RECORD IN DELTA FILE   *
*           (LARGER OF $DLRMAXL AND LARGEST POSSIBLE TEXT RECORD)     *
*        IF NO BLKSIZE, SPECIFY LRECL+4                               *
*                                                                     *
*        DCB SPECIFICATIONS WILL BE CHECKED IN ANY CASE AFTER OPEN    *
*        IN MAINLINE CODE.                                            *
*                                                                     *
*        CODE MUST BE CALLED AFTER 'OPENCMP' HAS BEEN CALLED FOR      *
*        REVISION FILE.                                               *
*---------------------------------------------------------------------*
*
DCBXDLW  DC    0A(0),X'85',AL3(DCBXDL)
*
DCBXDL   DS    0H
         NC    DCBRECFM-IHADCB(L'DCBRECFM,R1),DCBRECFM-IHADCB(R1)
         BNZ   DCBXDL2
         OI    DCBRECFM-IHADCB(R1),DCBRECV
*
DCBXDL2  EQU   *
         NC    DCBLRECL-IHADCB(L'DCBLRECL,R1),DCBLRECL-IHADCB(R1)
         BNZ   DCBXDL3
         MVC   DCBLRECL-IHADCB(L'DCBLRECL,R1),UPDRECLN+L'UPDRECLN-L'DCBX
               LRECL
*
DCBXDL3  EQU   *
         NC    DCBBLKSI-IHADCB(L'DCBBLKSI,R1),DCBBLKSI-IHADCB(R1)
         BNZ   DCBXDLX
         LH    R2,DCBLRECL-IHADCB(,R1)
         LA    R2,$BDWLEN(,R2)
         STH   R2,DCBBLKSI-IHADCB(,R1)
*
DCBXDLX  EQU   *
         BR    R14
         EJECT
*---------------------------------------------------------------------*
*        LITERALS                                                     *
*---------------------------------------------------------------------*
*
         PRINT NOGEN
INDCB    DCB   DDNAME=IN,DSORG=PS,MACRF=GL
OUTDCB   DCB   DDNAME=OUT,DSORG=PS,MACRF=PM
         PRINT GEN
*
DIAGLIT  DC    C' COMPARATOR DIAGNOSTICS '
LIBFIRST DC    C'FIRST'
*
SCALE1   DC    C'1...+....10...+....20...+....30...+....40...+....50...X
               +....60...+....70...+....80...+....90...+....100..+....1X
               10..+....120..+....130'
*
SCALE2   DC    C'0...+....1....+....2....+....3....+....4....+....5....X
               +....6....+....7....+....8....+....9....+....0....+....1X
               ....+....2....+....3..'
*
UDELLIT  DC    C'-DEL '
UINSLIT  DC    C'-INS '
C1LIT    DC    C' BASE FILE: '
C2LIT    DC    C' REVISION FILE: '
PNOMASK  DC    X'4020202020202120'
PNOLIT   DC    C'PAGE '
DSNTRML  DC    C' '
GENTITLE DC    C'VM COMPARATOR SERIES  (C) 1988 W. A. HUMPHRIES INCORPOX
               RATED'
         EJECT
*---------------------------------------------------------------------*
*        LITERALS                                                     *
*---------------------------------------------------------------------*
*
         LTORG
         EJECT
*---------------------------------------------------------------------*
*        LITERALS                                                     *
*---------------------------------------------------------------------*
*
DATLIT   DC    CL8'RUNDATE:'
TIMLIT   DC    CL8'RUNTIME:'
PAGLIT   DC    CL8'PAGE:'
REPLIT   DC    CL8'REPORT:'
OPNLIT   DC    CL8'OPTIONS:'
BCLLIT   DC    CL8'B/COL:'
RCLLIT   DC    CL8'R/COL:'
BLNLIT   DC    CL8'B/LEN:'
RLNLIT   DC    CL8'R/LEN:'
PADLIT   DC    CL8'PAD:'
HSLOTFIL DC    C'.'
HSLOTLEN EQU   8+3+8
*
REP010   DC    C'&WHCN.-010'
REP020   DC    C'&WHCN.-020'
         EJECT
*---------------------------------------------------------------------*
*        LITERALS                                                     *
*---------------------------------------------------------------------*
*
FOOTR1   DC    CL(L'PLINE)' '
         ORG   FOOTR1+H4BGN-H4
         DC    (H4LEN)C'-'
         ORG   ,
*
FOOTR1C  DC    CL(L'PLINE)' '
         ORG   FOOTR1C+H4BGN-H4
         DC    C'CONTINUED'
         ORG   ,
*
FOOTR1E  DC    CL(L'PLINE)' '
         ORG   FOOTR1E+H4BGN-H4
         DC    C'END'
         ORG   ,
         EJECT
NUMTAB   WHCTBN10
         EJECT
BXC1403  WHCBXC10 TYPE=1403
         EJECT
BXC1403T WHCBXC10 TYPE=1403T
         EJECT
BXC3800  WHCBXC10 TYPE=3800
         EJECT
SCANB    WHCTBB10
         EJECT
XLATE    WHCTBS10 FILL=4B,TYPE=3800
         EJECT
REVTAB   WHCTBR10
         EJECT
UPPERCAS WHCTBU10
         EJECT
H1D      DC    CL(L'PLINE)'0'
         ORG   H1D+1+(L'PLINE-1-H1DLEN)/2
H1DBGN   EQU   *
         DC    C'DELTA COMPARATOR (&WHLV1..&WHLV2.)'
H1DLEN   EQU   *-H1DBGN
         ORG   ,
         EJECT
         DCBD  DSORG=QS,DEVD=DA
*
         END
