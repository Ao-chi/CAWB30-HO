       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UAADD.
      * Revision Information
      * ^{File:samp00.cob  }
      * ^{Timestamp:Tue Apr 29 13:57:04 1997}
      * ^{Revision:4.0}
      *
      * CA-Realia CICS Version 6.0
      *
      * This software is the property of Computer Associates International, Inc.
      * and is protected by International Copyright law
      * Any unauthorized use or reproduction is prohibited
      *
      * Copyright (c) 1984, 1997 Computer Associates International, Inc.
      *
      * This software is provided by Computer Associates
      * as an example of a CICS application program.
      * It should not be considered complete or correct.					       *
      *
      ******************************************************************
      *DESCRIPTION:Main menu program				       *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-MAP  VALUE 'sm0001s'        PIC X(7).
      *01  WS-TIME                               PIC 9(15).
       01  WS-DATE                               PIC 9(7).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(7).
      *01  WS-USERID                     PIC X(8).     
       01  WS-LENGTH                     PIC S9(4) COMP.   
       01  WS-REC-LENGTH                 PIC S9(4) COMP VALUE +227. 
       01  WS-KEY-LENGTH                 PIC S9(4) COMP VALUE +6. 
       01  WS-REC3-LENGTH                PIC S9(4) COMP VALUE +89. 
       01  WS-KEY3-LENGTH                PIC S9(4) COMP VALUE +9. 
       01  WS-USR-LREC                   PIC S9(4) COMP VALUE +20. 
       01  WS-TIME			 PIC 9(15) COMP-3.
       01  WS-ERR-MSG			 PIC X(50).
       01  WS-PGM		         PIC X(6).
       01  WS-KEY                        PIC X(06) VALUE '000001'.
       01  WS-KEY-NUM REDEFINES WS-KEY   PIC 9(06).                                       
       01  WS-KEY2                       PIC X(08) VALUE 'ISCB119 '.
       01  WS-KEY3                       PIC X(09) VALUE '000002003'.
       01  WS-REC2                       PIC X(20).  
       01  I                             PIC 9(03).
       01  J                             PIC 9(03). 
       01  K                             PIC 9(03). 
       01  WS-INVALID-RESP               PIC X(13) VALUE 'INVALID RESP'.

       01  WS-MASS-REC. 
           05 FILLER     PIC X(20)    VALUE
              'ISCB101 YYYYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB102 YYYYISCB126 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB103 YYYYISCB127 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB107 NYYNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB108 NYYYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB109 YNNNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB110 YNNYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB111 YYYYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB112 YNYYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB113 YYNNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB114 YYNYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB115 NNNNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB116 NNNNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB117 NNNYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB118 NNYNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB119 NNYYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB120 NYNNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB121 NYNYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB122 NYYNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB123 YYYNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB124 NYYYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB125 YNNNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB126 YNNYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB127 YNYNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB128 YNYYISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB129 YYNNISCB131 '.
           05 FILLER   PIC X(20)    VALUE
              'ISCB130 XCCCISCB131 '.
       01  FILLER REDEFINES WS-MASS-REC.
           05 WS-USER-REC                           OCCURS 28.
              10 WS-USERID.
                 15  WS-USERID7                     PIC X(7).
                 15  FILLER                         PIC X(1).
              10 WS-TYPE.
                 15  WS-REQUESTOR                   PIC X.
                 15  WS-ADMIN                       PIC X.  
                 15  WS-APPROVER                    PIC X.
                 15  WS-SERVICE                     PIC X.
              10 WS-UPDATEDBY                       PIC X(8).
       01  USER-REC.
           05 USERID.
              10  USERID7                        PIC X(7).
              10  FILLER                         PIC X(1).
           05 USR-TYPE.
              10  USR-REQUESTOR                   PIC X.
              10  USR-ADMIN                       PIC X.  
              10  USR-APPROVER                    PIC X.
              10  USR-SERVICE                     PIC X.
           05 UPDATEDBY                       PIC X(8).      
 
       77  WS-RESPONSE                   PIC S9(08) COMP.    
        
      *
	    COPY DFHAID.
      *
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                           PIC X(10).
      *
       PROCEDURE DIVISION.
       100-PROCESS.
	        EXEC CICS IGNORE CONDITION ERROR END-EXEC
                MOVE 1 TO I.
      *         MOVE 1 TO K
      *         move 1 TO J
                PERFORM 28 TIMES
                     MOVE WS-USERID(I) TO  USERID    
                     MOVE WS-TYPE(I) to USR-TYPE
                     move WS-UPDATEDBY(I) to UPDATEDBY
                         EXEC CICS WRITE FILE('UAF002')
                              FROM (USER-REC)
                              RIDFLD (USERID)
                              LENGTH (WS-USR-LREC)
                              RESP2 (WS-RESPONSE)
                        END-EXEC
                         IF EIBRESP NOT = +0
                         EXEC CICS SEND TEXT  
                                  FROM(WS-RESPONSE)
                         END-EXEC
                         END-IF 
                         ADD 1 TO I   
      *                 ADD 1 TO J
                END-PERFORM
                EXEC CICS RETURN
                END-EXEC.  
        