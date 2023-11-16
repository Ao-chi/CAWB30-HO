       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM0000.
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
       01  WS-USERID                     PIC X(8).     
       01  WS-LENGTH                     PIC S9(4) COMP.   
       01  WS-REC-LENGTH                 PIC S9(4) COMP VALUE +227. 
       01  WS-KEY-LENGTH                 PIC S9(4) COMP VALUE +6. 
       01  WS-REC2-LENGTH                PIC S9(4) COMP VALUE +20. 
       01  WS-KEY2-LENGTH                PIC S9(4) COMP VALUE +8. 
       01  WS-REC3-LENGTH                PIC S9(4) COMP VALUE +89. 
       01  WS-KEY3-LENGTH                PIC S9(4) COMP VALUE +9. 
       01  WS-TIME			 PIC 9(15) COMP-3.
       01  WS-ERR-MSG			 PIC X(50).
       01  WS-PGM		         PIC X(6).
       01  WS-KEY                        PIC X(06) VALUE '000001'.
       01  WS-KEY-NUM REDEFINES WS-KEY   PIC 9(06).                                       
       01  WS-KEY2                       PIC X(08). 
       01  WS-KEY3                       PIC X(09) VALUE '000002003'.
       01  WS-REC2                       PIC X(20).  
       01  I                             PIC 9(03).
       01  J                             PIC 9(03). 
       01  K                             PIC 9(03). 
       01  WS-MASS-REC. 
           05 FILLER     PIC X(20)    VALUE
              'ISCB101 NNNNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB102 NNNYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB103 NNYNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB104 NNYYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB105 NYNNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB106 NYNYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB107 NYYNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB108 NYYYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB109 YNNNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB110 YNNYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB111 YNYNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB112 YNYYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB113 YYNNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB114 YYNYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB115 YYYYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB116 NNNNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB117 NNNYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB118 NNYNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB119 NNYYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB120 NYNNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB121 NYNYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB122 NYYNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB124 NYYYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB125 YNNNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB126 YNNYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB127 YNYNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB128 YNYYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB129 YYNNISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB130 YYNYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB131 YYYYISCB131 '.
           05 FILLER     PIC X(20)    VALUE
              'ISCB132 YYYNISCB131 '.
       01  FILLER REDEFINES WS-MASS-REC.
           05 WS-REC-TABLE  OCCURS 32. 
              10 WS-REC2-KEY   PIC X(8).
              10 FILLER        PIC X(12).
 
       01  WS-REC-CONSTANTS.
           05 Ticket-Title           PIC X(25)  VALUE 'TITLE'.
           05 Ticket-Description     PIC X(100) VALUE 'DESCRIPTION'.
           05 Last-Update            PIC X(20)  VALUE
               '22/09/2022 10:11:23'. 
           05 Last-Update-by         PIC X(8)  VALUE 'ISCB145 '. 
           05 Last-Update-Remarks    PIC X(50) VALUE 'REMARKS'. 
   	
       01  WS-REC.
           05 WS-REC-KEY                 PIC X(06).
           05 WS-REC-KEY-NUM REDEFINES WS-REC-KEY PIC 9(06).
           05 WS-REC-Requestor           PIC X(8).
           05 WS-REC-Status              PIC X(10).
           05 WS-REC-Title               PIC X(25).
           05 WS-REC-Description         PIC X(100).
           05 WS-REC-Last-Update         PIC X(20). 
           05 WS-REC-Last-Update-by      PIC X(8). 
           05 WS-REC-Update-Remarks      PIC X(50).
       01  WS-REC3. 
           05 WS-REC3-Ticket-ID          PIC X(6).  
           05 WS-REC3-Seq-Number         PIC 9(03).
           05 WS-REC3-Last-Update        PIC X(20). 
           05 WS-REC3-Last-Update-by     PIC X(8). 
           05 WS-REC3-Update-Remarks     PIC X(50).
           05 FILLER                     PIC X(2). 


 
	   COPY sm0001s.
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
                MOVE 0 TO I.
                
                PERFORM 32 TIMES
                     ADD 1 TO I
                     MOVE WS-REC-TABLE(I) TO WS-REC2 

                    EXEC CICS WRITE FILE('UAF0001')
                         FROM (WS-REC2)
                         RIDFLD (WS-REC2-KEY(I))
                         LENGTH (WS-REC2-LENGTH)
                    END-EXEC
                    EXEC CICS SEND TEXT 
                         FROM (WS-REC2)
                    END-EXEC
                    EXEC CICS SEND TEXT 
                         FROM (EIBRESP)
                    END-EXEC 
                END-PERFORM
                 EXEC CICS RETURN
                END-EXEC.  
        