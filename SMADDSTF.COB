       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SMADDSTF.
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
       01  WS-REC-LENGTH                 PIC S9(4) COMP VALUE +228. 
       01  WS-KEY-LENGTH                 PIC S9(4) COMP VALUE +7. 
       01  WS-REC3-LENGTH                PIC S9(4) COMP VALUE +89. 
       01  WS-KEY3-LENGTH                PIC S9(4) COMP VALUE +9. 
       01  WS-TIME			 PIC 9(15) COMP-3.
       01  WS-ERR-MSG			 PIC X(50).
       01  WS-PGM		         PIC X(6).
       01  WS-KEY                        PIC X(07) VALUE '000001 '.
       01  WS-KEY-NUM REDEFINES WS-KEY   PIC 9(06).                                       
       01  WS-INVALID-RESP               PIC X(13) VALUE 'INVALID RESP'.
       01  WS-KEY2                       PIC X(08) VALUE 'ISCB119 '.
       01  WS-KEY3                       PIC X(09) VALUE '000002003'.
       01  WS-REC2                       PIC X(20).  
       01  I                             PIC 9(03).
       01  J                             PIC 9(03). 
       01  K                             PIC 9(03). 
       01  WS-MASS-REC. 
           05 FILLER     PIC X(25)    VALUE
              '000006 ISCB101 ONGOING   '.
           05 FILLER     PIC X(25)    VALUE
              '000007 ISCB102 APPROVED  '.
           05 FILLER     PIC X(25)    VALUE
              '000008 ISCB103 ONGOING   '.
           05 FILLER     PIC X(25)    VALUE
              '000009 ISCB104 COMPLETED '.
           05 FILLER     PIC X(25)    VALUE
              '000010 ISCB105 CLOSED    '.
           05 FILLER     PIC X(25)    VALUE
              '000011 ISCB106 APPROVED  '.
           05 FILLER     PIC X(25)    VALUE
              '000012 ISCB107 COMPLETED '.
           05 FILLER     PIC X(25)    VALUE
              '000013 ISCB108 APPROVED  '.
           05 FILLER     PIC X(25)    VALUE
              '000014 ISCB109 CLOSED    '.
           05 FILLER     PIC X(25)    VALUE
              '000015 ISCB110 CREATED   '.
           05 FILLER     PIC X(25)    VALUE
              '000016 ISCB110 CREATED   '.
           05 FILLER     PIC X(25)    VALUE
              '000017 ISCB110 CREATED   '.
           05 FILLER     PIC X(25)    VALUE
              '000018 ISCB110 APPROVED  '.
           05 FILLER     PIC X(25)    VALUE
              '000019 ISCB110 CREATED   '.
           05 FILLER     PIC X(25)    VALUE
              '000020 ISCB120 ONGOING   '.
           05 FILLER     PIC X(25)    VALUE
              '000021 ISCB120 ONGOING   '.
           05 FILLER     PIC X(25)    VALUE
              '000022 ISCB120 CREATED   '.
           05 FILLER     PIC X(25)    VALUE
              '000023 ISCB120 ONGOING   '.
           05 FILLER     PIC X(25)    VALUE
              '000024 ISCB120 ONGOING   '.
           05 FILLER     PIC X(25)    VALUE
              '000025 ISCB120 APPROVED  '.
           05 FILLER     PIC X(25)    VALUE
              '000026 ISCB120 APPROVED  '.
           05 FILLER     PIC X(25)    VALUE
              '000027 ISCB120 CREATED   '.
           05 FILLER     PIC X(25)    VALUE
              '000028 ISCB120 CREATED   '.
           05 FILLER     PIC X(25)    VALUE
              '000029 ISCB120 CREATED   '.
           05 FILLER     PIC X(25)    VALUE
              '000030 ISCB130 APPROVED  '.
           05 FILLER     PIC X(25)    VALUE
              '000031 ISCB130 ONGOING   '.
           05 FILLER     PIC X(25)    VALUE
              '000032 ISCB130 CREATED   '.
           05 FILLER     PIC X(25)    VALUE
              '000033 ISCB130 ONGOING   '.
           05 FILLER     PIC X(25)    VALUE
              '000034 ISCB130 ONGOING   '.
           05 FILLER     PIC X(25)    VALUE
              '000035 ISCB130 APPROVED  '.
       01  FILLER REDEFINES WS-MASS-REC.
           05 WS-REC-TABLE           OCCURS 30.
              10 Ticket-ID                  PIC X(7).
              10 Requestor                  PIC X(8).
              10 TKT-Stat                   PIC X(10).
       01  WS-REC-CONSTANTS.
           05 Ticket-Title           PIC X(25)  VALUE 'TITLE'.
           05 Ticket-Description     PIC X(100) VALUE 'DESCRIPTION'.
           05 Last-Update            PIC X(20)  VALUE
               '22/09/2022 10:11:23'. 
           05 Last-Update-by         PIC X(8)  VALUE 'ISCB145 '. 
           05 Last-Update-Remarks    PIC X(50) VALUE 'REMARKS'. 
   	
       01  WS-REC.
           05 WS-REC-KEY                 PIC X(07).
           05 WS-REC-KEY-NUM REDEFINES WS-REC-KEY PIC 9(07).
           05 WS-REC-Requestor           PIC X(8).
           05 WS-REC-Status              PIC X(10).
           05 WS-REC-Title               PIC X(25).
           05 WS-REC-Description         PIC X(100).
           05 WS-REC-Last-Update         PIC X(20). 
           05 WS-REC-Last-Update-by      PIC X(8). 
           05 WS-REC-Update-Remarks      PIC X(50).
       01  WS-REC3. 
           05 WS-REC3-Ticket-ID          PIC X(7).  
           05 WS-REC3-Seq-Number         PIC 9(03).
           05 WS-REC3-Last-Update        PIC X(20). 
           05 WS-REC3-Last-Update-by     PIC X(8). 
           05 WS-REC3-Update-Remarks     PIC X(50).
           05 FILLER                     PIC X(2). 


 
	   
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
                
                PERFORM 30 TIMES
                     
                     MOVE Ticket-ID(I)       TO 
                          WS-REC-KEY  
                     MOVE Requestor(I)       TO
                         WS-REC-Requestor 
                     MOVE TKT-STAT(I)        TO
                         WS-REC-Status
                     MOVE Ticket-Title        TO 
                         WS-REC-Title  
                     MOVE Ticket-Description  TO
                         WS-REC-Description 
                     MOVE Last-Update         TO 
                         WS-REC-Last-Update    
                     MOVE Last-Update-by      TO
                         WS-REC-Last-Update-by 
                     MOVE Last-Update-Remarks TO 
                         WS-REC-Update-Remarks   

                    EXEC CICS WRITE FILE('STF001C')
                         FROM (WS-REC)
                         RIDFLD (WS-REC-KEY)
                         LENGTH (+228)
                    END-EXEC

                    IF EIBRESP NOT = +0
                    EXEC CICS SEND TEXT  
                             FROM(WS-INVALID-RESP)
                    END-EXEC
                    END-IF 
                    ADD 1 TO I                
                END-PERFORM
                EXEC CICS RETURN
                END-EXEC.  
        