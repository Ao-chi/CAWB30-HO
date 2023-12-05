       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM006.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       FILE SECTION. 
      *----------------------------------------------------------------*
      *                        WORKING STORAGE                         *
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01  WS-FIELDS. 
           05  WS-STD-KEY-LEN                    PIC S9(4) COMP VALUE 8.
           05  WS-REC.
               10  WS-USERID.
                   15  WS-USERID7                PIC X(7).
                   15  FILLER                    PIC X(1).
               10  WS-TYPE.
                   15  WS-REQUESTOR              PIC X.
                   15  WS-ADMIN                  PIC X.  
                   15  WS-APPROVER               PIC X.
                   15  WS-SERVICE                PIC X.
               10  WS-UPDATEDBY                  PIC X(8).

       01  WS-CURRENT-MAP  VALUE 'SM06S'         PIC X(7).
       01  WS-TIME                               PIC 9(15) COMP-3.
       01  WS-DATE                               PIC 9(10).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(7). 
       01  WS-LENGTH                             PIC S9(4) COMP. 

       01  WS-ERRMSGS.
           05 WS-INVALID-ACCESS                  PIC X(15) VALUE
              'INVALID ACCESS'.
           05 WS-INVALID-PGMID                   PIC X(20) VALUE
              'INVALID USER ACCESS'.   
           05 WS-INVALID-TIX-ACC                 PIC X(34) VALUE
              'INVALID ACCESS TO SELECTED TICKET'.
           05 WS-FIELD-REQ                       PIC X(24) VALUE
              'OPTION FIELD IS REQUIRED'.
           05 WS-PRESS-F2                        PIC X(30) VALUE
              'PRESS PF2 TO CANCEL THE TICKET'.
           05 WS-PRESS-F5                        PIC X(51) VALUE
             'TICKET CANCEL ABORTED. ENTER TICKET AND PRESS ENTER'.
           05 WS-TICKET-CANCELED                 PIC X(53) VALUE
              'TICKET CENCELED, PRESS ENTER TO CANCEL ANOTHER TICKET'.     
           05 WS-ENTER-TICKET                    PIC X(35) VALUE
              'ENTER TICKET NUMBER AND PRESS ENTER'.
           05 WS-NOT-EXIST                       PIC X(28) VALUE
              'TICKET NUMBER DOES NOT EXIST'.    
           05 WS-INVALID-STATUS                  PIC X(28) VALUE 
              'INVALID TICKET STATUS'.
           05 WS-INVALID-PFKEY                   PIC X(21) VALUE
               'INAVLID PFKEY PRESSED'. 

       01  WS-FOUND                              PIC X VALUE 'N'.
       77  WS-RETNCODE                           PIC S9(8) COMP.
       77  WS-RETNCODE2                          PIC S9(8) COMP.
      
       01  WS-FLAG                               PIC X(01) VALUE 'N'.

       01  TICKET-FIELDS.
           05 TICKET-SER-KEY-LEN                 PIC S9(04) COMP 
               VALUE 7.
           05 TICKET-REC.                         
              10 TICKET-NUM                         PIC X(07).
              10 TICKET-REQ-BY                      PIC X(08).
              10 TICKET-STATUS                      PIC X(10).
              10 TICKET-TITLE                       PIC X(25).
              10 TICKET-DESC                        PIC X(100).
              10 TICKET-UPDT-TIME                   PIC X(20).
              10 TICKET-UPDT-BY                     PIC X(08).
              10 TICKET-UPDT-REMARKS                PIC X(50).

       01  WS-TICKET-LOGS.
           05 WS-KEY-LOG.
              10 WS-LOG-TICKET-ID                   PIC X(06).
              10 WS-LOG-SEQ-NUM                     PIC 9(03).
           05 WS-LOG-UPDT-TIME                      PIC X(20).
           05 WS-LOG-UPDT-BY                        PIC X(10).
           05 WS-LOG-UPDT-REMARKS                   PIC X(50).
           

      *--------------------------------------------------------------*
      *               COMMAREA WORKING STORAGE                       *
      *--------------------------------------------------------------*
       01  WS-COMMAREA.
           05 WS-PGMID                           PIC X(06).
           05 WS-STATE                           PIC X.
           05 WS-TICKET-PASSED                   PIC X(07).
           05 USERID.
              10  USERID7                        PIC X(7).
              10  FILLER                         PIC X(1).
           05 USR-TYPE.
             15 USR-REQUESTOR                    PIC X.
             15 USR-ADMIN                        PIC X.  
             15 USR-APPROVER                     PIC X.
             15 USR-SERVICE                      PIC X.
           05 WS-SM012-STATE                     PIC X.  
           05 WS-SM012-PGMID                     PIC X(06).
           05 WS-STF01-REC.
              10 WS-STF01-ID                     PIC X(07).
              10 FILL1                           PIC X(03).
              10 WS-STF01-TITLE                  PIC X(25).
              10 FILL2                           PIC X(01).
              10 WS-STF01-STATUS                 PIC X(10).
              10 FILL3                           PIC X(04). 
              10 WS-STF01-LAST-UPD               PIC X(10).
              10 FILL4                           PIC X(02). 
              10 WS-STF01-LAST-UPDBY             PIC X(8).
              10 WS-STF01-REQ                    PIC X(8).
           05 WS-PGMNAME                         PIC  X(6).
           05 WS-DFHSTATE                        PIC X(15). 
           05 WS-TRANS                           PIC X(04).  
           05 WS-LUSER                           PIC 9(03).
           05 WS-FUSER                           PIC X(07).
           05 WS-PAGE                            PIC 9(02).
           05 WS-PAGE-END                        PIC 9(01).
           05 WS-ADDED-USER                      PIC X(08).   
           05 WS-TICKET-NUMBER                   PIC X(07).
           05 WS-FLAG                            PIC X(01).
           05 WS-TIX-DETL.
              10 WS-TIX-KEY                      PIC X(07).
              10 WS-TIX-KEY-NUM REDEFINES WS-TIX-KEY PIC 9(06).
              10 WS-TIX-REQUESTOR                PIC X(8).
              10 WS-TIX-STATUS                   PIC X(10).
              10 WS-TIX-TITLE                    PIC X(25).
              10 WS-TIX-DESC                     PIC X(100).
              10 WS-TIX-LAST-UPD                 PIC X(20). 
              10 WS-TIX-LAST-UPDBY               PIC X(8). 
              10 WS-TIX-UPD-REMARKS              PIC X(50).

           COPY SM06S.
           COPY DFHAID.
           COPY DFHBMSCA.

      *----------------------------------------------------------------*
      *                         LINKAGE SECTION                        *
      *----------------------------------------------------------------*
       LINKAGE SECTION.
        01 DFHCOMMAREA.
           05 DF-PGMID                           PIC X(06).
           05 DF-STATE                           PIC X. 
           05 DF-TICKET-PASSED                   PIC X(07).
           05 DF-USERID.
              10  DF-USERID7                     PIC X(7).
              10  FILLER                         PIC X(1).
           05 DF-USR-TYPE.
             15 DF-USR-REQUESTOR                 PIC X.
             15 DF-USR-ADMIN                     PIC X.  
             15 DF-USR-APPROVER                  PIC X.
             15 DF-USR-SERVICE                   PIC X.
           05 DF-SM012-STATE                     PIC X.  
           05 DF-SM012-PGMID                     PIC X(06).
           05 DF-STF01-REC.
              10 DF-STF01-ID                     PIC X(07).
              10 DF-FILLER                       PIC X(03).
              10 DF-STF01-TITLE                  PIC X(25). 
              10 DF-FILLER                       PIC X(01).
              10 DF-STF01-STATUS                 PIC X(10).
              10 DF-FILLER                       PIC X(04).
              10 DF-STF01-LAST-UPD               PIC X(10). 
              10 DF-FILLER4                      PIC X(02). 
              10 DF-STF01-LAST-UPDBY             PIC X(8). 
              10 DF-STF01-REQ                    PIC X(8).
           05 DF-PGMNAME                         PIC X(6).
           05 DFHSTATE                           PIC X(15).
           05 DF-TRANS                           PIC X(04).  
           05 DF-LUSER                           PIC 9(03).
           05 DF-FUSER                           PIC X(07).
           05 DF-PAGE                            PIC 9(02).
           05 DF-PAGE-END                        PIC 9(01).
           05 DF-ADDED-USER                      PIC X(08).
           05 DF-ADDED-USER                      PIC X(08).
           05 DF-TIX-DETL.
              10 DF-TIX-KEY                      PIC X(07).
              10 DF-TIX-KEY-NUM REDEFINES DF-TIX-KEY PIC 9(06).
              10 DF-TIX-REQUESTOR                PIC X(8).
              10 DF-TIX-STATUS                   PIC X(10).
              10 DF-TIX-TITLE                    PIC X(25).
              10 DF-TIX-DESC                     PIC X(100).
              10 DF-TIX-LAST-UPD                 PIC X(20). 
              10 DF-TIX-LAST-UPDBY               PIC X(8). 
              10 DF-TIX-UPD-REMARKS              PIC X(50).

       PROCEDURE DIVISION.
       100-PROCESS.
           EXEC CICS IGNORE CONDITION
                     ERROR 
           END-EXEC

           MOVE DFHCOMMAREA TO WS-COMMAREA
           IF WS-PGMID = 'SM000' OR WS-PGMID = 'SM001' OR
              WS-PGMID = 'SM012' 
                   EVALUATE TRUE
                    WHEN WS-STATE = LOW-VALUES
                         EVALUATE TRUE
                            WHEN WS-TICKET-PASSED = LOW-VALUES
                                MOVE 1 TO WS-STATE
                                MOVE WS-ENTER-TICKET TO ERRMSGO
                                MOVE -1 TO TKTNUML
                                PERFORM 200-NEW-MAP
                         END-EVALUATE
                    WHEN WS-STATE = 1
                         PERFORM 300-REC-MAP
                         PERFORM 400-CHECK-AID
                         PERFORM 500-MOVE-FILE-TO-COMM
                         PERFORM 200-NEW-MAP
                    WHEN WS-STATE = 2
                         PERFORM 300-REC-MAP
                         PERFORM 400-CHECK-AID
                         PERFORM 200-NEW-MAP
                    WHEN WS-STATE = 3
                         PERFORM 300-REC-MAP
                         PERFORM 400-CHECK-AID
                         PERFORM 200-NEW-MAP
                   END-EVALUATE
            ELSE
                MOVE SPACES TO ERRMSGO
                MOVE LENGTH OF WS-INVALID-ACCESS TO WS-LENGTH
                EXEC CICS SEND TEXT
                     FROM (WS-INVALID-ACCESS)
                     LENGTH (WS-LENGTH)
                     ERASE
                 END-EXEC
                EXEC CICS RETURN
                END-EXEC  
            END-IF.

       100-EXIT.
           EXIT.

       110-DATE-AND-TIME.

           MOVE EIBDATE TO WS-DATE.
           MOVE WS-DATE-X TO DATEO.
           EXEC CICS ASKTIME    
	           ABSTIME	(WS-TIME)
	           END-EXEC
	           EXEC CICS FORMATTIME
	           ABSTIME	(WS-TIME)
	           DATESEP	('/')
	           MMDDYYYY (DATEO)
               TIME     (TIMEO)
               TIMESEP  (':')    
           END-EXEC.
       110-EXIT.
           EXIT.

       200-NEW-MAP.

           PERFORM 110-DATE-AND-TIME
           EXEC CICS SEND 
                 MAP('SM006M')
                 MAPSET('SM06S')
                 FROM(SM006MO)
                 CURSOR
                 LENGTH(WS-LENGTH)
                 ERASE
           END-EXEC
           EXEC CICS RETURN
                TRANSID('SM06')
                COMMAREA(WS-COMMAREA)
           END-EXEC.

       200-EXIT.
           EXIT.

       300-REC-MAP.
           EXEC CICS RECEIVE 
                MAP('SM006M')
                MAPSET('SM06S')
                INTO (SM006MI)
           END-EXEC.

       300-EXIT.
           EXIT.

       400-CHECK-AID.
           EVALUATE EIBAID
               WHEN DFHENTER
                   EVALUATE TRUE
                       WHEN WS-STATE = 1
                       MOVE 2 TO WS-STATE
                       MOVE -1 TO TKTNUML
                    IF TKTNUMI NOT = LOW-VALUES
                       MOVE TKTNUMI TO WS-TICKET-PASSED 
                    ELSE
                        MOVE WS-TICKET-PASSED TO TKTNUMI
                    END-IF
                       PERFORM 600-TICKET-VALID
                       IF WS-FOUND = 'Y'
                           PERFORM 700-MOVE-DATA-TO-SCREEN
                           MOVE WS-PRESS-F2 TO ERRMSGO
                       ELSE    
                           PERFORM 610-MOVE-LOW-VAL
                           MOVE 1 TO WS-STATE
                       END-IF
               WHEN WS-STATE = 2
                       PERFORM 410-REDISPLAY-INVALID-PFKEY
                       MOVE WS-PRESS-F2 TO ERRMSGO
                       MOVE -1 TO TKTNUML
                  CONTINUE
               WHEN WS-STATE = 3
                   MOVE 1 TO WS-STATE
                   PERFORM 610-MOVE-LOW-VAL
                   MOVE -1 TO TKTNUML
                   MOVE WS-ENTER-TICKET TO ERRMSGO
               END-EVALUATE
            WHEN DFHPF2
                EVALUATE TRUE
                    WHEN WS-STATE = LOW-VALUES
                        CONTINUE
                    WHEN WS-STATE = 1
                        MOVE -1 TO TKTNUML
                        MOVE WS-ENTER-TICKET TO ERRMSGO
                        MOVE WS-INVALID-PFKEY TO ERRMSGO
                        CONTINUE
                    WHEN WS-STATE = 2
                        PERFORM 600-TICKET-VALID
                        MOVE WS-TICKET-CANCELED TO ERRMSGO
                        PERFORM 110-DATE-AND-TIME
                        MOVE DATEO TO UPDTO(1:10)
                        MOVE TIMEO TO UPDTO(12:9)
                        MOVE WS-TIX-KEY TO TKTNUMI
                        MOVE WS-TIX-REQUESTOR TO REQBYO
                        MOVE WS-TIX-TITLE TO TKTLEO
                        MOVE TICKET-DESC (1:50) TO TKDES1O
                        MOVE TICKET-DESC (51:50) TO TKDES2O
                        MOVE 'CANCELED' TO STATO
                        MOVE USERID TO UPDTBYO
                        MOVE 'TICKET CANCELED' TO UPDREM1O
                        MOVE 3 TO WS-STATE
                        PERFORM 500-MOVE-FILE-TO-COMM
                        PERFORM 710-UPDATE-TICKET
                        PERFORM 800-READ-LOG-REC
                        MOVE -1 TO TKTNUML
                    WHEN WS-STATE = 3
                        MOVE -1 TO TKTNUML
                        MOVE WS-TICKET-CANCELED TO ERRMSGO
                        PERFORM 110-DATE-AND-TIME
                        MOVE DATEO TO UPDTO (1:10)
                        MOVE TIMEO TO UPDTO (12:9)
                        MOVE WS-TIX-KEY TO TKTNUMI
                        MOVE WS-TIX-REQUESTOR TO REQBYO
                        MOVE WS-TIX-TITLE TO TKTLEO
                        MOVE TICKET-DESC (1:50) TO TKDES1O
                        MOVE TICKET-DESC (51:50) TO TKDES2O
                        MOVE 'CANCELED' TO STATO
                        MOVE USERID TO UPDTBYO
                        MOVE 'TICKET CANCELED' TO UPDREM1O
                END-EVALUATE

             WHEN DFHPF3
                MOVE LOW-VALUES TO WS-STATE
                MOVE LOW-VALUES TO WS-TICKET-PASSED
                EVALUATE WS-PGMID
                 WHEN 'SM000'
                    MOVE 'SM006' TO WS-PGMID
		            EXEC CICS XCTL
			             PROGRAM('SM000')
                         COMMAREA(WS-COMMAREA)  
                    END-EXEC  
                 WHEN 'SM001'  
                    MOVE 'SM006' TO WS-PGMID
                    EXEC CICS XCTL
			              PROGRAM('SM001')
                    END-EXEC  
                 WHEN 'SM012'  
                    MOVE 'SM006' TO WS-PGMID
                    EXEC CICS XCTL
			              PROGRAM('SM012')
                    END-EXEC     
                END-EVALUATE

            WHEN DFHPF5
                EVALUATE TRUE
                    WHEN WS-STATE = LOW-VALUES
                        MOVE -1 TO TKTNUML
                        MOVE WS-ENTER-TICKET TO ERRMSGO
                        CONTINUE
                    WHEN WS-STATE = 1
                        MOVE -1 TO TKTNUML
                        MOVE WS-ENTER-TICKET TO ERRMSGO
                        MOVE WS-INVALID-PFKEY TO ERRMSGO
                        CONTINUE
                    WHEN WS-STATE = 2
                        MOVE 1 TO WS-STATE
                        MOVE -1 TO TKTNUML
                        PERFORM 610-MOVE-LOW-VAL
                        MOVE WS-PRESS-F5 TO ERRMSGO
                        CONTINUE
                    WHEN WS-STATE = 3
                        MOVE -1 TO TKTNUML
                        MOVE WS-TICKET-CANCELED TO ERRMSGO
                        PERFORM 110-DATE-AND-TIME
                        MOVE DATEO TO UPDTO (1:10)
                        MOVE TIMEO TO UPDTO (12:9)
                        MOVE WS-TIX-KEY TO TKTNUMI
                        MOVE WS-TIX-REQUESTOR TO REQBYO
                        MOVE WS-TIX-TITLE TO TKTLEO
                        MOVE TICKET-DESC (1:50) TO TKDES1O
                        MOVE TICKET-DESC (51:50) TO TKDES2O
                        MOVE 'CANCELED' TO STATO
                        MOVE USERID TO UPDTBYO
                        MOVE 'TICKET CANCELED' TO UPDREM1O
                END-EVALUATE
             WHEN DFHPF12 
                  MOVE 'SM006' TO WS-SM012-PGMID
      *           MOVE LOW-VALUES TO WS-STATE
                  MOVE LOW-VALUES TO WS-SM012-STATE 
                  EXEC CICS XCTL
                       PROGRAM('SM012')
                       COMMAREA(WS-COMMAREA)
                  END-EXEC  
            WHEN DFHCLEAR
                EVALUATE TRUE    
                    WHEN WS-STATE = LOW-VALUES
                        MOVE -1 TO TKTNUML
                        MOVE WS-INVALID-PFKEY TO ERRMSGO
                        PERFORM 200-NEW-MAP
                    WHEN WS-STATE = 1
                        MOVE -1 TO TKTNUML
                        MOVE WS-INVALID-PFKEY TO ERRMSGO
                        PERFORM 200-NEW-MAP
                    WHEN WS-STATE = 2
                        PERFORM 410-REDISPLAY-INVALID-PFKEY
                        MOVE WS-INVALID-PFKEY TO ERRMSGO
                        MOVE -1 TO TKTNUML
                        PERFORM 200-NEW-MAP
                    WHEN WS-STATE = 3
                        PERFORM 410-REDISPLAY-INVALID-PFKEY
                        MOVE WS-INVALID-PFKEY TO ERRMSGO
                        MOVE -1 TO TKTNUML
                        PERFORM 200-NEW-MAP
                END-EVALUATE
            WHEN OTHER 
                        PERFORM 410-REDISPLAY-INVALID-PFKEY
                        MOVE WS-INVALID-PFKEY TO ERRMSGO
                        MOVE -1 TO TKTNUML
                        PERFORM 200-NEW-MAP

           END-EVALUATE.

       400-EXIT.
           EXIT.

       410-REDISPLAY-INVALID-PFKEY.
      
           MOVE WS-TIX-KEY TO TKTNUMI
           MOVE WS-TIX-REQUESTOR TO REQBYO
           MOVE WS-TIX-TITLE TO TKTLEO
           EVALUATE WS-STATE    
               WHEN 2
                   MOVE WS-TIX-DESC(1:50) TO TKDES1O
                   MOVE WS-TIX-DESC(51:50) TO TKDES2O
                   MOVE WS-TIX-UPD-REMARKS(1:25) TO UPDREM1O
                   MOVE WS-TIX-UPD-REMARKS(26:25) TO UPDREM2O
               WHEN 3
                   MOVE TICKET-DESC(1:50) TO TKDES1O
                   MOVE TICKET-DESC(51:50) TO TKDES2O
                   MOVE TICKET-UPDT-REMARKS(1:25) TO UPDREM1O
                   MOVE TICKET-UPDT-REMARKS(26:25) TO UPDREM2O
            END-EVALUATE
           MOVE WS-TIX-STATUS TO STATO
           MOVE WS-TIX-LAST-UPDBY TO UPDTBYO
           MOVE WS-TIX-LAST-UPD TO UPDTO.

       410-EXIT.    
           EXIT.

       500-MOVE-FILE-TO-COMM.
           IF TKTNUMI = LOW-VALUES
              MOVE WS-TICKET-PASSED TO WS-TIX-KEY
           ELSE 
           MOVE TKTNUMI TO WS-TIX-KEY
           END-IF
           MOVE REQBYO TO WS-TIX-REQUESTOR
           MOVE TKTLEO TO WS-TIX-TITLE
           MOVE TKDES1O TO WS-TIX-DESC(1:50)
           MOVE TKDES2O TO WS-TIX-DESC(51:50)
           MOVE STATO TO WS-TIX-STATUS
           MOVE UPDTBYO TO WS-TIX-LAST-UPDBY
           MOVE UPDREM1O TO WS-TIX-UPD-REMARKS(1:25)
           MOVE UPDREM2O TO WS-TIX-UPD-REMARKS(26:25)
           MOVE UPDTO TO WS-TIX-LAST-UPD.

       500-EXIT.
           EXIT.
    
       600-TICKET-VALID.
           
           EXEC CICS STARTBR
                FILE('STF001C')
                RIDFLD (WS-TICKET-PASSED)
                GTEQ
            END-EXEC
            IF EIBRESP = +0
                EXEC CICS 
                     READ FILE('STF001C')
                     INTO (TICKET-REC)
                     RIDFLD(WS-TICKET-PASSED)
                     EQUAL
                     UPDATE
                END-EXEC
            MOVE 'Y' TO WS-FOUND
                IF TICKET-STATUS = 'ONGOING' OR 'CREATED' OR 'CLOSED'
                                   OR 'COMPLETED' OR 'APPROVED'
                MOVE 'Y' TO WS-FOUND
            ELSE
                MOVE -1 TO TKTNUML
                MOVE 'N' TO WS-FOUND
                MOVE WS-INVALID-STATUS TO ERRMSGO
            END-IF
            ELSE
                MOVE -1 TO TKTNUML
                MOVE 'N' TO WS-FOUND
                MOVE WS-NOT-EXIST TO ERRMSGO
            END-IF.
           
       600-EXIT.
           EXIT.

       610-MOVE-LOW-VAL.

           MOVE LOW-VALUES TO REQBYO
           MOVE LOW-VALUES TO TKTLEO
           MOVE LOW-VALUES TO TKDES1O
           MOVE LOW-VALUES TO TKDES2O
           MOVE LOW-VALUES TO STATO
           MOVE LOW-VALUES TO UPDTBYO
           MOVE LOW-VALUES TO UPDREM1O
           MOVE LOW-VALUES TO UPDREM2O
           MOVE LOW-VALUES TO UPDTO.

       610-EXIT.
           EXIT.

       700-MOVE-DATA-TO-SCREEN.

           MOVE TICKET-REQ-BY TO REQBYO
           MOVE TICKET-TITLE  TO TKTLEO
           MOVE TICKET-DESC(1:50) TO TKDES1O
           MOVE TICKET-DESC(51:50) TO TKDES2O
           MOVE TICKET-STATUS TO STATO
           MOVE TICKET-UPDT-BY TO UPDTBYO
           MOVE TICKET-UPDT-REMARKS(1:25) TO UPDREM1O
           MOVE TICKET-UPDT-REMARKS(26:25) TO UPDREM2O
           MOVE TICKET-UPDT-TIME TO UPDTO.
       

       700-EXIT.
           EXIT.

       710-UPDATE-TICKET.
      
      *    MOVE WS-TICKET-PASSED TO WS-TICKET-KEY
           MOVE REQBYO TO TICKET-REQ-BY
           MOVE TKTLEO TO TICKET-TITLE
           MOVE TKDES1O TO TICKET-DESC(1:50)
           MOVE TKDES2O TO TICKET-DESC(51:50)
           MOVE STATO TO TICKET-STATUS
           MOVE UPDTBYO TO TICKET-UPDT-BY
           MOVE UPDREM1O TO TICKET-UPDT-REMARKS(1:25)
           MOVE UPDREM2O TO TICKET-UPDT-REMARKS(26:25)
           MOVE UPDTO TO TICKET-UPDT-TIME
           EXEC CICS
                REWRITE FILE ('STF001C')
                FROM (TICKET-REC)
                RESP(WS-RETNCODE)
                RESP2(WS-RETNCODE2)
           END-EXEC.


       710-EXIT.    
           EXIT.

       800-READ-LOG-REC.
               
           EXEC CICS READ
               FILE('STF002E')
               INTO(WS-TICKET-LOGS)
               RIDFLD(WS-KEY-LOG)
               GTEQ
           END-EXEC
           MOVE WS-TICKET-PASSED TO WS-LOG-TICKET-ID
           ADD 1 TO WS-LOG-SEQ-NUM
           MOVE USERID TO WS-LOG-UPDT-BY
           MOVE DATEO TO WS-LOG-UPDT-TIME(1:10)
           MOVE TIMEO TO WS-LOG-UPDT-TIME(12:9) 
           MOVE UPDREM1O TO WS-LOG-UPDT-REMARKS(1:25)
           MOVE UPDREM2O TO WS-LOG-UPDT-REMARKS(26:25)
           PERFORM 810-CREATE-LOG-REC.

       800-EXIT.
           EXIT.

       810-CREATE-LOG-REC.
           EXEC CICS
                WRITE FILE('STF002E')
                FROM (WS-TICKET-LOGS)
                RIDFLD (WS-KEY-LOG)
                RESP(WS-RETNCODE)
           END-EXEC.
       810-EXIT.
           EXIT. 