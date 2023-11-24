       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM002.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-MAP  VALUE 'SM002M'        PIC X(7).
       01  WS-TIME				 PIC 9(15).
       01  WS-DATE                               PIC 9(7).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(7).
       01  WS-LENGTH                             PIC S9(4) COMP. 
      *01  WS-COMMAREA. 
      *    05 WS-PROG-STATE                      PIC X(17).
       01  WS-END                                PIC X(15) VALUE
           'INVALID ACCESS'.
       77 WS-RETNCODE                            PIC S9(8) COMP.
       77 TICKETADD                              PIC 9(06).
       77 REQUESTOR                              PIC X(01) VALUE 'Y'.

       01  TICKET.
           05 TICKET-KEY                 PIC X(07).
           05 TICKET-KEY-NUM REDEFINES TICKET-KEY PIC 9(06).
           05 TICKET-Requestor           PIC X(8).
           05 TICKET-Status              PIC X(10).
           05 TICKET-Title               PIC X(25).
           05 TICKET-DESCRIPTION         PIC X(100).
           05 TICKET-Last-Update         PIC X(20). 
           05 TICKET-Last-Update-by      PIC X(8). 
           05 TICKET-Update-Remarks      PIC X(50).
       
       01  NEW-TICKET.
           05 NEW-TICKET-KEY                 PIC X(07).
           05 NEW-TICKET-KEY-NUM REDEFINES NEW-TICKET-KEY PIC 9(07).
           05 NEW-TICKET-Requestor           PIC X(08) VALUE 'ISCB124'.
           05 NEW-TICKET-Status              PIC X(10) VALUE 'CREATED'.
           05 NEW-TICKET-Title               PIC X(25).
           05 NEW-TICKET-DESCRIPTION         PIC X(100).
           05 NEW-TICKET-Last-Update         PIC X(20) VALUE SPACE. 
           05 NEW-TICKET-Last-Update-by      PIC X(08) VALUE SPACE. 
           05 NEW-TICKET-Update-Remarks      PIC X(50) VALUE SPACE.

       01  WS-LOG. 
           05 WS-LOG-Ticket-ID          PIC X(07).  
           05 WS-LOG-Seq-Number         PIC 9(03) VALUE 1.
           05 WS-LOG-Last-Update        PIC X(20) VALUE SPACE. 
           05 WS-LOG-Last-Update-by     PIC X(08) VALUE SPACE. 
           05 WS-LOG-Update-Remarks     PIC X(50) VALUE SPACE.
           05 FILLER                    PIC X(02) VALUE SPACE.
       
       01  WS-ERRMSGS.
           05 WS-INVALID-ACCESS                  PIC X(15) VALUE
              'INVALID ACCESS'.
           05 WS-INVALID-PGMID                   PIC X(20) VALUE
              'INVALID USER ACCESS'.   
           05 WS-MAPFAIL                         PIC X(20) VALUE
              'MAPFAIL ERROR'.
           05 WS-INVALID-TIX-ACC                 PIC X(34) VALUE
              'INVALID ACCESS TO SELECTED TICKET'.
           05 WS-FIELD-REQ                       PIC X(24) VALUE
              'OPTION FIELD IS REQUIRED'.
           05 WS-TICKET-REQUIRED                 PIC X(24) VALUE
              'TICKET TITLE IS REQUIRED'.
           05 WS-LAST-PAGE                       PIC X(21) VALUE
              'THIS IS THE LAST PAGE'.
           05 WS-INVALID-VALUE                   PIC X(46) VALUE
              'INVALID VALUE. PLEASE CORRECT HIGHLIGHT FIELDS'.   
           05 WS-INVALID-PFKEY                   PIC X(21) VALUE
               'INAVLID PFKEY PRESSED'.    

       01  WS-COMMAREA.
           05 WS-PGMID                           PIC X(06).
           05 WS-TICKET-PASSED                   PIC X(07).
           05 USERID.
              10  USERID7                        PIC X(7).
              10  FILLER                         PIC X(1).
           05 USR-TYPE.
             15 USR-REQUESTOR                    PIC X.
             15 USR-ADMIN                        PIC X.  
             15 USR-APPROVER                     PIC X.
             15 USR-SERVICE                      PIC X.
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
           05 WS-STATE                           PIC X.
           05 WS-QITEM                           PIC S9(4) COMP.
           05 WS-QITEM-START                     PIC S9(4) COMP.
           05 WS-QITEM-END                       PIC S9(4) COMP.
           05 WS-QITEM-PAGE                      PIC S9(4) COMP.    
           05 WS-PROG-STATE                      PIC X(10).
           
           COPY SM02S.
           COPY DFHAID.
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
           05 DF-PGMID                           PIC X(06).
           05 DF-TICKET-PASSED                   PIC X(07).
           05 DF-USERID.
              10  DF-USERID7                     PIC X(7).
              10  FILLER                         PIC X(1).
           05 DF-USR-TYPE.
             15 DF-USR-REQUESTOR                 PIC X.
             15 DF-USR-ADMIN                     PIC X.  
             15 DF-USR-APPROVER                  PIC X.
             15 DF-USR-SERVICE                   PIC X.
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
           05 DF-STATE                           PIC X. 
           05 DF-QITEM                           PIC S9(4) COMP.
           05 DF-QITEM-START                     PIC S9(4) COMP.
           05 DF-QITEM-END                       PIC S9(4) COMP.
           05 DF-QITEM-PAGE                      PIC S9(4) COMP.
           05 DF-PROG-STATE                      PIC X(10).

       PROCEDURE DIVISION.
       100-MAIN.
           MOVE DFHCOMMAREA TO WS-COMMAREA
           EXEC CICS IGNORE CONDITION
                     ERROR
           END-EXEC

           MOVE HIGH-VALUE TO TICKET-KEY  
           EXEC CICS STARTBR FILE('STF001C')
             RIDFLD (TICKET-KEY)
             GTEQ
           END-EXEC
           IF EIBRESP = +0
             EXEC CICS READPREV
               FILE('STF001C') 
               INTO(TICKET)
               RIDFLD (TICKET-KEY)
             END-EXEC
           END-IF
           
           IF WS-PGMID = 'SM000' OR WS-PGMID = 'SM001' OR
              WS-PGMID = 'SM002'
               IF USR-REQUESTOR = 'Y'
                  EVALUATE TRUE
      *       Neal pinalitan ko ung WS-COMMAREA at WS-PROG-STATE ng 
      *       WS-STATE, bali nanggaling ung state sa commarea ng calling program    
                   WHEN WS-STATE = LOW-VALUES
                     ADD  1 TO TICKET-KEY-NUM 
                     MOVE TICKET-KEY-NUM TO IDO
                     MOVE 'A'  TO WS-STATE
                     MOVE SPACE TO TICKETTO
                     MOVE SPACE TO TICKTD1O
                     MOVE SPACE TO TICKTD2O
                     MOVE 'ENTER TICKET DETAILS AND PRESS PF2' 
                         TO ERRMSGO 
                     PERFORM 1000-NEW-MAP

                   WHEN WS-STATE = 'A'
                     PERFORM 3000-RMAP
                     PERFORM 2000-CHECK-AID
                     PERFORM 1000-NEW-MAP

                   WHEN WS-STATE = 'B'
                     PERFORM 3000-RMAP
                     PERFORM 2000-CHECK-AID
                     PERFORM 4000-WRITE
                     MOVE 'CREATED' TO STATSO
                     MOVE 'SUCCESSFULLY CREATED. PF2 TO ADD MORE'
                        TO ERRMSGO 
                     MOVE DFHBMASK TO STA
                     MOVE DFHBMPRO TO STATSA
                     MOVE DFHBMPRO TO TICKETTA
                     MOVE DFHBMPRO TO TICKTD1A
                     MOVE DFHBMPRO TO TICKTD2A
                     PERFORM 1000-NEW-MAP

                  END-EVALUATE 
               ELSE
                  PERFORM 110-INVALID-ACCESS
               END-IF
           ELSE
              PERFORM 110-INVALID-ACCESS
           END-IF. 
       100-EXIT.
           EXIT.

       110-INVALID-ACCESS.
           MOVE LENGTH OF WS-INVALID-ACCESS TO WS-LENGTH
           EXEC CICS SEND TEXT
              FROM (WS-INVALID-ACCESS)
              LENGTH (WS-LENGTH)
              ERASE
           END-EXEC  
           EXEC CICS RETURN
           END-EXEC.
       110-EXIT. EXIT.

       1000-NEW-MAP.
           MOVE EIBDATE TO WS-DATE
           MOVE WS-DATE-X TO DATEO
           EXEC CICS ASKTIME
              ABSTIME    (WS-TIME)
           END-EXEC
           EXEC CICS FORMATTIME
              ABSTIME    (WS-TIME)
              DATESEP    ('/')
              MMDDYYYY   (DATEO)
              TIME       (TIMEO)
              TIMESEP    (':')
           END-EXEC
           MOVE DFHBMASB TO TIMEA
           MOVE DFHBMASB TO DATEA
           MOVE DFHBMASB TO ERRMSGA
           MOVE LENGTH OF SM002MO TO WS-LENGTH 
		       EXEC CICS SEND 
                    MAP('SM002M')
			        MAPSET('SM02S')
			        FROM(SM002MO)
			        LENGTH(WS-LENGTH)
              ERASE
			     END-EXEC.
           EXEC CICS RETURN
                TRANSID('SM02')
            	COMMAREA(WS-COMMAREA)
	         END-EXEC.
       1000-EXIT. 
           EXIT.

       2000-CHECK-AID.
            EVALUATE TRUE
              WHEN EIBAID = DFHPF2
                MOVE IDI TO NEW-TICKET-KEY
                MOVE IDI TO WS-LOG-Ticket-ID
                MOVE TICKETTI TO NEW-TICKET-Title
                MOVE TICKTD1I TO NEW-TICKET-DESCRIPTION(1:50)
                MOVE TICKTD2I TO NEW-TICKET-DESCRIPTION(51:50)

               IF WS-STATE = 'B' THEN
                 MOVE SPACE  TO WS-STATE
               ELSE
                 EVALUATE TRUE
                   WHEN NEW-TICKET-TITLE = SPACE
                     MOVE WS-TICKET-REQUIRED TO ERRMSGO 

                   WHEN NEW-TICKET-DESCRIPTION = SPACE
                     MOVE 'TICKET DESCRIPTION IS REQUIRED' 
                         TO ERRMSGO 

                   WHEN OTHER
                     MOVE 'B' TO WS-STATE
                     MOVE 'VALID INPUT PRESS PF2 TO ADD' 
                         TO ERRMSGO 
                 END-EVALUATE
              END-IF
              WHEN EIBAID = DFHPF3
                   EVALUATE WS-PGMID
                    WHEN 'SM000'
		    	       EXEC CICS XCTL
			             PROGRAM('SM000')
      *		             COMMAREA(WS-COMMAREA)
      *		             LENGTH(17) 
                       END-EXEC  
                    WHEN 'SM001'  
                          EXEC CICS XCTL
			                    PROGRAM('SM000')
                          END-EXEC  
                   END-EVALUATE       
              WHEN EIBAID = DFHPF5
                MOVE LOW-VALUES TO WS-STATE
              WHEN OTHER
                MOVE WS-INVALID-PFKEY TO  ERRMSGO 
            END-EVALUATE.
       2000-EXIT. EXIT.


       3000-RMAP.
           EXEC CICS RECEIVE 
                MAP('SM002M')
                MAPSET('SM02S')
                INTO (SM002MI)
           END-EXEC.
       3000-EXIT. EXIT.

       4000-WRITE.
           EXEC CICS
            WRITE FILE('STF001C')
            FROM (NEW-TICKET)
            RIDFLD (NEW-TICKET-KEY)
           END-EXEC
      
           EXEC CICS
            WRITE FILE('STF002D')
            FROM (WS-LOG)
            RIDFLD (WS-LOG-Ticket-ID)
            RESP(WS-RETNCODE)
           END-EXEC.
       4000-EXIT. EXIT.    