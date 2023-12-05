         CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM002.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-MAP  VALUE 'SM002M'        PIC X(7).
       01  WS-TIME                               PIC 9(15).
       01  WS-DATE                               PIC 9(7).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(7).
       01  WS-LENGTH                             PIC S9(4) COMP. 
       01  WS-END                                PIC X(15) VALUE
           'INVALID ACCESS'.
       77 WS-RETNCODE                            PIC S9(8) COMP.
       77 TICKETADD                              PIC 9(06).
       77 REQUESTOR                              PIC X(01) VALUE 'Y'.

       01  TICKET.
           05 TICKET-KEY                         PIC X(07).
           05 TICKET-KEY-NUM REDEFINES TICKET-KEY PIC 9(06).
           05 TICKET-REQUESTOR                   PIC X(8).
           05 TICKET-STATUS                      PIC X(10).
           05 TICKET-TITLE                       PIC X(25).
           05 TICKET-DESCRIPTION                 PIC X(100).
           05 TICKET-LAST-UPDATE                 PIC X(20). 
           05 TICKET-LAST-UPDATE-BY              PIC X(8). 
           05 TICKET-UPDATE-REMARKS              PIC X(50).
       
       01  NEW-TICKET.
           05 NEW-TICKET-KEY                     PIC X(07).
           05 NEW-TICKET-KEY-NUM REDEFINES NEW-TICKET-KEY PIC 9(07).
           05 NEW-TICKET-REQUESTOR               PIC X(08).
           05 NEW-TICKET-STATUS                  PIC X(10) VALUE 
              'CREATED'.
           05 NEW-TICKET-TITLE                   PIC X(25).
           05 NEW-TICKET-DESCRIPTION             PIC X(100).
           05 NEW-TICKET-LAST-UPDATE             PIC X(20) VALUE SPACE. 
           05 NEW-TICKET-LAST-UPDATE-BY          PIC X(08) VALUE SPACE. 
           05 NEW-TICKET-UPDATE-REMARKS          PIC X(50) VALUE SPACE.

       01  WS-LOG. 
           05 WS-LOG-TICKET-ID          PIC X(07).  
           05 WS-LOG-SEQ-NUMBER         PIC 9(03) VALUE 1.
           05 WS-LOG-LAST-UPDATE        PIC X(20) VALUE SPACE. 
           05 WS-LOG-LAST-UPDATE-BY     PIC X(08) VALUE SPACE. 
           05 WS-LOG-UPDATE-REMARKS     PIC X(50) VALUE SPACE.
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
           05 WS-TITLE-REQUIRED                 PIC X(24) VALUE
              'TICKET TITLE IS REQUIRED'.
           05 WS-LAST-PAGE                       PIC X(21) VALUE
              'THIS IS THE LAST PAGE'.
           05 WS-INVALID-VALUE                   PIC X(46) VALUE
              'INVALID VALUE. PLEASE CORRECT HIGHLIGHT FIELDS'.   
           05 WS-INVALID-PFKEY                   PIC X(21) VALUE
               'INAVLID PFKEY PRESSED'.

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
           05 WS-FLAG                            PIC X(01).
           05 WS-HOLDER.
              10 WS-ID                          PIC X(06).
              10 WS-TITLE                       PIC X(25).
              10 WS-DES1                        PIC X(50).
              10 WS-DES2                        PIC X(50).

       COPY SM02S.
       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
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
           05 DF-FLAG                            PIC X(01).
           05 HOLDER.
              10 T-ID                            PIC X(06).
              10 T-TITLE                         PIC X(25).
              10 T-DES1                          PIC X(50).
              10 T-DES2                          PIC X(50).


       PROCEDURE DIVISION.
       100-MAIN.
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
                  RIDFLD(TICKET-KEY)
             END-EXEC
           END-IF

           MOVE DFHCOMMAREA  TO WS-COMMAREA

            IF WS-STATE = 'A' OR  
                WS-STATE  = 'B' OR 
                WS-STATE  =  SPACE
                CONTINUE
            ELSE
              MOVE SPACE TO WS-STATE
            END-IF

           IF WS-PGMID = 'SM000' OR WS-PGMID = 'SM001' OR
              WS-PGMID = 'SM002'
                IF USR-REQUESTOR = 'Y'
                   PERFORM 2000-CHECK-AID
                   EVALUATE TRUE
                     WHEN WS-STATE = SPACE
                       ADD  1 TO TICKET-KEY-NUM 
                       MOVE TICKET-KEY-NUM TO IDO
                       MOVE SPACE TO TICKETTO
                       MOVE SPACE TO TICKTD1O
                       MOVE SPACE TO TICKTD2O
                       MOVE -1 TO TICKETTL
 
                     WHEN WS-STATE = 'A'
                       PERFORM 3000-RMAP
 
                     WHEN WS-STATE = 'B'
                       PERFORM 3000-RMAP
 
                       IF WS-FLAG = 'Y'
                         MOVE IDI TO NEW-TICKET-KEY
                         MOVE USERID TO NEW-TICKET-REQUESTOR
                         MOVE TICKETTI TO NEW-TICKET-TITLE
                         MOVE USERID TO NEW-TICKET-LAST-UPDATE-BY
                         MOVE TICKTD1I TO NEW-TICKET-DESCRIPTION(1:50)
                         MOVE TICKTD2I TO NEW-TICKET-DESCRIPTION(51:50)
                         PERFORM 1100-GET-DATE-TIME
                         MOVE DATEO    TO NEW-TICKET-Last-Update(1:10)
                         MOVE TIMEO    TO NEW-TICKET-Last-Update(12:9)
                         MOVE IDI TO WS-LOG-TICKET-ID
                         MOVE DATEO    TO WS-LOG-LAST-UPDATE(1:10)
                         MOVE TIMEO    TO WS-LOG-LAST-UPDATE(12:8)
                         MOVE USERID TO WS-LOG-LAST-UPDATE-BY
                         PERFORM 4000-WRITE
                       END-IF
 
                       MOVE DFHBMASK TO STA
                       MOVE DFHBMASK TO UPMA
                       MOVE DFHBMASK TO UPDTA
                       MOVE DFHBMASK TO UPBYA
                       MOVE DFHBMPRO TO STATSA
                       MOVE DFHBMPRO TO TICKETTA
                       MOVE DFHBMPRO TO TICKTD1A
                       MOVE DFHBMPRO TO TICKTD2A
 
                   END-EVALUATE
                   PERFORM 2100-CHECK-AID-MSG
                   PERFORM 1000-NEW-MAP
                ELSE
                  PERFORM 110-INVALID
                END-IF
            ELSE
              PERFORM 110-INVALID
            END-IF.       
       100-EXIT. EXIT.
		  
       110-INVALID.
           MOVE LENGTH OF WS-INVALID-ACCESS TO WS-LENGTH
           EXEC CICS SEND TEXT
              FROM (WS-INVALID-ACCESS)
              LENGTH (WS-LENGTH)
              ERASE
           END-EXEC  
           EXEC CICS RETURN
           END-EXEC.
       110-EXIT. EXIT.
       
       1100-GET-DATE-TIME.
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
           END-EXEC.
       1100-EXIT.
           EXIT.

       1000-NEW-MAP.
           PERFORM 1100-GET-DATE-TIME
           MOVE DFHBMASB TO TIMEA
           MOVE DFHBMASB TO DATEA
           MOVE DFHBMASB TO ERRMSGA
           MOVE LENGTH OF SM002MO TO WS-LENGTH 
		       EXEC CICS SEND 
                MAP('SM002M')
			          MAPSET('SM02S')
			          FROM(SM002MO)
			          LENGTH(WS-LENGTH)
                CURSOR
                ERASE
			     END-EXEC.
           EXEC CICS RETURN
                TRANSID('SM02')
              	COMMAREA(WS-COMMAREA)
	         END-EXEC.
       1000-EXIT. EXIT.

       2000-CHECK-AID.
            EVALUATE TRUE
              WHEN EIBAID = DFHPF2
                IF WS-STATE = SPACE
                  MOVE 'A'  TO WS-STATE
                END-IF

                IF WS-STATE = 'A' AND WS-FLAG = 'Y'
                  MOVE 'B'  TO WS-STATE
                END-IF

                IF WS-STATE = 'B' AND WS-FLAG = 'N'
                  MOVE SPACE  TO WS-STATE
                END-IF
            
              WHEN EIBAID = DFHPF3
		    	         EVALUATE WS-PGMID
                    WHEN 'SM000'
                       MOVE LOW-VALUES TO WS-STATE
		    	             EXEC CICS XCTL
			                     PROGRAM('SM000')
                           COMMAREA(WS-COMMAREA)
                         END-EXEC  
                    WHEN 'SM001'  
                         EXEC CICS XCTL
			                     PROGRAM('SM000')
                         END-EXEC  
                   END-EVALUATE   

              WHEN EIBAID = DFHPF5
		    	      MOVE SPACE TO WS-STATE
                   
              WHEN EIBAID = DFHENTER
                IF WS-STATE = SPACE AND WS-FLAG = 'N'
                  MOVE 'A' TO WS-STATE 
                END-IF
                IF WS-STATE = 'B'
                  MOVE SPACE TO WS-STATE
                END-IF
              WHEN EIBAID = DFHCLEAR
                CONTINUE
              WHEN OTHER
                CONTINUE
            END-EVALUATE.
       2000-EXIT. EXIT.

       2100-CHECK-AID-MSG.
            MOVE IDI TO NEW-TICKET-KEY
            MOVE IDI TO WS-LOG-TICKET-ID
            MOVE TICKETTI TO NEW-TICKET-TITLE
            MOVE TICKTD1I TO NEW-TICKET-DESCRIPTION(1:50)
            MOVE TICKTD2I TO NEW-TICKET-DESCRIPTION(51:50)
            EVALUATE TRUE
              WHEN EIBAID = DFHPF2

                IF WS-STATE = SPACE AND WS-FLAG = 'N'
                  MOVE 'ENTER TICKET DETAILS AND PRESS PF2' 
                   TO ERRMSGO
                END-IF

                IF WS-STATE = 'A'
                  MOVE IDI  TO WS-ID
                  MOVE TICKETTI TO WS-TITLE
                  MOVE TICKTD1I TO WS-DES1
                  MOVE TICKTD2I TO WS-DES2 
                  EVALUATE TRUE
                    WHEN NEW-TICKET-TITLE = SPACE
                      MOVE WS-TITLE-REQUIRED TO ERRMSGO 
                      MOVE -1 TO TICKETTL

                    WHEN NEW-TICKET-DESCRIPTION = SPACE
                      MOVE 'TICKET DESCRIPTION IS REQUIRED' 
                       TO ERRMSGO 
                      MOVE -1 TO TICKTD1L

                    WHEN OTHER
                      MOVE 'Y' TO WS-FLAG
                      MOVE -1 TO TICKETTL
                      MOVE 'VALID INPUT PRESS PF2 TO ADD' 
                       TO ERRMSGO 
                  END-EVALUATE

                END-IF

                IF WS-STATE = 'B'
                  IF WS-FLAG = 'Y'
                    MOVE 'N' TO WS-FLAG
                  END-IF

                  IF WS-FLAG = 'N'
                    MOVE 'CREATED' TO STATSO
                    MOVE -1 TO TICKETTL
                    MOVE 'TICKET CREATED' TO UPR1O 
                    MOVE NEW-TICKET-Last-Update(1:10) TO UPDATEO
                    MOVE NEW-TICKET-Last-Update(12:9) TO UPTIMEO
                    MOVE NEW-TICKET-Last-Update-by TO UPBO
                    MOVE 'SUCCESSFULLY CREATED. PF2 TO ADD MORE'
                       TO ERRMSGO 
                  END-IF
                END-IF

              WHEN EIBAID = DFHENTER
                MOVE IDI  TO WS-ID
                MOVE TICKETTI TO WS-TITLE
                MOVE TICKTD1I TO WS-DES1
                MOVE TICKTD2I TO WS-DES2 
                EVALUATE TRUE
                WHEN WS-STATE = SPACE AND WS-FLAG NOT = 'N'
                  MOVE 'N' TO WS-FLAG
                  MOVE 'ENTER TICKET DETAILS AND PRESS PF2' 
                   TO ERRMSGO

                WHEN WS-STATE = SPACE AND WS-FLAG = 'N'
                  MOVE 'ENTER TICKET DETAILS AND PRESS PF2' 
                   TO ERRMSGO
                
                WHEN OTHER
                   EVALUATE TRUE
                    WHEN NEW-TICKET-TITLE = SPACE
                      MOVE 'TICKET TITLE IS REQUIRED' 
                       TO ERRMSGO 
                      MOVE -1 TO TICKETTL
                      MOVE DFHUNIMD TO TICKETTA

                    WHEN NEW-TICKET-DESCRIPTION = SPACE
                      MOVE 'TICKET DESCRIPTION IS REQUIRED' 
                       TO ERRMSGO 
                      MOVE -1 TO TICKTD1L
                      MOVE DFHUNIMD TO TICKTD1A

                    WHEN OTHER
                      MOVE 'Y' TO WS-FLAG
                      MOVE 'VALID INPUT PRESS PF2 TO ADD' 
                       TO ERRMSGO 
                  END-EVALUATE
                END-EVALUATE

              WHEN EIBAID = DFHPF5
		    	      IF WS-STATE = SPACE
                  MOVE 'ENTER TICKET DETAILS AND PRESS PF2' 
                  TO ERRMSGO
                ELSE
                  MOVE 'TRANSACTION IS ABORTED' TO ERRMSGO  
                END-IF

              WHEN EIBAID = DFHCLEAR
                IF WS-STATE = 'B'
                  MOVE TICKET-KEY TO IDO
                  MOVE TICKET-TITLE TO TICKETTO
                  MOVE TICKET-DESCRIPTION(1:50) TO TICKTD1O
                  MOVE TICKET-DESCRIPTION(51:50) TO TICKTD2O
                  MOVE 'CREATED' TO STATSO
                  MOVE 'TICKET CREATED' TO UPR1O 
                  MOVE TICKET-Last-Update(1:10) TO UPDATEO
                  MOVE TICKET-Last-Update(11:10) TO UPTIMEO
                  MOVE TICKET-Last-Update-by TO UPBO
                  MOVE 'SUCCESSFULLY CREATED. PF2 TO ADD MORE'
                    TO ERRMSGO 
                END-IF

                IF WS-STATE = 'A'
                    MOVE T-ID     TO IDO
                    MOVE T-TITLE  TO TICKETTO
                    MOVE T-DES1   TO TICKTD1O
                    MOVE T-DES2   TO TICKTD2O
                    MOVE -1 TO TICKETTL
                    MOVE WS-INVALID-PFKEY TO ERRMSGO
                END-IF

                IF WS-STATE = SPACE
                    MOVE TICKET-KEY-NUM TO IDO
                    MOVE SPACE TO TICKETTO
                    MOVE SPACE TO TICKTD1O
                    MOVE SPACE TO TICKTD2O
                    MOVE 'ENTER TICKET DETAILS AND PRESS PF2' 
                        TO ERRMSGO
                    MOVE -1 TO TICKETTL 
                END-IF

              WHEN OTHER
                EVALUATE TRUE
                  WHEN WS-STATE = SPACE
                    MOVE TICKET-KEY-NUM TO IDO
                    MOVE SPACE TO TICKETTO
                    MOVE SPACE TO TICKTD1O
                    MOVE SPACE TO TICKTD2O

                  WHEN WS-STATE = 'A'
                    MOVE NEW-TICKET-KEY TO IDO
                    MOVE NEW-TICKET-TITLE TO TICKETTO
                    MOVE NEW-TICKET-DESCRIPTION(1:50) TO TICKTD1O
                    MOVE NEW-TICKET-DESCRIPTION(51:50) TO TICKTD2O

                  WHEN WS-STATE = 'B'
                    MOVE TICKET-KEY TO IDO
                    MOVE TICKET-TITLE TO TICKETTO
                    MOVE TICKET-DESCRIPTION(1:50) TO TICKTD1O
                    MOVE TICKET-DESCRIPTION(51:50) TO TICKTD2O
                    MOVE 'CREATED' TO STATSO
                    MOVE 'TICKET CREATED' TO UPR1O 
                    MOVE TICKET-Last-Update(1:10) TO UPDATEO
                    MOVE TICKET-Last-Update(11:10) TO UPTIMEO
                    MOVE TICKET-Last-Update-by TO UPBO

                END-EVALUATE
                MOVE WS-INVALID-PFKEY TO ERRMSGO 
            END-EVALUATE.
       2100-EXIT. EXIT.


       3000-RMAP.
           EXEC CICS RECEIVE
                MAP('SM002M')
                MAPSET('SM02S')
                INTO (SM002MI)
           END-EXEC.
       3000-EXIT. EXIT.

       4000-WRITE.
           EXEC CICS WRITE 
                FILE('STF001C')
                FROM (NEW-TICKET)
                RIDFLD (NEW-TICKET-KEY)
                RESP(WS-RETNCODE)
           END-EXEC
           
           MOVE 'TICKET CREATED' TO WS-LOG-UPDATE-REMARKS
           MOVE USERID TO WS-LOG-LAST-UPDATE-BY
           MOVE NEW-TICKET-LAST-UPDATE TO WS-LOG-LAST-UPDATE

           EXEC CICS WRITE 
                FILE('STF002D')
                FROM (WS-LOG)
                RIDFLD (WS-LOG-TICKET-ID)
                RESP(WS-RETNCODE)
           END-EXEC.
       4000-EXIT. EXIT.

         

