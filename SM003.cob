         CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM003.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-MAP  VALUE 'SM03S'         PIC X(7).
       01  WS-TIME				 PIC 9(15).
       01  WS-DATE                               PIC 9(7).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(7).
       01  WS-LENGTH                             PIC S9(4) COMP. 
  

       01  WS-END                                PIC X(15) VALUE
           'INVALID ACCESS'.
       77 WS-RETNCODE                            PIC S9(8) COMP.

      * PRE-DEFINE DATA DAPAT MGA ITO AY 
      * GALING SA COMMAREA FROM PREVIOUS MAP
       01 DAPAT-COMMAREA.
          05 TICKETADD                      PIC X(07) VALUE '000006'.
          05 REQUESTOR                      PIC X(01) VALUE 'Y'.
      *   05 USERID                         PIC X(07) VALUE 'ISCB110'.


       01  TICKET.
           05 TICKET-KEY                         PIC X(07).
           05 TICKET-KEY-NUM REDEFINES TICKET-KEY PIC 9(06).
           05 TICKET-Requestor                   PIC X(8).
           05 TICKET-Status                      PIC X(10).
           05 TICKET-Title                       PIC X(25).
           05 TICKET-Description                 PIC X(100).
           05 TICKET-Last-Update                 PIC X(20). 
           05 TICKET-Last-Update-by              PIC X(8). 
           05 TICKET-Update-Remarks              PIC X(50).
       
       01  NEW-TICKET.
           05 NEW-TICKET-KEY                     PIC X(07).
           05 NEW-TICKET-KEY-NUM REDEFINES NEW-TICKET-KEY PIC 9(07).
           05 NEW-TICKET-Requestor               PIC X(08).
           05 NEW-TICKET-Status                  PIC X(10).
           05 NEW-TICKET-Title                   PIC X(25).
           05 NEW-TICKET-Description             PIC X(100).
           05 NEW-TICKET-Last-Update             PIC X(20). 
           05 NEW-TICKET-Last-Update-by          PIC X(08). 
           05 NEW-TICKET-Update-Remarks          PIC X(50).

       01  WS-LOG. 
           05 WS-KEYS.
               10 WS-LOG-Ticket-ID               PIC X(06).  
               10 WS-LOG-Seq-Number              PIC 9(03).
           05 WS-LOG-Last-Update                 PIC X(20). 
           05 WS-LOG-Last-Update-by              PIC X(10). 
           05 WS-LOG-Update-Remarks              PIC X(50).

       01  WS-LOG-BACKUP. 
           05 WS-B-KEYS.
               10 WS-B-LOG-Ticket-ID             PIC X(06).  
               10 WS-B-LOG-Seq-Number            PIC 9(03).
           05 WS-B-LOG-Last-Update               PIC X(20). 
           05 WS-B-LOG-Last-Update-by            PIC X(10). 
           05 WS-B-LOG-Update-Remarks            PIC X(50).

       01  CHECK-HOLDER.
           05 WS-C-TITLE                         PIC X(25).
           05 WS-C-DES                           PIC X(100).
           05 WS-C-UPDATE                        PIC X(50).

       01  HOLDER                                PIC X(50).


       COPY SM03S.
       COPY DFHAID.
       COPY DFHBMSCA.
       
       01  WS-COMMAREA. 
           05 WS-PGMID                           PIC X(06).
           05 WS-STATE                           PIC X.
           05 WS-TICKET-PASSED                   PIC X(07).
           05 USERID.
              10  USERID7                        PIC X(7).
              10  FILLER                         PIC X(1).
           05 USR-TYPE.
              15 USR-REQUESTOR                   PIC X.
              15 USR-ADMIN                       PIC X.  
              15 USR-APPROVER                    PIC X.
              15 USR-SERVICE                     PIC X.
           05 WS-TICKET-NUM                      PIC X(07).
           05 WS-FLAG                            PIC X(01).
           05 ESC-BACKUP.              
              10 WS-E-TITLE                      PIC X(25).
              10 WS-E-DES                        PIC X(100).
              10 WS-E-STATS                      PIC X(10).
              10 WS-E-UPDATE                     PIC X(50).  

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
           05 DF-TICKET-NUM                      PIC X(07).
           05 DF-FLAG                            PIC X(01).
           05 DF-ESC-BACKUP.              
             10 DF-E-TITLE                       PIC X(25).
             10 DF-E-DES                         PIC X(100).
             10 DF-E-STATS                       PIC X(10).
             10 DF-E-UPDATE                      PIC X(50).

       PROCEDURE DIVISION.
       100-MAIN.
           EXEC CICS IGNORE CONDITION
                     ERROR
           END-EXEC     

           MOVE DFHCOMMAREA  TO WS-COMMAREA

           IF WS-STATE  = 'A' OR  
               WS-STATE   = 'B' OR 
               WS-STATE   = 'C' OR
               WS-STATE   =  SPACE
              CONTINUE
           ELSE
              MOVE SPACE TO WS-STATE
           END-IF

           IF WS-TICKET-PASSED  NOT = SPACE AND WS-STATE = SPACE
              MOVE 'A' TO WS-STATE
              MOVE WS-TICKET-PASSED  TO WS-TICKET-NUM
           END-IF

           IF WS-PGMID = 'SM000' OR WS-PGMID = 'SM001' OR
              WS-PGMID = 'SM012'
              PERFORM 4000-RMAP
              IF IDI NOT = NULL
                MOVE IDI TO WS-TICKET-NUM
              END-IF
              PERFORM 2000-CHECK-AID

              EVALUATE TRUE
                WHEN WS-STATE = SPACE
                  MOVE DFHBMUNP TO IDA
                  MOVE -1 TO IDL
                  MOVE DFHBMPRO TO TICKETTA
                  MOVE DFHBMPRO TO TICKTD1A
                  MOVE DFHBMPRO TO TICKTD2A

                WHEN WS-STATE = 'A'
                  PERFORM 3000-READ-RECORD

                WHEN WS-STATE = 'B'
                  MOVE -1 TO TICKETTL

                WHEN WS-STATE = 'C'
                 PERFORM 3200-READ-ONLY
                 PERFORM 2110-STATEA-FLAGY
                 MOVE DFHBMPRO TO IDA
                 MOVE DFHBMPRO TO TICKETTA
                 MOVE DFHBMPRO TO TICKTD1A
                 MOVE DFHBMPRO TO TICKTD2A
                 MOVE DFHBMPRO TO REQA
                 MOVE DFHBMPRO TO STATSA
                 MOVE DFHBMPRO TO UPBA
                 MOVE DFHBMPRO TO UPR1A
                 MOVE DFHBMPRO TO UPR2A
                 MOVE DFHBMPRO TO UPDATEA
                 MOVE DFHBMPRO TO UPTIMEA
                 MOVE -1 TO IDL

              END-EVALUATE

              PERFORM 2100-CHECK-AID-MSG
              PERFORM 1000-NEW-MAP
           ELSE
              PERFORM 110-INVALID
           END-IF.
       100-EXIT. EXIT.

       110-INVALID.
           EXEC CICS SEND TEXT
              FROM (WS-END)
              LENGTH (+15)
              ERASE
           END-EXEC  
           EXEC CICS RETURN
           END-EXEC.
       110-EXIT. EXIT.
      
       1000-NEW-MAP.
           MOVE 'SM003' TO MAPTO
           MOVE EIBDATE TO WS-DATE
           MOVE WS-DATE-X TO DATEO
           EXEC CICS ASKTIME
             ABSTIME    (WS-TIME)
           END-EXEC
           EXEC CICS FORMATTIME
             ABSTIME    (WS-TIME)
             DATESEP    ('-')
             MMDDYYYY   (DATEO)
             TIME       (TIMEO)
             TIMESEP    (':')
           END-EXEC
           MOVE DFHBMASB TO MAPTA
           MOVE DFHBMASB TO TIMEA
           MOVE DFHBMASB TO DATEA
           MOVE DFHBMASB TO ERRMSGA
           MOVE DFHBMASK TO STA
           MOVE DFHBMASK TO UPMA
           MOVE DFHBMASK TO UPDTA
           MOVE LENGTH OF SM003MO TO WS-LENGTH 
		       EXEC CICS
			        SEND MAP('SM003M')
			        MAPSET('SM03S')
			        FROM(SM003MO)
			        LENGTH(WS-LENGTH)
              CURSOR
              ERASE
			     END-EXEC
           EXEC CICS RETURN
              TRANSID('SM03')
            	COMMAREA(WS-COMMAREA)
              LENGTH(207)
	         END-EXEC.
       1000-EXIT. EXIT.  

       2000-CHECK-AID.
           EVALUATE TRUE
            WHEN EIBAID = DFHPF2
              IF WS-STATE = 'B' AND  WS-FLAG = 'V'
               PERFORM 3000-READ-RECORD
               IF WS-E-TITLE = NEW-TICKET-Title AND
                  WS-E-DES = NEW-TICKET-Description AND
                  WS-E-STATS = NEW-TICKET-Status AND
                  WS-E-UPDATE = NEW-TICKET-Update-Remarks

                  MOVE 'W'  TO WS-FLAG
               ELSE
                  MOVE 'K' TO WS-FLAG

               END-IF

              END-IF

            WHEN EIBAID = DFHPF3
             IF WS-STATE = 'C' OR 'A'
               EVALUATE WS-PGMID
                 WHEN 'SM000'
                      EXEC CICS XCTL
			                   PROGRAM('SM000')
			                   COMMAREA(WS-COMMAREA)
			                   LENGTH(133) 
                      END-EXEC  
                 WHEN 'SM001' 
                      EXEC CICS XCTL
			                   PROGRAM('SM001')
			                   COMMAREA(WS-COMMAREA)
			                   LENGTH(133) 
                      END-EXEC  
                 WHEN 'SM012'
                       EXEC CICS XCTL
			                   PROGRAM('SM001')
			                   COMMAREA(WS-COMMAREA)
			                   LENGTH(133) 
                      END-EXEC
               END-EVALUATE            
             END-IF

            WHEN EIBAID = DFHPF5
              IF WS-STATE = 'B' AND WS-FLAG = 'Y'
                MOVE SPACE TO WS-STATE
                MOVE NULL TO WS-TICKET-NUM 
      *         MOVE NULL TO MAP3I
              END-IF

            WHEN EIBAID = DFHENTER
              IF WS-STATE = SPACE
                IF WS-TICKET-NUM = NULL
                  CONTINUE
                ELSE
                  MOVE 'A' TO WS-STATE
                END-IF
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'Y'
                  MOVE 'N' TO WS-FLAG
              END-IF

              IF WS-STATE = 'C' AND WS-FLAG = 'N'
                 MOVE SPACE TO WS-STATE
                 MOVE NULL TO WS-TICKET-NUM 
      *          MOVE NULL TO MAP3I
              END-IF

            WHEN EIBAID = DFHCLEAR
              CONTINUE

            WHEN EIBAID = DFHPF11
              CONTINUE

            WHEN OTHER
              CONTINUE

           END-EVALUATE.
       2000-EXIT. EXIT.   

       2100-CHECK-AID-MSG.
           EVALUATE TRUE
            WHEN EIBAID = DFHPF2
              IF WS-STATE = SPACE
                MOVE 'DISABLED PFKEY PRESSED' TO ERRMSGO
              END-IF

              IF WS-STATE = 'A'
                MOVE -1 TO IDL
                MOVE DFHBMUNP TO IDA
                MOVE WS-TICKET-NUM TO IDO
                MOVE 'DISABLED PFKEY PRESSED' TO ERRMSGO
              END-IF

              EVALUATE TRUE

              WHEN WS-STATE = 'B' AND WS-FLAG = 'W'
                MOVE -1 TO TICKETTL
                MOVE 'NO CHANGES' TO ERRMSGO
                MOVE 'N' TO WS-FLAG
                PERFORM 2120-STATEB

              WHEN WS-STATE = 'B' AND WS-FLAG = 'N'
                PERFORM 3000-READ-RECORD
                PERFORM 2120-STATEB
                MOVE 'PRESS ENTER TO VALIDATE' TO ERRMSGO

             WHEN WS-STATE = 'B' AND WS-FLAG = 'Y'
                PERFORM 3000-READ-RECORD
                PERFORM 2120-STATEB
                MOVE 'PRESS ENTER TO VALIDATE' TO ERRMSGO
           
              END-EVALUATE

              IF WS-STATE = 'C' AND WS-FLAG = 'N'
                 MOVE 'DISABLED PFKEY PRESSED' TO ERRMSGO
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'K'
                 PERFORM 3500-CHECK-EDIT
                 PERFORM 3100-UPDATE-FILE
                 MOVE WS-TICKET-NUM TO WS-LOG-Ticket-ID
                 MOVE 0 TO WS-LOG-Seq-Number 
                 PERFORM 3400-CREATE-LOG
                 PERFORM 2110-STATEA-FLAGY
                 MOVE DFHBMPRO TO IDA
                 MOVE DFHBMPRO TO TICKETTA
                 MOVE DFHBMPRO TO TICKTD1A
                 MOVE DFHBMPRO TO TICKTD2A
                 MOVE DFHBMPRO TO REQA
                 MOVE DFHBMPRO TO STATSA
                 MOVE DFHBMPRO TO UPBA
                 MOVE DFHBMPRO TO UPR1A
                 MOVE DFHBMPRO TO UPR2A
                 MOVE DFHBMPRO TO UPDATEA
                 MOVE DFHBMPRO TO UPTIMEA
                 
             MOVE 'TICKET UPDATED, PRESS ENTER TO UPDATE ANOTHER TICKET'
               TO ERRMSGO
                 MOVE 'N' TO WS-FLAG
                 MOVE 'C' TO WS-STATE
             
              END-IF

            WHEN EIBAID = DFHPF5
              IF WS-STATE = SPACE
                MOVE 'DISABLED PFKEY PRESSED' TO ERRMSGO
              END-IF

              IF WS-STATE = SPACE AND WS-FLAG = 'Y'
                MOVE 'UPDATE ABORTED' TO ERRMSGO
                MOVE NULL TO WS-FLAG
              END-IF

              IF WS-STATE = 'A'
                MOVE -1 TO IDL
                MOVE DFHBMUNP TO IDA
                MOVE WS-TICKET-NUM TO IDO
                MOVE 'DISABLED PFKEY PRESSED' TO ERRMSGO
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'N'
                PERFORM 3000-READ-RECORD
                PERFORM 2110-STATEA-FLAGY
                MOVE 'UPDATE ABORTED' TO ERRMSGO
                MOVE 'Y' TO WS-FLAG
                MOVE DFHBMPRO TO IDA
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'V'
                PERFORM 3000-READ-RECORD
                PERFORM 2110-STATEA-FLAGY
                MOVE 'N' TO WS-FLAG
                MOVE 'UPDATE ABORTED' TO ERRMSGO
                MOVE DFHBMPRO TO IDA
              END-IF

              IF WS-STATE = 'C' AND WS-FLAG = 'N'
                 MOVE 'DISABLED PFKEY PRESSED' TO ERRMSGO
              END-IF

            WHEN EIBAID = DFHENTER
              IF WS-STATE = SPACE AND WS-TICKET-NUM = NULL
                MOVE 'ENTER TICKET NUMBER AND PRESS ENTER' TO ERRMSGO
              END-IF

              IF WS-STATE = 'A' AND WS-FLAG = 'N'
                MOVE -1 TO IDL
                MOVE DFHBMUNP TO IDA
                MOVE WS-TICKET-NUM TO IDO
                MOVE 'TICKET NUMBER DOES NOT EXIST' TO ERRMSGO
              END-IF

              IF WS-STATE = 'A' AND WS-FLAG = 'S'
                MOVE -1 TO IDL
                MOVE DFHBMUNP TO IDA
                MOVE WS-TICKET-NUM TO IDO
                MOVE 'INVALID TICKET STATUS' TO ERRMSGO
              END-IF

              IF WS-STATE = 'A' AND WS-FLAG = 'R'
                MOVE -1 TO IDL
                MOVE DFHBMUNP TO IDA
                MOVE WS-TICKET-NUM TO IDO
                MOVE 'INVALID ACCESS TO THE TICKET' TO ERRMSGO
              END-IF

              IF WS-STATE = 'A' AND WS-FLAG = 'Y'
                PERFORM 2110-STATEA-FLAGY
                MOVE 'B' TO WS-STATE
                MOVE DFHBMPRO TO IDA
                MOVE -1 TO TICKETTL
                MOVE 'ENTER UPDATES' TO ERRMSGO
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'V'
                PERFORM 3000-READ-RECORD
                PERFORM 2120-STATEB
                MOVE 'PRESS PF2 TO UPDATE TICKET' TO ERRMSGO
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'N'
                EVALUATE TRUE
                  WHEN TICKETTI = SPACE
                    MOVE -1 TO TICKETTL
                    MOVE 'TICKET TITLE IS REQUIRED' TO ERRMSGO
                  WHEN TICKTD1I = SPACE
                    MOVE -1 TO TICKTD1L
                    MOVE 'DESCRIPTION IS REQUIRED' TO ERRMSGO
                  WHEN STATSI  NOT = 'ONGOING' AND 'COMPLETED'
                    MOVE -1 TO STATSL
                    MOVE 'STATUS IS NOT VALID' TO ERRMSGO
                  WHEN UPR1I = SPACE
                    MOVE -1 TO UPR1L
                    MOVE 'UPDATE REMARKS IS REQUIRED' TO ERRMSGO
                  WHEN OTHER 
                    MOVE 'PRESS PF2 TO UPDATE TICKET' TO ERRMSGO
                    MOVE 'V' TO WS-FLAG
                END-EVALUATE
                MOVE TICKETTI TO WS-E-TITLE
                MOVE TICKTD1I TO WS-E-DES(1:50)
                MOVE TICKTD2I TO WS-E-DES(51:50)
                MOVE STATSI TO WS-E-STATS
                MOVE UPR1I TO WS-E-UPDATE(1:25)
                MOVE UPR2I TO WS-E-UPDATE(26:25)
                PERFORM 3000-READ-RECORD
                PERFORM 2120-STATEB
              END-IF

            WHEN EIBAID = DFHCLEAR
              IF WS-STATE = 'A'
                MOVE -1 TO IDL
                MOVE DFHBMUNP TO IDA
                MOVE WS-TICKET-NUM TO IDO
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'N'
                PERFORM 3000-READ-RECORD
                PERFORM 2110-STATEA-FLAGY
                MOVE DFHBMPRO TO IDA
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'Y'
                PERFORM 3000-READ-RECORD
                PERFORM 2110-STATEA-FLAGY
                MOVE DFHBMPRO TO IDA
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'V'
                PERFORM 3000-READ-RECORD
                PERFORM 2120-STATEB
                MOVE WS-E-TITLE TO TICKETTO
                MOVE WS-E-DES(1:50) TO TICKTD1O
                MOVE WS-E-DES(51:50) TO TICKTD2O
                MOVE WS-E-STATS TO STATSO
                MOVE WS-E-UPDATE(1:25) TO UPR1O
                MOVE WS-E-UPDATE(26:25) TO UPR2O
              END-IF
               
              MOVE 'INVALID PFKEY PRESSED' TO ERRMSGO

            WHEN EIBAID = DFHPF11
              MOVE 'GO TO LOG' TO ERRMSGO

            WHEN OTHER
              IF WS-STATE = 'A'
                MOVE -1 TO IDL
                MOVE DFHBMUNP TO IDA
                MOVE WS-TICKET-NUM TO IDO
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'N'
                PERFORM 3000-READ-RECORD
                PERFORM 2110-STATEA-FLAGY
                MOVE DFHBMPRO TO IDA
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'Y'
                PERFORM 3000-READ-RECORD
                PERFORM 2110-STATEA-FLAGY
                MOVE DFHBMPRO TO IDA
              END-IF

              IF WS-STATE = 'B' AND WS-FLAG = 'V'
                PERFORM 3000-READ-RECORD
                PERFORM 2120-STATEB
                MOVE WS-E-TITLE TO TICKETTO
                MOVE WS-E-DES(1:50) TO TICKTD1O
                MOVE WS-E-DES(51:50) TO TICKTD2O
                MOVE WS-E-STATS TO STATSO
                MOVE WS-E-UPDATE(1:25) TO UPR1O
                MOVE WS-E-UPDATE(26:25) TO UPR2O
              END-IF


              MOVE 'INVALID PFKEY PRESSED' TO ERRMSGO

           END-EVALUATE.
       2100-EXIT. EXIT.

       2110-STATEA-FLAGY.
            MOVE NEW-TICKET-KEY TO IDO
            MOVE NEW-TICKET-Title TO TICKETTO
            MOVE NEW-TICKET-Description(1:50) TO TICKTD1O
            MOVE NEW-TICKET-Description(51:50) TO TICKTD2O
            MOVE NEW-TICKET-Requestor TO REQO
            MOVE NEW-TICKET-Status  TO STATSO
            MOVE NEW-TICKET-Last-Update-by TO UPBO
            MOVE NEW-TICKET-Update-Remarks(1:25) TO UPR1O
            MOVE NEW-TICKET-Update-Remarks(26:25) TO UPR2O
            MOVE NEW-TICKET-Last-Update(1:10) TO UPDATEO
            MOVE NEW-TICKET-Last-Update(12:09) TO  UPTIMEO.
       2110-EXIT. EXIT.

       2120-STATEB.
            MOVE NEW-TICKET-KEY TO IDO
            MOVE NEW-TICKET-Requestor TO REQO
            MOVE NEW-TICKET-Last-Update-by TO UPBO
            MOVE NEW-TICKET-Last-Update(1:10) TO UPDATEO
            MOVE NEW-TICKET-Last-Update(12:09) TO  UPTIMEO 
            MOVE DFHBMPRO TO IDA.
       2120-EXIT. EXIT.

      

       3000-READ-RECORD.
           EXEC CICS STARTBR 
                FILE('STF001C')
                RIDFLD (WS-TICKET-NUM)
                GTEQ
           END-EXEC
           IF EIBRESP = +0
            EXEC CICS
                 READ FILE('STF001C')
                 INTO (NEW-TICKET)
                 RIDFLD (WS-TICKET-NUM)
                 EQUAL
                 UPDATE
            END-EXEC
            IF NEW-TICKET-Status = 'APPROVED' OR 'ONGOING'
                IF WS-STATE = 'A'
      ****I FFIX PA YUN REQUESTOR GALING SA COMMAREA
      ***************REQUESTOR USER
                  IF USR-REQUESTOR = 'Y'
                    IF USERID = NEW-TICKET-Requestor
                      MOVE 'Y' TO WS-FLAG
                    ELSE
                      MOVE 'R' TO WS-FLAG
                    END-IF
                  END-IF
      ***************SERVICE PROVIDER USER
                  IF USR-SERVICE = 'Y'
                    MOVE 'Y' TO WS-FLAG
                  END-IF
                END-IF
            ELSE
              MOVE 'S' TO WS-FLAG
            END-IF
           ELSE
            MOVE 'N' TO WS-FLAG
           END-IF.
       3000-EXIT. EXIT.

       3100-UPDATE-FILE.
           MOVE WS-E-TITLE TO NEW-TICKET-Title
           MOVE WS-E-DES TO NEW-TICKET-Description
           MOVE WS-E-STATS TO NEW-TICKET-Status
           MOVE WS-E-UPDATE TO NEW-TICKET-Update-Remarks
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
           MOVE DATEO    TO NEW-TICKET-Last-Update(1:10)
           MOVE TIMEO    TO NEW-TICKET-Last-Update(12:09)
           MOVE USERID   TO NEW-TICKET-Last-Update-by
           EXEC CICS
             REWRITE FILE ('STF001C')
             FROM (NEW-TICKET)
             RESP(WS-RETNCODE)
           END-EXEC.

       3100-EXIT. EXIT.

       3200-READ-ONLY.
           EXEC CICS STARTBR 
              FILE('STF001C')
              RIDFLD (WS-TICKET-NUM)
              GTEQ
           END-EXEC
           IF EIBRESP = +0
            EXEC CICS
              READ FILE('STF001C')
              INTO (NEW-TICKET)
              RIDFLD (WS-TICKET-NUM)
              EQUAL
              UPDATE
            END-EXEC.
       3200-EXIT. EXIT.

       3300-READ-LOG.

           EXEC CICS STARTBR 
              FILE('STF002E')
              RIDFLD (WS-KEYS)
              GTEQ
           END-EXEC

           MOVE WS-LOG TO WS-LOG-BACKUP
           
           PERFORM UNTIL WS-LOG-Ticket-ID NOT = WS-B-LOG-Ticket-ID 
             EXEC CICS READ
               FILE('STF002E')
               INTO(WS-LOG)
               RIDFLD(WS-KEYS)
               GTEQ
             END-EXEC
             IF WS-LOG-Ticket-ID  = WS-B-LOG-Ticket-ID 
               MOVE WS-LOG TO WS-LOG-BACKUP
               ADD 1 TO WS-LOG-Seq-Number 
             END-IF 
             
           END-PERFORM

           EXEC CICS ENDBR
             FILE('STF002E')
           END-EXEC.

       3300-EXIT. EXIT.

       3400-CREATE-LOG.
           PERFORM 3300-READ-LOG
           MOVE WS-LOG-BACKUP TO WS-LOG
           ADD 1 TO WS-LOG-Seq-Number 
      **********ID GALING SA COMMAREA
           MOVE USERID TO WS-LOG-Last-Update-by
           MOVE NEW-TICKET-Last-Update-by  TO WS-LOG-Last-Update-by
           MOVE HOLDER TO WS-LOG-Update-Remarks
           EXEC CICS
                WRITE FILE('STF002E')
                FROM (WS-LOG)
                RIDFLD (WS-KEYS)
                RESP(WS-RETNCODE)
           END-EXEC

           ADD 1 TO WS-LOG-Seq-Number 

           PERFORM 3600-CHECK-NEW
           MOVE HOLDER TO WS-LOG-Update-Remarks

           EXEC CICS
                WRITE FILE('STF002E')
                FROM (WS-LOG)
                RIDFLD (WS-KEYS)
                RESP(WS-RETNCODE)
           END-EXEC.


       3400-EXIT. EXIT.

       3500-CHECK-EDIT.
           
           IF WS-E-TITLE NOT = NEW-TICKET-Title
             MOVE WS-E-TITLE TO WS-C-TITLE
           END-IF

           IF WS-E-DES NOT = NEW-TICKET-Description
               MOVE WS-E-DES TO WS-C-DES 
           END-IF

           IF WS-E-UPDATE NOT = NEW-TICKET-Update-Remarks
                 MOVE WS-E-UPDATE TO WS-C-UPDATE 
           END-IF.

           IF NEW-TICKET-Status = 'COMPLETED'
               EVALUATE TRUE
               WHEN WS-C-TITLE NOT = NULL AND 
                    WS-C-DES NOT = NULL AND
                    WS-C-UPDATE NOT = NULL
                    MOVE 'COMPLETED. TITLE/DESCRIPTION/REMARKS UPDATED'
                      TO HOLDER
               
               WHEN WS-C-TITLE NOT = NULL AND 
                    WS-C-DES   NOT = NULL AND
                    WS-C-UPDATE    = NULL
                    MOVE 'COMPLETED. TITLE/DESCRIPTION UPDATED'
                      TO HOLDER

               WHEN WS-C-TITLE NOT = NULL AND 
                    WS-C-DES       = NULL AND
                    WS-C-UPDATE    = NULL
                    MOVE 'COMPLETED. TITLE UPDATED' TO HOLDER

               WHEN WS-C-TITLE      = NULL AND 
                    WS-C-DES    NOT  = NULL AND
                    WS-C-UPDATE NOT = NULL
                    MOVE 'COMPLETED. DESCRIPTION/REMARKS UPDATED'
                     TO HOLDER

              WHEN WS-C-TITLE       = NULL AND 
                    WS-C-DES        = NULL AND
                    WS-C-UPDATE NOT = NULL
                    MOVE 'COMPLETED. REMARKS UPDATED' TO HOLDER

              WHEN WS-C-TITLE       = NULL AND 
                    WS-C-DES   NOT  = NULL AND
                    WS-C-UPDATE     = NULL
                    MOVE 'COMPLETED. DESCRIPTION UPDATED' TO HOLDER
               
               WHEN WS-C-TITLE NOT  = NULL AND 
                    WS-C-DES        = NULL AND
                    WS-C-UPDATE NOT = NULL
                    MOVE 'COMPLETED. TITLE/REMARKS UPDATED' TO HOLDER
               END-EVALUATE
           ELSE
               EVALUATE TRUE
               WHEN WS-C-TITLE NOT = NULL AND 
                    WS-C-DES NOT = NULL AND
                    WS-C-UPDATE NOT = NULL
                    MOVE 'TITLE/DESCRIPTION/REMARKS UPDATED' TO HOLDER
               
               WHEN WS-C-TITLE NOT = NULL AND 
                    WS-C-DES   NOT = NULL AND
                    WS-C-UPDATE    = NULL
                    MOVE 'TITLE/DESCRIPTION UPDATED' TO HOLDER

               WHEN WS-C-TITLE NOT = NULL AND 
                    WS-C-DES       = NULL AND
                    WS-C-UPDATE    = NULL
                    MOVE 'TITLE UPDATED' TO HOLDER

               WHEN WS-C-TITLE      = NULL AND 
                    WS-C-DES    NOT  = NULL AND
                    WS-C-UPDATE NOT = NULL
                    MOVE 'DESCRIPTION/REMARKS UPDATED' TO HOLDER

              WHEN WS-C-TITLE       = NULL AND 
                    WS-C-DES        = NULL AND
                    WS-C-UPDATE NOT = NULL
                    MOVE 'REMARKS UPDATED' TO HOLDER

              WHEN WS-C-TITLE       = NULL AND 
                    WS-C-DES   NOT  = NULL AND
                    WS-C-UPDATE     = NULL
                    MOVE 'DESCRIPTION UPDATED' TO HOLDER
               
               WHEN WS-C-TITLE NOT  = NULL AND 
                    WS-C-DES        = NULL AND
                    WS-C-UPDATE NOT = NULL
                    MOVE 'TITLE/REMARKS UPDATED' TO HOLDER

               END-EVALUATE
           END-IF.
       3500-EXIT. EXIT.

       3600-CHECK-NEW.
           EVALUATE TRUE
               WHEN WS-C-TITLE NOT = NULL AND 
                    WS-C-DES NOT = NULL AND
                    WS-C-UPDATE NOT = NULL
                 STRING WS-C-TITLE DELIMITED BY ' '
                   ' ' DELIMITED BY SIZE
                   WS-C-DES DELIMITED BY ' '
                   ' ' DELIMITED BY SIZE
                   WS-C-UPDATE DELIMITED BY ' '
                   INTO HOLDER
                 END-STRING
               
               WHEN WS-C-TITLE NOT = NULL AND 
                    WS-C-DES   NOT = NULL AND
                    WS-C-UPDATE    = NULL
                 STRING WS-C-TITLE DELIMITED BY ' '
                   ' ' DELIMITED BY SIZE
                   WS-C-DES DELIMITED BY ' '
                   INTO HOLDER
                 END-STRING

               WHEN WS-C-TITLE NOT = NULL AND 
                    WS-C-DES       = NULL AND
                    WS-C-UPDATE    = NULL
                 STRING WS-C-TITLE DELIMITED BY ' '
                   INTO HOLDER
                 END-STRING

               WHEN WS-C-TITLE      = NULL AND 
                    WS-C-DES    NOT  = NULL AND
                    WS-C-UPDATE NOT = NULL
                 STRING WS-C-DES DELIMITED BY ' '
                   ' ' DELIMITED BY SIZE
                   WS-C-UPDATE DELIMITED BY ' '
                   INTO HOLDER
                 END-STRING

              WHEN WS-C-TITLE       = NULL AND 
                    WS-C-DES        = NULL AND
                    WS-C-UPDATE NOT = NULL
                 STRING 
                   WS-C-UPDATE DELIMITED BY ' '
                   INTO HOLDER
                 END-STRING

              WHEN WS-C-TITLE       = NULL AND 
                    WS-C-DES   NOT  = NULL AND
                    WS-C-UPDATE     = NULL
                STRING 
                   WS-C-DES DELIMITED BY ' '
                   INTO HOLDER
                 END-STRING
               
               WHEN WS-C-TITLE NOT  = NULL AND 
                    WS-C-DES        = NULL AND
                    WS-C-UPDATE NOT = NULL
                 STRING WS-C-TITLE DELIMITED BY ' '
                   ' ' DELIMITED BY SIZE
                   WS-C-UPDATE DELIMITED BY ' '
                   INTO HOLDER
                 END-STRING

               END-EVALUATE.

       3600-EXIT. EXIT.

       4000-RMAP.
           EXEC CICS 
                RECEIVE MAP('SM003M')
                MAPSET('SM03S')
                INTO (SM003MI)
           END-EXEC.
       4000-EXIT. EXIT.
	
