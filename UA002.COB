CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UA002.
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
           COPY UA002S.
           COPY DFHBMSCA.
           COPY DFHAID.

       01  WS-CURRENT-MAP  VALUE 'UA002S'        PIC X(7).
       01  WS-REC-LENGTH                         PIC S9(4) 
           COMP VALUE +21. 
       01  WS-KEYB.                       
           05 WS-KEYB6                   PIC X(06) VALUE LOW-VALUES.
           05 FILLER REDEFINES WS-KEYB6.
              10 FILLER                  PIC X(03).
              10 WS-KEYB3                PIC 9(03). 
           05 WS-KEYB1                   PIC X(01) VALUE HIGH-VALUES.    
       01  WS-STF-REC.
           05 WS-TICKET-ID                       PIC 9(07).
           05 WS-TIX-ID                          REDEFINES WS-TICKET-ID.
              10 WS-T-ID                         PIC X(06).
              10 FILLER                          PIC X.
           05 WS-TICKET-REQ                      PIC X(08).
           05 WS-TICKET-STAT                     PIC X(10).
           05 WS-TICKET-TITLE                    PIC X(25).
           05 WS-TICKET-DESC                     PIC X(100).
           05 WS-LAST-UPD                        PIC X(20).
           05 WS-UPD-BY                          PIC X(08).
           05 WS-UPD-REMARKS                     PIC X(50).
       01  WS-REC.
           05 WS-USERID.
              10  WS-USERID7                     PIC X(7).
              10  FILLER                         PIC X(1).
           05 WS-TYPE.
              10  WS-REQUESTOR                   PIC X.
              10  WS-ADMIN                       PIC X.  
              10  WS-APPROVER                    PIC X.
              10  WS-SERVICE                     PIC X.
           05 WS-UPDATEDBY                       PIC X(8).
       01  WS-TIME                               PIC 9(15) COMP-3.
       01  WS-DATE                               PIC 9(7).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(7).
       01  WS-LENGTH                             PIC S9(4) COMP.
      *--------------------------------------------------------------*
      *               COMMAREA WORKING STORAGE                       *
      *--------------------------------------------------------------*
       01  WS-COMMAREA.
           05 WS-PROG-STATE                      PIC X(15).
           05 WS-PGMID                           PIC X(06).
           05 USERID.
              10  USERID7                        PIC X(7).
              10  FILLER                         PIC X(1).
           05 USR-TYPE.
             15 USR-REQUESTOR                    PIC X.
             15 USR-ADMIN                        PIC X.  
             15 USR-APPROVER                     PIC X.
             15 USR-SERVICE                      PIC X.
           05 WS-STATE                           PIC X.

       01  WS-COUNTERS.
           05 WS-INDEX                           PIC 9(02).   
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
           05 WS-FIRST-PAGE                      PIC X(23) VALUE
              'THIS IS THE FIRST PAGE'.
           05 WS-LAST-PAGE                       PIC X(21) VALUE
              'THIS IS THE LAST PAGE'.
                    
       01  WS-PAGE-NO                            PIC 9(03) VALUE 1.
       77 WS-RETNCODE        PIC S9(8) COMP.
       77 WS-RETNCODE2        PIC S9(8) COMP.
      *----------------------------------------------------------------*
      *                         LINKAGE SECTION                        *
      *----------------------------------------------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA                           PIC X(50).
      *----------------------------------------------------------------*
      *                         PROGRAM DIVISION                       *
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       100-MAIN.
           EXEC CICS IGNORE CONDITION
                     ERROR
           END-EXEC
           
           MOVE DFHCOMMAREA TO WS-COMMAREA
           IF WS-PGMID = 'SM000' OR 'UA001' OR 'UA002' OR LOW-VALUES
               IF WS-STATE NOT = LOW-VALUES
                  PERFORM 200-REC-MAP
               ELSE
                   MOVE 'ENTER USER DETAILS AND PRESS PF2' TO ERRMSG8O 
                   MOVE 1 TO WS-STATE
                   PERFORM 111-CREATE-MAP
               END-IF
           ELSE 
               MOVE SPACES TO ERRMSG8O
               MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
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

       110-DATE-TIME.
           MOVE EIBDATE TO WS-DATE.
           MOVE WS-DATE-X TO DATEO.
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
           MOVE DFHBMASB TO DATEA.
       110-EXIT.
           EXIT.

       111-CREATE-MAP.
           PERFORM 110-DATE-TIME
           MOVE LENGTH OF UA002MO TO WS-LENGTH
           MOVE -1 TO USRIDL
           EXEC CICS SEND
                MAP('UA002M')
                MAPSET(WS-CURRENT-MAP)
                FROM(UA002MO)
                LENGTH(WS-LENGTH)
                CURSOR
                ERASE
           END-EXEC
           EXEC CICS RETURN
                TRANSID('UA02')
                COMMAREA(WS-COMMAREA)
           END-EXEC.
       110-EXIT.
           EXIT.    
       
       200-REC-MAP.
           EXEC CICS RECEIVE
                MAP('UA002M')
                MAPSET(WS-CURRENT-MAP)
                INTO (UA002MI)
                RESP(WS-RETNCODE)
           END-EXEC
           IF EIBRESP = DFHRESP(MAPFAIL)
              MOVE WS-MAPFAIL TO ERRMSG8O
              PERFORM 111-CREATE-MAP
           END-IF.
       200-EXIT.
           EXIT.    