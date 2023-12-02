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
           COPY SM06S.
           COPY DFHAID.
           COPY DFHBMSCA.
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
           05 COMM-TICKET-REC.
              10 COMM-TICKET-ID                       PIC 9(07).
              10 COMM-TICKET-KEY-NUM REDEFINES COMM-TICKET-ID PIC X(07).
              10 COMM-TICKET-REQUESTOR                   PIC X(08).
              10 COMM-TICKET-STATUS                      PIC X(10).
              10 COMM-TICKET-TITLE                       PIC X(25).
              10 COMM-TICKET-DESC                        PIC X(100).
              10 COMM-TICKET-LAST-UPD                    PIC X(20). 
              10 COMM-TICKET-LAST-UPD-BY                 PIC X(08). 
              10 COMM-TICKET-UPD-REMARKS                 PIC X(50).  

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
           05 DF-TICKET-REC.
              10 DF-TICKET-ID                       PIC 9(07).
              10 DF-TICKET-KEY-NUM REDEFINES DF-TICKET-ID PIC 9(06).
              10 DF-TICKET-REQUESTOR                   PIC X(08).
              10 DF-TICKET-STATUS                      PIC X(10).
              10 DF-TICKET-TITLE                       PIC X(25).
              10 DF-TICKET-DESC                        PIC X(100).
              10 DF-TICKET-LAST-UPD                    PIC X(20). 
              10 DF-TICKET-LAST-UPD-BY                 PIC X(08). 
              10 DF-TICKET-UPD-REMARKS                 PIC X(50).   

       PROCEDURE DIVISION.
       100-PROCESS.
           EXEC CICS IGNORE CONDITION
                     ERROR 
           END-EXEC
           MOVE DFHCOMMAREA TO WS-COMMAREA
           IF WS-PGMID = 'SM000' OR WS-PGMID = 'SM001' OR
              WS-PGMID = 'SM012'
              IF WS-STATE NOT = LOW-VALUES
                 PERFORM 200-REC-MAP
              ELSE 
                 MOVE 1 TO WS-STATE
                 PERFORM 110-NEW-MAP
              END-IF   
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

       110-NEW-MAP.
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
           END-EXEC
           MOVE DFHBMASB TO TIMEA
           MOVE DFHBMASB TO DATEA
           MOVE -1       TO TKTNUML
           MOVE LENGTH OF SM006MO TO WS-LENGTH 
           EXEC CICS SEND 
                 MAP('SM006M')
                 MAPSET('SM06S')
                 FROM(SM006MO)
                 LENGTH(WS-LENGTH)
                 ERASE
           END-EXEC
           EXEC CICS RETURN
	            TRANSID('SM06')
		        COMMAREA(WS-COMMAREA)
	       END-EXEC.
       110-EXIT.
           EXIT.

       200-REC-MAP.
           EXEC CICS RECEIVE 
                MAP('SM006M')
                MAPSET('SM06S')
                INTO (SM006MI)
           END-EXEC
           IF EIBRESP = DFHRESP(MAPFAIL)
              MOVE WS-MAPFAIL TO ERRMSGO
           END-IF.
       200-EXIT.
           EXIT.
