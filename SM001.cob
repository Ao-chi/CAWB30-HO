       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM001.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       FILE SECTION.
      *------------------------*
      *    WORKING STORAGE     *
      *------------------------*
       WORKING-STORAGE SECTION.
           COPY SM0001.
           COPY DFHBMSCA.
           COPY DFHAID.
       
       01  WS-DETL-LINE        REDEFINES SM001MI.
           05 FILLER                             PIC X(162).
           05 DETL-SELECT                        OCCURS 11 TIMES.
              10 DETL-SELECTL				     PIC S9(4)
                 COMP.
              10 DETL-SELECTF				     PIC X.
	          10 FILLER REDEFINES DETL-SELECTF.
                 15 DETL-SELECTA			     
                 PIC X.
	          10 DETL-SELECTI				     
                 PIC X(01). 
           05 DETL-TIXNUM                        OCCURS 11 TIMES.
              10 DETL-TIXNUML				     PIC S9(4)
                 COMP.
              10 DETL-TIXNUMF				     PIC X.
	          10 FILLER REDEFINES DETL-TIXNUMF.
                 15 DETL-TIXNUMA			     
                 PIC X.
	          10 DETL-TIXNUMI				     
                 PIC X(06).
           05 DETL-TITLE1                        OCCURS 11 TIMES.
              10 DETL-TITLE1L				     PIC S9(4)
                 COMP.
              10 DETL-TITLE1F				     PIC X.
	          10 FILLER REDEFINES DETL-TITLE1F.
                 15 DETL-TITLE1A			     
                 PIC X.
	          10 DETL-TITLE1I				     
                 PIC X(25).      
           05 DETL-STATS                        OCCURS 11 TIMES.
              10 DETL-STATSL				     PIC S9(4)
                 COMP.
              10 DETL-STATSF				     PIC X.
	          10 FILLER REDEFINES DETL-STATSF.
                 15 DETL-STATSA			     
                 PIC X.
	          10 DETL-STATSI				     
                 PIC X(10).
           05 DETL-LSTUPD                        OCCURS 11 TIMES.
              10 DETL-LSTUPDL				     PIC S9(4)
                 COMP.
              10 DETL-LSTUPDF				     PIC X.
	          10 FILLER REDEFINES DETL-LSTUPDF.
                 15 DETL-LSTUPDA			     
                 PIC X.
	          10 DETL-LSTUPDI				     
                 PIC X(08).
           05 DETL-UPDBY                        OCCURS 11 TIMES.
              10 DETL-UPDBYL				     PIC S9(4)
                 COMP.
              10 DETL-UPDBYF				     PIC X.
	          10 FILLER REDEFINES DETL-UPDBYF.
                 15 DETL-UPDBYA			     
                 PIC X.
	          10 DETL-UPDBYI				     
                 PIC X(06).                   
       01  WS-CURRENT-MAP  VALUE 'SM01S'         PIC X(7).
       01  WS-REC-LENGTH                         PIC S9(4) 
           COMP VALUE +227. 
       01  WS-STF-REC.
           05 WS-TICKET-ID                       PIC 9(06).
           05 WS-TICKET-TITLE                    PIC X(25).
           05 WS-TICKET-DESC                     PIC X(100).
           05 WS-TICKET-REQ                      PIC X(08).
           05 WS-TICKET-STAT                     PIC X(10).
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
      *------------------------*
      *    LINKAGE SECTION     *
      *------------------------*
       LINKAGE SECTION.
       01  DFHCOMMAREA                           PIC X(50).

       PROCEDURE DIVISION.
       100-MAIN.
           EXEC CICS IGNORE CONDITION
                     ERROR
           END-EXEC
           
           MOVE DFHCOMMAREA TO WS-COMMAREA
           PERFORM 300-BROWSE-TICKET
           IF WS-PGMID = 'SM0000'
               IF WS-STATE NOT = LOW-VALUES
                  PERFORM 200-REC-MAP
               ELSE
                   MOVE 'SELECT TICKET AND PRESS ENTER' TO ERRMSG1O 
                   MOVE 1 TO WS-STATE
                   PERFORM 111-CREATE-MAP

               END-IF
           ELSE 
               MOVE SPACES TO ERRMSG1O
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
           MOVE WS-DATE-X TO DATE1O.
           EXEC CICS ASKTIME
                ABSTIME    (WS-TIME)
           END-EXEC
           EXEC CICS FORMATTIME
                ABSTIME    (WS-TIME)
                DATESEP    ('/')
                MMDDYYYY   (DATE1O)
                TIME       (TIME1O)
                TIMESEP    (':')
           END-EXEC
           MOVE DFHBMASB TO TIME1A
           MOVE DFHBMASB TO DATE1A.
       110-EXIT.
           EXIT.

       111-CREATE-MAP.
           PERFORM 110-DATE-TIME
           MOVE DFHUNIMD TO TITLEA
           MOVE DFHUNIMD TO STATUSA
           MOVE LENGTH OF SM001MO TO WS-LENGTH
           MOVE WS-PAGE-NO TO PAGEI
           MOVE -1 TO TITLEL
           EXEC CICS SEND
                MAP('SM001M')
                MAPSET('SM0001')
                FROM(SM001MO)
                LENGTH(WS-LENGTH)
                CURSOR
                ERASE
           END-EXEC
           EXEC CICS RETURN
                TRANSID('SM01')
                COMMAREA(WS-COMMAREA)
           END-EXEC.
       110-EXIT.
           EXIT.

       200-REC-MAP.
           EXEC CICS RECEIVE
                MAP('SM001M')
                MAPSET('SM0001')
                INTO (SM001MI)
           END-EXEC
           IF EIBRESP = DFHRESP(MAPFAIL)
              MOVE WS-MAPFAIL TO ERRMSG1O
              PERFORM 111-CREATE-MAP
           END-IF
              PERFORM 500-CHECK-EIBAID.
       200-EXIT.
           EXIT.    

       500-CHECK-EIBAID.
           EVALUATE EIBAID
           WHEN DFHPF2
                IF USR-REQUESTOR  = 'Y'
                   MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
                   EXEC CICS LINK 
                        PROGRAM ('SM002')
                        COMMAREA (WS-COMMAREA)
                        LENGTH (WS-LENGTH)
                   END-EXEC
                   MOVE WS-PROG-STATE TO ERRMSG1O
                ELSE
                   MOVE WS-INVALID-ACCESS TO ERRMSG1O
                END-IF   
           WHEN DFHPF3
                MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
                EXEC CICS XCTL 
                     PROGRAM('SM000')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(WS-LENGTH)
                END-EXEC
           WHEN DFHENTER 
                EVALUATE SELECTI
                WHEN 'U'
                     MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
                     EXEC CICS LINK 
                          PROGRAM ('SM003')
                          COMMAREA (WS-COMMAREA)
                          LENGTH (WS-LENGTH)
                     END-EXEC
                     MOVE WS-PROG-STATE TO ERRMSG1O
                WHEN 'C'
                     MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
                     EXEC CICS LINK 
                          PROGRAM ('SM004')
                          COMMAREA (WS-COMMAREA)
                          LENGTH (WS-LENGTH)
                     END-EXEC
                     MOVE WS-PROG-STATE TO ERRMSG1O  
                WHEN 'A'
                     MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
                     EXEC CICS LINK 
                          PROGRAM ('SM005')
                          COMMAREA (WS-COMMAREA)
                          LENGTH (WS-LENGTH)
                     END-EXEC
                     MOVE WS-PROG-STATE TO ERRMSG1O  
                WHEN 'X'
                     MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
                     EXEC CICS LINK 
                          PROGRAM ('SM006')
                          COMMAREA (WS-COMMAREA)
                          LENGTH (WS-LENGTH)
                     END-EXEC
                     MOVE WS-PROG-STATE TO ERRMSG1O   
                WHEN OTHER
                  MOVE 'INVALID VALUE. PLEASE CORRECT HIGHLIGHT FIELDS'
                     TO ERRMSG1O                   
                END-EVALUATE    
           WHEN OTHER
                MOVE 'INAVLID PFKEY PRESSED' TO ERRMSG1O     
           END-EVALUATE.     
       500-EXIT.
           EXIT.    

       300-BROWSE-TICKET.
           EXEC CICS STARTBR 
                FILE('STF001')
                RIDFLD(WS-TICKET-ID)
                KEYLENGTH(06)
                RESP2(WS-RETNCODE2)
                RESP(WS-RETNCODE)
                GTEQ
           END-EXEC
           
           PERFORM 10 TIMES
           EXEC CICS READNEXT 
                     FILE('STF001')
                     RIDFLD(WS-TICKET-ID)
                     INTO(WS-STF-REC)
                     LENGTH(WS-REC-LENGTH)
                     KEYLENGTH(06)
                      RESP(WS-RETNCODE)

           END-EXEC
           MOVE WS-TICKET-ID TO TIXNUMI

           
           END-PERFORM

           EXEC CICS ENDBR  
                FILE('STF001')
           END-EXEC.
       300-EXIT.
           EXIT.    