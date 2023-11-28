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
           COPY SM01S.
      *----------------------------------------------------------------*     
      *                     COPYBOOK REDEFINES                         *
      *----------------------------------------------------------------*
       01  DETL-LINE REDEFINES SM001MI.
           05 FILLER                              PIC X(104).
           05 DETL-SELECT                         OCCURS 11.
              10 DETL-SELECTL                     PIC S9(4) COMP.
              10 DETL-SELECTF                     PIC X.
	           10 FILLER REDEFINES DETL-SELECTF.
                 15 DETL-SELECTA                  PIC X.
	           10 DETL-SELECTI                     PIC X(001). 
           05 DETL-DETAIL                         OCCURS 11.
              10 DETL-DETAILL                     PIC S9(4) COMP.
              10 DETL-DETAILF                     PIC X.
              10 FILLER REDEFINES DETL-DETAILF.
                 15 DETL-DETAILA                  PIC X.
	          10 DETL-DETAILI. 
                 15 DETAILS-TIX-ID                PIC X(07). 
                 15 FILLER1                       PIC X(03).
                 15 DETAILS-TIX-TITLE             PIC X(25). 
                 15 FILLER2                       PIC X(01).
                 15 DETAILS-TIX-STAT              PIC X(10).
                 15 FILLER3                       PIC X(04).   
                 15 DETAILS-LAST-UPD.
                    20  DETAIL-DD                 PIC X(02).
                    20  SLASH1                    PIC X.
                    20  DETAIL-MM                 PIC X(02).
                    20  SLASH2                    PIC X.
                    20  DETAIL-YYYY               PIC X(04). 
                 15 FILLER4                       PIC X(02).
                 15 DETAILS-UPD-BY                PIC X(08).  
           05 FILLER                              PIC X(65).
           COPY DFHBMSCA.
           COPY DFHAID.

       01  WS-CURRENT-MAP                    PIC X(7) VALUE 'SM01S'.
       01  WS-REC-LENGTH                         PIC S9(4) 
           COMP VALUE +228. 
       01  WS-QNAME                              PIC X(08).    
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
       01  WS-STF-REC2.                          
           05 WS-STF-REQ2                        OCCURS 11 PIC X(08).    
      *01  WS-REC.
      *    05 WS-USERID.
      *       10  WS-USERID7                     PIC X(7).
      *       10  FILLER                         PIC X(1).
      *    05 WS-TYPE.
      *       10  WS-REQUESTOR                   PIC X.
      *       10  WS-ADMIN                       PIC X.  
      *       10  WS-APPROVER                    PIC X.
      *       10  WS-SERVICE                     PIC X.
      *    05 WS-UPDATEDBY                       PIC X(8).
       01  WS-TIME                               PIC 9(15) COMP-3.
       01  WS-DATE                               PIC 9(7).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(7).
       01  WS-LENGTH                             PIC S9(4) COMP.
       01  WS-FLAG                               PIC X    VALUE 'N'.
       01  WS-SELECTED-VALUE                     PIC X.
       01  WS-COUNTERS.
           05 WS-INDEX                           PIC 9(02).   
           05 WS-I                               PIC 9(02).                        
           05 WS-SELECT-COUNT                    PIC 9.
           05 WS-PAGE-UPDOWN                     PIC 9(02).

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
           05 WS-INVALID-VALUE                   PIC X(46) VALUE
              'INVALID VALUE. PLEASE CORRECT HIGHLIGHT FIELDS'.   
           05 WS-INVALID-PFKEY                   PIC X(21) VALUE
               'INAVLID PFKEY PRESSED'.    
           05 WS-MULTIPLE-SELECT                 PIC X(31) VALUE 
               'NO MULTIPLE SELECTED IS ALLOWED'.    
           05 WS-SELECT-OPTION                   PIC X(29) VALUE 
              'SELECT TICKET AND PRESS ENTER'.    
                     
       77 WS-RETNCODE                            PIC S9(8) COMP.
       77 WS-RETNCODE2                           PIC S9(8) COMP.
       01  WS-LASTPAGE                           PIC X(1).
       01  WS-FIRSTPAGE                          PIC X(1).
       01  WS-PAGE-CTR                           PIC 9(02).
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
           05 WS-PAGES.        
              10 WS-PREV-PAGE                       PIC X(02).    
              10 PREV-PAGE REDEFINES WS-PREV-PAGE   PIC 9(02).    
              10 WS-CURR-PAGE                       PIC X(02).
              10 CURR-PAGE REDEFINES WS-CURR-PAGE   PIC 9(02).  
              10 WS-TOTAL-QITEM                     PIC 9(02).
              10 WS-MAX-PAGE                        PIC 9(02).  
      *----------------------------------------------------------------*
      *                          LINKAGE SECTION                       *
      *----------------------------------------------------------------*
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
           05 DF-PAGES.        
              10 DF-PREV-PAGE                       PIC X(02).    
              10 PREV REDEFINES DF-PREV-PAGE        PIC 9(02).    
              10 DF-CURR-PAGE                       PIC X(02).
              10 CURR REDEFINES DF-CURR-PAGE        PIC 9(02).  
              10 DF-TOTAL-QITEM                     PIC 9(02).
              10 DF-MAX-PAGE                        PIC 9(02). 

       PROCEDURE DIVISION.
       100-MAIN.
           MOVE DFHCOMMAREA TO WS-COMMAREA
           STRING EIBTRMID DELIMITED BY SIZE
	             'SM01' DELIMITED BY SIZE
                  INTO WS-QNAME 
           EXEC CICS
             IGNORE CONDITION ERROR
           END-EXEC

           IF WS-PGMID = 'SM000' OR WS-PGMID = 'SM001' OR
              WS-PGMID = 'SM002' OR WS-PGMID = 'SM003' OR 
              WS-PGMID = 'SM004' OR WS-PGMID = 'SM005' OR
              WS-PGMID = 'SM006'
               IF EIBCALEN NOT = +26
                  PERFORM 200-REC-MAP
               ELSE
                   MOVE WS-SELECT-OPTION TO ERRMSG1O 
                   MOVE 1 TO WS-STATE
                   MOVE 1 TO WS-PAGE
                   MOVE WS-PAGE TO PAGEO
                   PERFORM 900-MOVE-FILES-TO-Q
                   PERFORM 600-MOVE-Q-TO-SCREEN
                   PERFORM 111-CREATE-MAP
               END-IF
           ELSE 
               MOVE SPACES TO ERRMSG1O
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
           EXEC CICS SEND
                MAP('SM001M')
                MAPSET('SM01S')
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
                MAPSET('SM01S')
                INTO (SM001MI)
                RESP(WS-RETNCODE)
           END-EXEC
           IF EIBRESP = DFHRESP(MAPFAIL)
              MOVE WS-INVALID-PFKEY TO ERRMSG1O
              PERFORM 600-MOVE-Q-TO-SCREEN  
              PERFORM 111-CREATE-MAP
           END-IF
              PERFORM 500-CHECK-EIBAID
              PERFORM 600-MOVE-Q-TO-SCREEN  
              PERFORM 111-CREATE-MAP.
       200-EXIT.
           EXIT.    

       530-PAGE-UP-ENTER.
           IF WS-QITEM-PAGE > WS-QITEM-END 
              SUBTRACT 11 FROM WS-QITEM-PAGE
              MOVE WS-LAST-PAGE TO ERRMSG1O
           ELSE
              MOVE WS-SELECT-OPTION TO 
               ERRMSG1O
              MOVE CURR-PAGE TO WS-PAGE
              MOVE WS-PAGE TO PAGEO
              PERFORM 600-MOVE-Q-TO-SCREEN
           END-IF
           PERFORM 111-CREATE-MAP.
       530-EXIT.
           EXIT.

       540-PAGEDOWN-ENTER.
           IF WS-QITEM-PAGE = WS-QITEM-START OR 
              WS-QITEM-PAGE <  WS-QITEM-START
              MOVE WS-FIRST-PAGE TO ERRMSG1O
              MOVE 1 TO WS-PAGE
              MOVE WS-QITEM-START TO WS-QITEM-PAGE
           ELSE
             SUBTRACT CURR-PAGE FROM WS-PAGE 
             MOVE WS-PAGE TO PAGEO
           END-IF
           MOVE WS-PAGE TO PAGEO
           PERFORM 600-MOVE-Q-TO-SCREEN   
           PERFORM 111-CREATE-MAP.
       540-EXIT.
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
                   MOVE 'SM002' TO ERRMSG1O
                ELSE
                   MOVE WS-INVALID-ACCESS TO ERRMSG1O
                END-IF   
           WHEN DFHPF3
                MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
                MOVE 'SM001' TO WS-PGMID
                MOVE LOW-VALUES TO WS-STATE
                EXEC CICS 
                    SEND CONTROL 
                    ERASE
                END-EXEC
                EXEC CICS DELETEQ TS
                     QUEUE(WS-QNAME)
                END-EXEC 
                EXEC CICS XCTL 
                     PROGRAM('SM000')
                END-EXEC
           WHEN DFHPF5
                MOVE 'PF5 PRESSED' TO ERRMSG1O
           WHEN DFHPF7         
                PERFORM 510-PF7-PAGE-UP

           WHEN DFHPF8         
                PERFORM 520-PF8-PAGE-DOWN

           WHEN DFHPF12         
                PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 11
                   MOVE LOW-VALUES TO DETL-SELECTI(WS-INDEX)
                END-PERFORM
                PERFORM 600-MOVE-Q-TO-SCREEN
                PERFORM 111-CREATE-MAP
           WHEN DFHENTER 
                MOVE PAGEI TO WS-CURR-PAGE
                IF CURR-PAGE IS NUMERIC
                   PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL 
                           WS-INDEX > WS-MAX-PAGE
                     IF CURR-PAGE = WS-INDEX
                        SUBTRACT PREV-PAGE FROM CURR-PAGE GIVING 
                                 WS-PAGE-UPDOWN
      *                 IF CURR-PAGE < 1 OR CURR-PAGE > 4 
      *                    MOVE 
      *                 END-IF         
                        IF CURR-PAGE > PREV-PAGE
                           IF WS-PAGE-UPDOWN = 1       
                              ADD 11 TO WS-QITEM-PAGE
                              PERFORM 530-PAGE-UP-ENTER
                           ELSE    
                              MOVE 1 TO WS-I
                              PERFORM UNTIL WS-I = CURR-PAGE
                                      ADD 11 TO WS-QITEM-PAGE
                                      ADD 1 TO WS-I
                              END-PERFORM
                              PERFORM 530-PAGE-UP-ENTER
                           END-IF             
                        ELSE 
                           IF WS-PAGE-UPDOWN = 1
                              SUBTRACT 11 FROM WS-QITEM-PAGE
                             PERFORM 540-PAGEDOWN-ENTER
                           ELSE 
                              MOVE PREV-PAGE TO WS-I
                              PERFORM UNTIL WS-I = CURR-PAGE
                                      SUBTRACT 11 FROM WS-QITEM-PAGE
                                      SUBTRACT 1 FROM WS-I
                              END-PERFORM   
                              PERFORM 540-PAGEDOWN-ENTER
                           END-IF   
                        END-IF
                     END-IF
                   END-PERFORM
                ELSE 
                    PERFORM 600-MOVE-Q-TO-SCREEN
                    PERFORM 111-CREATE-MAP  
                END-IF
                   
                PERFORM 600-MOVE-Q-TO-SCREEN

                MOVE 1 TO WS-INDEX
                MOVE 0 TO WS-SELECT-COUNT
                PERFORM UNTIL WS-INDEX > 11  
                   IF DETL-SELECTI(WS-INDEX) NOT = '-' AND 
                      DETL-SELECTI(WS-INDEX) NOT = SPACES AND
                      DETL-SELECTI(WS-INDEX) NOT = LOW-VALUES
                      IF DETL-SELECTI(WS-INDEX) = 'U' OR 
                         DETL-SELECTI(WS-INDEX) = 'C' OR 
                         DETL-SELECTI(WS-INDEX) = 'A' OR 
                         DETL-SELECTI(WS-INDEX) = 'X'
                         ADD 1 TO WS-SELECT-COUNT
                         MOVE DETL-SELECTI(WS-INDEX) TO 
                              WS-SELECTED-VALUE
                         MOVE DETL-DETAILI(WS-INDEX) TO WS-STF01-REC
                         MOVE WS-STF-REQ2(WS-INDEX) TO 
                              WS-STF01-REQ
                      END-IF
                    IF WS-SELECT-COUNT = 1
                       EVALUATE WS-SELECTED-VALUE
                        WHEN 'U'
                            IF WS-STF01-STATUS = 'APPROVED' OR 
                               'ONGOING'
                               IF USR-REQUESTOR = 'Y'  OR 
                                  USR-SERVICE = 'Y'  
                                  IF WS-STF01-REQ = USERID
                                     MOVE LENGTH OF WS-COMMAREA TO 
                                       WS-LENGTH
                                     EXEC CICS LINK 
                                          PROGRAM ('SM003')
                                          COMMAREA (WS-COMMAREA)
                                          LENGTH (WS-LENGTH)
                                     END-EXEC
                                     MOVE 'SM003' TO ERRMSG1O
                                  ELSE 
                                     MOVE WS-INVALID-TIX-ACC TO ERRMSG1O   
                                  END-IF
                               ELSE
                                   MOVE WS-INVALID-TIX-ACC TO ERRMSG1O
                               END-IF   
                            ELSE
                                MOVE WS-INVALID-TIX-ACC TO ERRMSG1O
                            END-IF
                        WHEN 'C'
                             IF WS-STF01-STATUS = 'COMPLETED'
                                IF WS-STF01-REQ = USERID
                                   MOVE LENGTH OF WS-COMMAREA 
                                        TO WS-LENGTH
                                   EXEC CICS LINK 
                                        PROGRAM ('SM004')
                                        COMMAREA (WS-COMMAREA)
                                        LENGTH (WS-LENGTH)
                                   END-EXEC
                                   MOVE 'SM004' TO ERRMSG1O  
                                ELSE 
                                   MOVE WS-INVALID-TIX-ACC TO ERRMSG1O
                                   MOVE DFHUNIMD TO 
                                        DETL-SELECTA(WS-INDEX)
                                   MOVE -1 TO DETL-SELECTL(WS-INDEX)
                                END-IF   
                             ELSE
                                MOVE WS-INVALID-TIX-ACC TO ERRMSG1O
                                MOVE DFHUNIMD TO DETL-SELECTA(WS-INDEX)
                                MOVE -1 TO DETL-SELECTL(WS-INDEX)
                             END-IF   
                        WHEN 'A'
                             IF WS-STF01-STATUS = 'CREATED' 
                                IF WS-STF01-REQ = USERID
                                   MOVE LENGTH OF WS-COMMAREA 
                                        TO WS-LENGTH
                                   EXEC CICS LINK 
                                        PROGRAM ('SM005')
                                        COMMAREA (WS-COMMAREA)
                                        LENGTH (WS-LENGTH)
                                   END-EXEC
                                   MOVE 'SM005' TO ERRMSG1O
                                ELSE
                                   MOVE WS-INVALID-TIX-ACC TO ERRMSG1O
                                   MOVE DFHUNIMD TO 
                                        DETL-SELECTA(WS-INDEX)
                                   MOVE -1 TO DETL-SELECTL(WS-INDEX)
                                END-IF   
                              ELSE
                                MOVE WS-INVALID-TIX-ACC TO ERRMSG1O
                                MOVE DFHUNIMD TO DETL-SELECTA(WS-INDEX)
                                MOVE -1 TO DETL-SELECTL(WS-INDEX)
                              END-IF    
                        WHEN 'X'
                             IF WS-STF01-REQ = USERID
                                MOVE LENGTH OF WS-COMMAREA 
                                     TO WS-LENGTH
                                EXEC CICS LINK 
                                     PROGRAM ('SM006')
                                     COMMAREA (WS-COMMAREA)
                                     LENGTH (WS-LENGTH)
                                END-EXEC
                                MOVE 'SM006' TO ERRMSG1O   
                             ELSE
                                MOVE WS-INVALID-TIX-ACC TO ERRMSG1O
                                MOVE DFHUNIMD TO DETL-SELECTA(WS-INDEX)
                                   MOVE -1 TO DETL-SELECTL(WS-INDEX)
                             END-IF   
                        WHEN OTHER
                             MOVE DFHUNIMD TO DETL-SELECTA(WS-INDEX)
                             MOVE -1 TO DETL-SELECTL(WS-INDEX)
                             MOVE WS-INVALID-VALUE TO ERRMSG1O 
                             PERFORM 600-MOVE-Q-TO-SCREEN  
                             PERFORM 111-CREATE-MAP            
                       END-EVALUATE  
                    ELSE 
                      MOVE DFHUNIMD TO DETL-SELECTA(WS-INDEX)
                      MOVE -1 TO DETL-SELECTL(WS-INDEX)
                      MOVE WS-MULTIPLE-SELECT TO ERRMSG1O
                      PERFORM 600-MOVE-Q-TO-SCREEN
                      PERFORM 111-CREATE-MAP    
                      IF WS-SELECT-COUNT < 1
                         MOVE DFHUNIMD TO DETL-SELECTA(WS-INDEX)
                         MOVE -1 TO DETL-SELECTL(WS-INDEX)
                         MOVE WS-FIELD-REQ TO ERRMSG1O
                         PERFORM 600-MOVE-Q-TO-SCREEN
                         PERFORM 111-CREATE-MAP    
                      END-IF   
                    END-IF
                 END-IF  
                   ADD 1 TO WS-INDEX  
                   
                END-PERFORM
           WHEN OTHER
                MOVE 'INAVLID PFKEY PRESSED' TO ERRMSG1O  
                PERFORM 600-MOVE-Q-TO-SCREEN  
                PERFORM 111-CREATE-MAP 
           END-EVALUATE.     
       500-EXIT.
           EXIT.    
    
       510-PF7-PAGE-UP.
           SUBTRACT 11 FROM WS-QITEM-PAGE
           IF WS-QITEM-PAGE = WS-QITEM-START OR 
              WS-QITEM-PAGE <  WS-QITEM-START
              MOVE WS-FIRST-PAGE TO ERRMSG1O
              MOVE 1 TO WS-PAGE
              MOVE WS-QITEM-START TO WS-QITEM-PAGE
           ELSE
             MOVE WS-SELECT-OPTION TO ERRMSG1O
             SUBTRACT 1 FROM WS-PAGE 
             MOVE WS-PAGE TO PAGEO
           END-IF
           MOVE WS-PAGE TO PAGEO
           PERFORM 600-MOVE-Q-TO-SCREEN   
           PERFORM 111-CREATE-MAP.
       510-EXIT.
           EXIT.    
       

       520-PF8-PAGE-DOWN.
           ADD 11 TO WS-QITEM-PAGE
           IF WS-QITEM-PAGE > WS-QITEM-END 
              SUBTRACT 11 FROM WS-QITEM-PAGE
              MOVE WS-LAST-PAGE TO ERRMSG1O
           ELSE
              MOVE WS-SELECT-OPTION TO ERRMSG1O
              ADD 1 TO WS-PAGE
              MOVE WS-PAGE TO PAGEO
              PERFORM 600-MOVE-Q-TO-SCREEN
           END-IF.   
           PERFORM 111-CREATE-MAP.
       520-EXIT.
           EXIT.

       600-MOVE-Q-TO-SCREEN.
            MOVE PAGEO TO WS-PREV-PAGE
            MOVE WS-QITEM-PAGE TO WS-QITEM
            EXEC CICS READQ TS
                      QUEUE(WS-QNAME)
                      INTO (WS-STF-REC)
                      LENGTH(WS-REC-LENGTH)
                      ITEM (WS-QITEM)
             END-EXEC  
          
            MOVE 1 TO WS-INDEX

            PERFORM UNTIL WS-INDEX > 11
                IF EIBRESP = DFHRESP(NORMAL) AND 
                   WS-QITEM <= WS-QITEM-END  
                   MOVE WS-TICKET-ID TO DETAILS-TIX-ID(WS-INDEX)
                   MOVE WS-TICKET-REQ TO WS-STF-REQ2(WS-INDEX)
                   MOVE WS-TICKET-STAT TO DETAILS-TIX-STAT(WS-INDEX)
                   MOVE WS-TICKET-TITLE TO DETAILS-TIX-TITLE(WS-INDEX)
                   MOVE WS-UPD-BY TO DETAILS-UPD-BY(WS-INDEX)
                   MOVE WS-LAST-UPD TO DETAILS-LAST-UPD(WS-INDEX)
                   ADD 1 TO WS-INDEX
                   ADD 1 TO WS-QITEM
                   EXEC CICS READQ TS
                        QUEUE(WS-QNAME)
                        INTO (WS-STF-REC)
                        ITEM (WS-QITEM)
                  END-EXEC  
               ELSE
                  MOVE SPACES TO DETL-DETAILI(WS-INDEX)
                  MOVE WS-LAST-PAGE  TO ERRMSG1O
                  MOVE '1' TO WS-LASTPAGE 
                  ADD 1 TO WS-INDEX
               END-IF
            END-PERFORM
            MOVE DETAILS-TIX-ID(1) TO WS-FUSER.

       600-EXIT.
           EXIT.
   
       900-MOVE-FILES-TO-Q.
           MOVE LOW-VALUES TO WS-KEYB. 
           MOVE LENGTH OF WS-STF-REC TO WS-REC-LENGTH
            EXEC CICS 
               STARTBR FILE('STF001C')
               RIDFLD (WS-KEYB)
               GTEQ
            END-EXEC
                 
            EXEC CICS 
                READNEXT FILE('STF001C')
                INTO (WS-STF-REC)
                RIDFLD (WS-KEYB)
            END-EXEC

           EXEC CICS WRITEQ TS
                     QUEUE(WS-QNAME)
                     FROM (WS-STF-REC)
                     LENGTH (WS-REC-LENGTH)
                     ITEM (WS-QITEM)
           END-EXEC
           MOVE WS-QITEM TO WS-QITEM-START
           MOVE 1 TO WS-TOTAL-QITEM
           PERFORM UNTIL EIBRESP NOT = DFHRESP(NORMAL)
               MOVE WS-QITEM TO WS-QITEM-END
               EXEC CICS 
                    READNEXT FILE('STF001C')
                    INTO (WS-STF-REC)
                    RIDFLD (WS-KEYB)
               END-EXEC
               IF EIBRESP = DFHRESP(NORMAL)
                  EXEC CICS WRITEQ TS
                       QUEUE(WS-QNAME)
                       FROM (WS-STF-REC)
                       LENGTH (WS-REC-LENGTH)
                       ITEM (WS-QITEM)
                  END-EXEC
                 MOVE WS-T-ID TO WS-KEYB6
                  MOVE HIGH-VALUES TO WS-KEYB1
                  ADD 1 TO WS-TOTAL-QITEM
               END-IF
           END-PERFORM
               DIVIDE WS-TOTAL-QITEM BY 11 GIVING WS-MAX-PAGE
               ADD 1 TO WS-MAX-PAGE
           EXEC CICS
                ENDBR FILE('STF001C')
           END-EXEC
           MOVE WS-QITEM-START TO WS-QITEM-PAGE.

       900-EXIT.
           EXIT.    
