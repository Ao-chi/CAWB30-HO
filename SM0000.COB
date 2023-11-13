       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM0000.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
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
       01  WS-CURRENT-MAP  VALUE 'SM000'         PIC X(7).
       01  WS-TIME                               PIC 9(15) COMP-3.
       01  WS-DATE                               PIC 9(7).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(7).
       01  WS-LENGTH                             PIC S9(4) COMP.
       01  WS-END                                PIC X(15) VALUE
           'END PROCESSING'.
       01  WS-ERRMSG.
           05 WS-FIELD-REQ                       PIC X(24) VALUE
              'OPTION FIELD IS REQUIRED'.
       01  WS-MAPFAIL                            PIC X(20) VALUE
           'MAPFAIL ERROR'.
       01  WS-INVALID-TRNID                      PIC X(15) VALUE
           'INVALID ACCESS'.
       01  WS-INVALID-USER                       PIC X(20) VALUE 
           'INVALID USER ACCESS'.    
       01  WS-COMMAREA.
           05 WS-PROG-STATE                      PIC X(15).
           05 WS-PGMID                           PIC X(06).
       77 WS-RETNCODE                            PIC S9(8) COMP.
           COPY SM000.
           COPY DFHAID.
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                           PIC X(21).
      *
       PROCEDURE DIVISION.
       100-PROCESS.
           EXEC CICS IGNORE CONDITION
                     ERROR
           END-EXEC
           PERFORM 300-VERIFY-USER
           
           IF EIBTRNID = 'SM00'
              MOVE DFHCOMMAREA TO WS-COMMAREA
              MOVE 'SM0000' TO WS-PGMID
              IF EIBCALEN NOT = +0
                 PERFORM 200-REC-MAP
              ELSE
                 MOVE 'ENTER OPTION AND PRESS ENTER' TO ERRMSGO 
                 PERFORM 110-VERIFY-ROLE
           ELSE
              MOVE SPACES TO ERRMSGO
              EXEC CICS SEND TEXT
                      FROM (WS-INVALID-TRNID)
                      LENGTH (+15)
                      ERASE
                 END-EXEC
              EXEC CICS RETURN
              END-EXEC
           END-IF.
       100-EXIT.
           EXIT.
      
       110-VERIFY-ROLE.
           EVALUATE WS-TYPE 
           WHEN 'YYYY'
           WHEN 'YYYN'
                PERFORM 111-NEW-MAP
           WHEN 'YNNN' 
           WHEN 'YNNY'
                MOVE '5 - CANCEL TICKET' TO OPTN05I
                MOVE SPACE TO OPTN06I
                MOVE SPACES TO OPTN07I
                PERFORM 111-NEW-MAP
           WHEN 'YYNN'
           WHEN 'YYNY'
                MOVE '5 - CANCEL TICKET' TO OPTN05I
                MOVE '6 - USER MAINTENANCE' TO OPTN06I
                MOVE SPACES TO OPTN07I
                PERFORM 111-NEW-MAP
           WHEN 'YNYN'
           WHEN 'YNYY'
                MOVE SPACES TO OPTN07I
                PERFORM 111-NEW-MAP            
           WHEN 'NYYY'
                MOVE '2 - UPDATE TICKET' TO OPTN02I
                MOVE '3 - APPROVE TICKET' TO OPTN03I
                MOVE '4 - USER MAINTENANCE' TO OPTN04I
                MOVE SPACES TO OPTN05I
                MOVE SPACE TO OPTN06I
                MOVE SPACES TO OPTN07I 
                PERFORM 111-NEW-MAP
           WHEN 'NNYY'
                MOVE '2 - UPDATE TICKET' TO OPTN02I
                MOVE '3 - APPROVE TICKET' TO OPTN03I
                MOVE SPACES TO OPTN04I
                MOVE SPACES TO OPTN05I
                MOVE SPACE TO OPTN06I
                MOVE SPACES TO OPTN07I 
                PERFORM 111-NEW-MAP    
           WHEN 'NNNY'
                MOVE '2 - UPDATE TICKET' TO OPTN02I
                MOVE SPACES TO OPTN03I
                MOVE SPACES TO OPTN04I
                MOVE SPACES TO OPTN05I
                MOVE SPACE TO OPTN06I
                MOVE SPACES TO OPTN07I 
                PERFORM 111-NEW-MAP 
           WHEN 'NYNY'
                MOVE '2 - UPDATE TICKET' TO OPTN02I
                MOVE '3 - USER MAINTENANCE' TO OPTN03I
                MOVE SPACES TO OPTN04I
                MOVE SPACES TO OPTN05I
                MOVE SPACE TO OPTN06I
                MOVE SPACES TO OPTN07I 
                PERFORM 111-NEW-MAP 
           WHEN 'NYYN'
                MOVE '2 - APPROVE TICKET' TO OPTN02I
                MOVE '3 - USER MAINTENANCE' TO OPTN03I
                MOVE SPACES TO OPTN04I
                MOVE SPACES TO OPTN05I
                MOVE SPACE TO OPTN06I
                MOVE SPACES TO OPTN07I 
                PERFORM 111-NEW-MAP 
           WHEN 'NYNN'
                MOVE '1 - USER MAINTENANCE' TO OPTN01I
                MOVE SPACES TO OPTN02I
                MOVE SPACES TO OPTN03I
                MOVE SPACES TO OPTN04I
                MOVE SPACES TO OPTN05I
                MOVE SPACE TO OPTN06I
                MOVE SPACES TO OPTN07I 
                PERFORM 111-NEW-MAP 
           WHEN 'NNYN'
                MOVE '2 - APPROVE TICKET' TO OPTN02I
                MOVE SPACES TO OPTN03I
                MOVE SPACES TO OPTN04I
                MOVE SPACES TO OPTN05I
                MOVE SPACE TO OPTN06I
                MOVE SPACES TO OPTN07I 
                PERFORM 111-NEW-MAP 
           WHEN 'NNNN'
                MOVE SPACES TO OPTN01I
                MOVE SPACES TO OPTN02I
                MOVE SPACES TO OPTN03I
                MOVE SPACES TO OPTN04I
                MOVE SPACES TO OPTN05I
                MOVE SPACE TO OPTN06I
                MOVE SPACES TO OPTN07I 
                PERFORM 111-NEW-MAP 
           WHEN OTHER
                MOVE SPACES TO ERRMSGO
                EXEC CICS SEND TEXT
                     FROM (WS-INVALID-TRNID)
                     LENGTH (+15)
                     ERASE
                END-EXEC
                EXEC CICS RETURN
                END-EXEC
           END-EVALUATE.
       110-EXIT.
           EXIT.
       
       112-DATE-TIME.
           MOVE EIBDATE TO WS-DATE.
           MOVE WS-DATE-X TO DATEO.
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
           MOVE DFHBMASB TO TIMEA
           MOVE DFHBMASB TO DATEA.
       112-EXIT.
           EXIT.

       111-NEW-MAP.
           PERFORM 112-DATE-TIME
              
           MOVE DFHUNIMD TO OPTIONA
           MOVE DFHUNIMD TO TICKETA
           MOVE -1       TO OPTIONL
           MOVE LENGTH OF SM00MO TO WS-LENGTH
           EXEC CICS SEND
                MAP('SM00M')
                MAPSET('SM000')
                FROM(SM00MO)
                LENGTH(WS-LENGTH)
                CURSOR
                ERASE
           END-EXEC
           EXEC CICS RETURN
                TRANSID(EIBTRNID)
                COMMAREA(WS-COMMAREA)
           END-EXEC.
       110-EXIT.
           EXIT.

       200-REC-MAP.
           EXEC CICS RECEIVE
                MAP('SM00M')
                MAPSET('SM000')
                INTO (SM00MI)
           END-EXEC
           IF EIBRESP = DFHRESP(MAPFAIL)
              MOVE WS-MAPFAIL TO ERRMSGO
              PERFORM 110-VERIFY-ROLE
           END-IF.
           MOVE WS-PROG-STATE TO ERRMSGO
           PERFORM 400-CHECK-EIBAID.
       200-EXIT.
           EXIT.
       
       21A-SEARCH-SM001.
           MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
           EXEC CICS XCTL
                PROGRAM('SM001')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-LENGTH)
                RESP(WS-RETNCODE)
           END-EXEC
           MOVE WS-PROG-STATE TO ERRMSGO.
       21A-EXIT.
           EXIT.

       21B-SUBMIT-SM002.
           EXEC CICS LINK
                PROGRAM('SM002')
                COMMAREA(WS-COMMAREA)
                LENGTH(+15)
                RESP(WS-RETNCODE)
           END-EXEC
           MOVE WS-PROG-STATE TO ERRMSGO.
       21B-EXIT.
           EXIT.

       21C-UPDATE-SM003.
           EXEC CICS LINK
                PROGRAM('SM003')
                COMMAREA(WS-COMMAREA)
                LENGTH(+15)
                RESP(WS-RETNCODE)
           END-EXEC
           MOVE WS-PROG-STATE TO ERRMSGO.
       21C-EXIT.
           EXIT.

       21D-CLOSE-SM004.
           EXEC CICS LINK
                PROGRAM('SM004')
                COMMAREA(WS-COMMAREA)
                LENGTH(+15)
                RESP(WS-RETNCODE)
           END-EXEC
           MOVE WS-PROG-STATE TO ERRMSGO.
       21D-EXIT.
           EXIT.
       
       21E-APPROVE-SM005.
           EXEC CICS LINK
                PROGRAM('SM005')
                COMMAREA(WS-COMMAREA)
                LENGTH(+15)
                RESP(WS-RETNCODE)
           END-EXEC
           MOVE WS-PROG-STATE TO ERRMSGO.
       21R-EXIT.
           EXIT.

       21F-CANCEL-SM006.
           EXEC CICS LINK
                PROGRAM('SM006')
                COMMAREA(WS-COMMAREA)
                LENGTH(+15)
                RESP(WS-RETNCODE)
           END-EXEC
           MOVE WS-PROG-STATE TO ERRMSGO.
       21F-EXIT.
           EXIT.
           
       21G-ADMIN-UA001.
           EXEC CICS LINK
                PROGRAM('UA001')
                COMMAREA(WS-COMMAREA)
                LENGTH(+15)
                RESP(WS-RETNCODE)
           END-EXEC
           MOVE WS-PROG-STATE TO ERRMSGO.
       21G-EXIT.
           EXIT.

       21H-F3-EXIT.
           MOVE SPACES TO ERRMSGO
           EXEC CICS SEND TEXT
                FROM (WS-END)
                LENGTH (+15)
                ERASE
           END-EXEC
           EXEC CICS RETURN
           END-EXEC.
       21H-EXIT.
           EXIT.
       
       300-VERIFY-USER SECTION.
           EXEC CICS ASSIGN 
               USERID(WS-USERID)
           END-EXEC
           EXEC CICS 
                READ FILE('uaf001')
                INTO (WS-REC)
                RIDFLD (WS-USERID)
                KEYLENGTH (WS-STD-KEY-LEN)
                EQUAL
           END-EXEC
           IF EIBRESP = DFHRESP(NOTFND)
               EXEC CICS SEND TEXT
                    FROM (WS-INVALID-USER)
                    LENGTH (+20)
                    ERASE
               END-EXEC
               EXEC CICS RETURN
               END-EXEC
           END-IF.
       300-EXIT.
           EXIT.
           
       400-CHECK-EIBAID.
           EVALUATE WS-TYPE 
           WHEN 'YYYY'
           WHEN 'YYYN'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21A-SEARCH-SM001
                     WHEN 2
                          PERFORM 21B-SUBMIT-SM002
                     WHEN 3
                          PERFORM 21C-UPDATE-SM003
                     WHEN 4
                          PERFORM 21D-CLOSE-SM004
                     WHEN 5
                          PERFORM 21E-APPROVE-SM005
                     WHEN 6 
                          PERFORM 21F-CANCEL-SM006
                     WHEN 7 
                          PERFORM 21G-ADMIN-UA001
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE 
           WHEN 'YNNN' 
           WHEN 'YNNY'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21A-SEARCH-SM001
                     WHEN 2
                          PERFORM 21B-SUBMIT-SM002
                     WHEN 3
                          PERFORM 21C-UPDATE-SM003
                     WHEN 4
                          PERFORM 21D-CLOSE-SM004
                     WHEN 5
                          PERFORM 21F-CANCEL-SM006
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE 
           WHEN 'YYNN'
           WHEN 'YYNY'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21A-SEARCH-SM001
                     WHEN 2
                          PERFORM 21B-SUBMIT-SM002
                     WHEN 3
                          PERFORM 21C-UPDATE-SM003
                     WHEN 4
                          PERFORM 21D-CLOSE-SM004
                     WHEN 5
                          PERFORM 21F-CANCEL-SM006
                     WHEN 6 
                          PERFORM 21G-ADMIN-UA001
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE 
           WHEN 'YNYN'
           WHEN 'YNYY'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21A-SEARCH-SM001
                     WHEN 2
                          PERFORM 21B-SUBMIT-SM002
                     WHEN 3
                          PERFORM 21C-UPDATE-SM003
                     WHEN 4
                          PERFORM 21D-CLOSE-SM004
                     WHEN 5
                          PERFORM 21E-APPROVE-SM005
                     WHEN 6 
                          PERFORM 21F-CANCEL-SM006
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE       
           WHEN 'NYYY'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21A-SEARCH-SM001
                     WHEN 2
                          PERFORM 21C-UPDATE-SM003
                     WHEN 3
                          PERFORM 21E-APPROVE-SM005
                     WHEN 4
                          PERFORM 21G-ADMIN-UA001     
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE   
           WHEN 'NNYY'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21A-SEARCH-SM001
                     WHEN 2
                          PERFORM 21C-UPDATE-SM003
                     WHEN 3
                          PERFORM 21E-APPROVE-SM005     
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE
           WHEN 'NNNY'
                 EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21A-SEARCH-SM001
                     WHEN 2
                          PERFORM 21C-UPDATE-SM003
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE
           WHEN 'NYNY'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21A-SEARCH-SM001
                     WHEN 2
                          PERFORM 21C-UPDATE-SM003
                     WHEN 3
                          PERFORM 21G-ADMIN-UA001    
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE
           WHEN 'NYYN'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21A-SEARCH-SM001
                     WHEN 2
                          PERFORM 21E-APPROVE-SM005
                     WHEN 3
                          PERFORM 21G-ADMIN-UA001    
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE
           WHEN 'NYNN'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21G-ADMIN-UA001    
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE
           WHEN 'NNYN'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE OPTIONI
                     WHEN 1 
                          PERFORM 21A-SEARCH-SM001
                     WHEN 2
                          PERFORM 21E-APPROVE-SM005  
                     WHEN DFHNULL
                          MOVE WS-FIELD-REQ TO ERRMSGO
                     WHEN OTHER
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE
              
           WHEN 'NNNN'
                EVALUATE EIBAID
                WHEN DFHPF3
                     PERFORM 21H-F3-EXIT
                WHEN DFHENTER
                     EVALUATE TRUE
                     WHEN OPTIONI NOT = SPACES
                     WHEN OPTIONI NOT = LOW-VALUES
                          MOVE 'OPTION INVALID VALUE' TO ERRMSGO
                     END-EVALUATE
                WHEN OTHER
                     MOVE 'INAVLID PFKEY PRESSED' TO ERRMSGO
                END-EVALUATE  
           WHEN OTHER
                MOVE SPACES TO ERRMSGO
                EXEC CICS SEND TEXT
                     FROM (WS-INVALID-TRNID)
                     LENGTH (+15)
                     ERASE
                END-EXEC
                EXEC CICS RETURN
                END-EXEC
           END-EVALUATE.
       400-EXIT.
           EXIT.
       
