       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM000.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
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
       01  WS-CURRENT-MAP  VALUE 'SM00S'         PIC X(7).
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
           05 WS-PGMID                           PIC X(06).
           05 WS-TICKET-PASSED                   PIC X(07).
           05 USERID.
              10  USERID7                        PIC X(7).
              10  FILLER                         PIC X(1).
           05 USR-TYPE.
              15 USR-REQUESTOR                   PIC X.
              15 USR-ADMIN                       PIC X.  
              15 USR-APPROVER                    PIC X.
              15 USR-SERVICE                     PIC X.
           05 WS-STATE                           PIC X.
   
       77 WS-RETNCODE                            PIC S9(8) COMP.
           COPY SM00S.
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
           05 DF-STATE                           PIC X.

       PROCEDURE DIVISION.
       100-PROCESS.
           EXEC CICS IGNORE CONDITION
                     ERROR
           END-EXEC
           PERFORM 300-VERIFY-USER
           
      *    IF EIBTRNID = 'SM00'
              
              MOVE DFHCOMMAREA TO WS-COMMAREA
           IF WS-PGMID = 'SM000' OR WS-PGMID = 'SM001' OR
              WS-PGMID = 'SM002' OR WS-PGMID = 'SM003' OR 
              WS-PGMID = 'SM004' OR WS-PGMID = 'SM005' OR
              WS-PGMID = 'SM006' OR EIBTRNID = 'SM00'
                 MOVE 'SM000' TO WS-PGMID
                 MOVE WS-TYPE TO USR-TYPE
                 MOVE WS-USERID TO USERID
                 IF EIBCALEN NOT = +0 
      *          IF WS-STATE NOT = LOW-VALUES
                    PERFORM 200-REC-MAP
                 ELSE
                    MOVE 'ENTER OPTION AND PRESS ENTER' TO ERRMSGO 
                    MOVE 1 TO WS-STATE
                    PERFORM 110-VERIFY-ROLE
                 END-IF   
              
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
                               OPTN02I
                               OPTN03I
                               OPTN04I
                               OPTN05I
                               OPTN06I
                               OPTN07I 
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
                MAPSET('SM00S')
                FROM(SM00MO)
                LENGTH(WS-LENGTH)
                CURSOR
                ERASE
           END-EXEC
           EXEC CICS RETURN
                TRANSID('SM00')
                COMMAREA(WS-COMMAREA)
           END-EXEC.
       110-EXIT.
           EXIT.

       200-REC-MAP.
           EXEC CICS RECEIVE
                MAP('SM00M')
                MAPSET('SM00S')
                INTO (SM00MI)
           END-EXEC
           IF EIBRESP = DFHRESP(MAPFAIL)
              MOVE WS-MAPFAIL TO ERRMSGO
              PERFORM 110-VERIFY-ROLE
           END-IF.
           PERFORM 400-CHECK-EIBAID.
       200-EXIT.
           EXIT.
       
       21A-SEARCH-SM001.
           MOVE LOW-VALUES TO WS-STATE
           MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
           EXEC CICS XCTL
                PROGRAM('SM001')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-LENGTH)
           END-EXEC.
       21A-EXIT.
           EXIT.

       21B-SUBMIT-SM002.
           MOVE LOW-VALUES TO WS-STATE
           MOVE TICKETI TO WS-TICKET-PASSED
           MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
           EXEC CICS XCTL
                PROGRAM('SM002')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-LENGTH)
           END-EXEC.
       21B-EXIT.
           EXIT.

       21C-UPDATE-SM003.
           MOVE TICKETI TO WS-TICKET-PASSED
           MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
           EXEC CICS LINK
                PROGRAM('SM003')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-LENGTH)
                RESP(WS-RETNCODE)
           END-EXEC
           MOVE 'SM003' TO ERRMSGO.
       21C-EXIT.
           EXIT.

       21D-CLOSE-SM004.
           MOVE TICKETI TO WS-TICKET-PASSED
           MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
           EXEC CICS LINK
                PROGRAM('SM004')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-LENGTH)
                RESP(WS-RETNCODE)
           END-EXEC.
       21D-EXIT.
           EXIT.
       
       21E-APPROVE-SM005.
           MOVE TICKETI TO WS-TICKET-PASSED
           MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
           EXEC CICS LINK
                PROGRAM('SM005')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-LENGTH)
                RESP(WS-RETNCODE)
           END-EXEC.
       21R-EXIT.
           EXIT.

       21F-CANCEL-SM006.
           MOVE TICKETI TO WS-TICKET-PASSED
           MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
           EXEC CICS LINK
                PROGRAM('SM006')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-LENGTH)
           END-EXEC.
       21F-EXIT.
           EXIT.
           
       21G-ADMIN-UA001.
           EXEC CICS LINK
                PROGRAM('UA001')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-LENGTH)
                RESP(WS-RETNCODE)
           END-EXEC
           MOVE 'UA001' TO ERRMSGO.
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
       
       300-VERIFY-USER.
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
       
