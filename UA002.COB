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
       01  WS-FLAG                               PIC X    VALUE 'N'.    
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
       01  WS-USER-REC.
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

       01  WS-COUNTERS.
           05 WS-INDEX                           PIC 9(02).   
       01  WS-ERRMSGS.
           05 WS-END                             PIC X(15) VALUE
              'END PREOCESSING'.
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
           05 WS-INVALID-VALUE                   PIC X(46) VALUE
              'INVALID VALUE. PLEASE CORRECT HIGHLIGHT FIELDS'. 
           05 WS-INVALID-PFKEY                   PIC X(21) VALUE
               'INAVLID PFKEY PRESSED'.   
                    
       01  WS-PAGE-NO                            PIC 9(03) VALUE 1.
       77 WS-RETNCODE        PIC S9(8) COMP.
       77 WS-RETNCODE2        PIC S9(8) COMP.
      *--------------------------------------------------------------*
      *               COMMAREA WORKING STORAGE                       *
      *--------------------------------------------------------------*
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
           05 WS-ADDED-USER                   PIC X(08).
           05 WS-USER-DETL.
              10 WS-USER-ADD                     PIC X(08).
              10 WS-USER-REQ                     PIC X. 
              10 WS-USER-ADMIN                   PIC X.  
              10 WS-USER-APPROVER                PIC X.
              10 WS-USER-SERVICE                 PIC X.     

      *----------------------------------------------------------------*
      *                         LINKAGE SECTION                        *
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
           05 DF-ADDED-USER                      PIC X(08).
           05 DF-USER-DETL.
              10 DF-USER                         PIC X(08). 
              10 DF-USER-REQ                     PIC X. 
              10 DF-USER-ADMIN                   PIC X.  
              10 DF-USER-APPROVER                PIC X.
              10 DF-USER-SERVICE                 PIC X.  
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
              EVALUATE TRUE 
                WHEN WS-STATE = LOW-VALUES
                   MOVE 1 TO WS-STATE
                   MOVE 'ENTER USER DETAILS AND PRESS PF2' TO ERRMSG8O 
                   MOVE -1 TO USRIDL
                   PERFORM 111-CREATE-MAP
                WHEN WS-STATE = 1               
                     PERFORM 200-REC-MAP
                     PERFORM 120-MOVE-DATA-TO-HOLDER
                     PERFORM 300-CHECK-EIBAID
                     PERFORM 111-CREATE-MAP
                WHEN WS-STATE = 2
                     PERFORM 200-REC-MAP
      *              PERFORM 120-MOVE-DATA-TO-HOLDER
                     PERFORM 300-CHECK-EIBAID
                     PERFORM 111-CREATE-MAP
                WHEN WS-STATE = 3
                     PERFORM 200-REC-MAP
      *              PERFORM 120-MOVE-DATA-TO-HOLDER
                     PERFORM 300-CHECK-EIBAID
                     PERFORM 111-CREATE-MAP     
              END-EVALUATE       
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
      *Minove sa commarea para pag nagescape ung user di mabubura sa screen
      *inputs nila
       120-MOVE-DATA-TO-HOLDER.
           MOVE USRIDI TO WS-USER-ADD
           MOVE REQTAGI TO WS-USER-REQ
           MOVE ADMTAGI TO WS-USER-ADMIN
           MOVE APRTAGI TO WS-USER-APPROVER
           MOVE SRVTAGI TO WS-USER-SERVICE.
       120-EXIT.
           EXIT.

       200-REC-MAP.
           EXEC CICS RECEIVE
                MAP('UA002M')
                MAPSET(WS-CURRENT-MAP)
                INTO (UA002MI)
                RESP(WS-RETNCODE)
           END-EXEC
      *    Eto pang handle ng escape keypress 
           IF EIBRESP = DFHRESP(MAPFAIL)
              MOVE WS-INVALID-PFKEY TO ERRMSG8O
              MOVE WS-USER-ADD TO USRIDI  
              MOVE WS-USER-REQ TO REQTAGI
              MOVE WS-USER-ADMIN TO ADMTAGI
              MOVE WS-USER-APPROVER TO APRTAGI
              MOVE WS-USER-SERVICE TO SRVTAGI
              MOVE -1 TO USRIDL
              MOVE 1 TO WS-STATE
              PERFORM 111-CREATE-MAP
           END-IF.
       200-EXIT.
           EXIT.    

       300-CHECK-EIBAID.
           EVALUATE EIBAID
            WHEN DFHPF2
                 EVALUATE TRUE 
                   WHEN WS-STATE = 1
                      MOVE 2 TO WS-STATE
                      PERFORM 310-VALIDATE-DETAILS
                      IF WS-FLAG = 'Y'
                          MOVE 1 TO WS-STATE   
                      ELSE 
                          MOVE 3 TO WS-STATE
                         
                          STRING WS-USER-ADD DELIMITED BY SIZE
                            'CREATED PRESS ENTER TO' DELIMITED BY SIZE
                            ' CREATE ANOTHER USER ID' DELIMITED BY SIZE
                            INTO ERRMSG8O
                          END-STRING  
                          MOVE WS-USER-ADD TO USRIDI  
                          MOVE WS-USER-REQ TO REQTAGI
                          MOVE WS-USER-ADMIN TO ADMTAGI
                          MOVE WS-USER-APPROVER TO APRTAGI
                          MOVE WS-USER-SERVICE TO SRVTAGI
                          PERFORM 320-SET-PROTECTED
                          MOVE -1 TO USRIDL
                          MOVE WS-USER-ADD TO WS-USERID
                          MOVE WS-USER-REQ TO WS-REQUESTOR
                          MOVE WS-USER-ADMIN TO WS-ADMIN
                          MOVE WS-USER-APPROVER TO WS-APPROVER
                          MOVE WS-USER-SERVICE TO WS-SERVICE
                          MOVE 'ISCB126 ' TO WS-UPDATEDBY
                          PERFORM 400-WRITE-USER
                      END-IF   
                   WHEN WS-STATE = 2
                        MOVE 3 TO WS-STATE
                          
                          STRING WS-USER-ADD DELIMITED BY SIZE
                            'CREATED PRESS ENTER TO' DELIMITED BY SIZE
                            ' CREATE ANOTHER USER ID' DELIMITED BY SIZE 
                            INTO ERRMSG8O
                          END-STRING 
                          MOVE WS-USER-ADD TO USRIDI  
                          MOVE WS-USER-REQ TO REQTAGI
                          MOVE WS-USER-ADMIN TO ADMTAGI
                          MOVE WS-USER-APPROVER TO APRTAGI
                          MOVE WS-USER-SERVICE TO SRVTAGI
                          PERFORM 320-SET-PROTECTED
                          MOVE -1 TO USRIDL
                          MOVE WS-USER-ADD TO WS-USERID
                          MOVE WS-USER-REQ TO WS-REQUESTOR
                          MOVE WS-USER-ADMIN TO WS-ADMIN
                          MOVE WS-USER-APPROVER TO WS-APPROVER
                          MOVE WS-USER-SERVICE TO WS-SERVICE
                          MOVE 'ISCB126 ' TO WS-UPDATEDBY

                          PERFORM 400-WRITE-USER
                   WHEN WS-STATE = 3
                        MOVE WS-USER-ADD TO USRIDI  
                        MOVE WS-USER-REQ TO REQTAGI
                        MOVE WS-USER-ADMIN TO ADMTAGI
                        MOVE WS-USER-APPROVER TO APRTAGI
                        MOVE WS-USER-SERVICE TO SRVTAGI
                        MOVE -1 TO USRIDL
                        PERFORM 320-SET-PROTECTED
                        CONTINUE
                 END-EVALUATE        
            WHEN DFHPF3
                 EXEC CICS SEND TEXT
                      FROM (WS-END)
                      LENGTH (+15)
                      ERASE
                 END-EXEC
                 EXEC CICS RETURN
                 END-EXEC   
            WHEN DFHPF5
                 EVALUATE TRUE
                   WHEN WS-STATE = 1 
                        MOVE 1 TO WS-STATE
                        MOVE LOW-VALUES TO USRIDI
                        MOVE LOW-VALUES TO REQTAGI
                        MOVE LOW-VALUES TO ADMTAGI
                        MOVE LOW-VALUES TO APRTAGI
                        MOVE LOW-VALUES TO SRVTAGI
                        MOVE 'ENTER USER DETAILS AND PRESS PF2'
                             TO ERRMSG8O 
                        MOVE -1 TO USRIDL  
                   WHEN WS-STATE = 2
                        MOVE 1 TO WS-STATE
                        MOVE LOW-VALUES TO USRIDI
                        MOVE LOW-VALUES TO REQTAGI
                        MOVE LOW-VALUES TO ADMTAGI
                        MOVE LOW-VALUES TO APRTAGI
                        MOVE LOW-VALUES TO SRVTAGI
                        MOVE 'ENTER USER DETAILS AND PRESS PF2'
                             TO ERRMSG8O 
                        MOVE -1 TO USRIDL    
                   WHEN WS-STATE = 3
                        MOVE WS-USER-ADD TO USRIDI  
                        MOVE WS-USER-REQ TO REQTAGI
                        MOVE WS-USER-ADMIN TO ADMTAGI
                        MOVE WS-USER-APPROVER TO APRTAGI
                        MOVE WS-USER-SERVICE TO SRVTAGI
                        MOVE -1 TO USRIDL
                        PERFORM 320-SET-PROTECTED
                        CONTINUE
                 END-EVALUATE                  
            WHEN DFHENTER
                 IF WS-STATE = 1
                    MOVE 2 TO WS-STATE
                    PERFORM 310-VALIDATE-DETAILS
                    IF WS-FLAG = 'Y'
                        MOVE 1 TO WS-STATE   
                     ELSE 
                        MOVE 2 TO WS-STATE
                        MOVE 'PRESS PF2 TO CONFIRM ADD' TO ERRMSG8O
                        PERFORM 320-SET-PROTECTED
                        MOVE -1 TO USRIDL
                    END-IF   
                   
                 ELSE 
                    IF WS-STATE = 2
                       MOVE 'PRESS PF2 TO ADD USER' TO ERRMSG8O 
                       PERFORM 320-SET-PROTECTED
                       MOVE -1 TO USRIDL
                       CONTINUE
                    ELSE 
                       IF WS-STATE = 3 
                          MOVE 1 TO WS-STATE
                          MOVE LOW-VALUES TO USRIDI
                          MOVE LOW-VALUES TO REQTAGI
                          MOVE LOW-VALUES TO ADMTAGI
                          MOVE LOW-VALUES TO APRTAGI
                          MOVE LOW-VALUES TO SRVTAGI
                          MOVE 'ENTER USER DETAILS AND PRESS PF2'
                               TO ERRMSG8O 
                          MOVE -1 TO USRIDL
                          
                       END-IF   
                    END-IF
                 END-IF 
            WHEN OTHER 
                 MOVE WS-INVALID-PFKEY TO ERRMSG8O
                 MOVE WS-USER-ADD TO USRIDI  
                 MOVE WS-USER-REQ TO REQTAGI
                 MOVE WS-USER-ADMIN TO ADMTAGI
                 MOVE WS-USER-APPROVER TO APRTAGI
                 MOVE WS-USER-SERVICE TO SRVTAGI
                 MOVE -1 TO USRIDL
                 EVALUATE TRUE 
                   WHEN WS-STATE = 1
                        MOVE 1 TO WS-STATE        
                   WHEN WS-STATE = 2
                        MOVE 2 TO WS-STATE 
                   WHEN WS-STATE = 3
                        MOVE 3 TO WS-STATE  
                 END-EVALUATE                            
           END-EVALUATE.
       300-EXIT.
           EXIT.

       310-VALIDATE-DETAILS.
           EVALUATE TRUE
             WHEN USRIDI = LOW-VALUES
                  MOVE 'USER ID IS REQUIRED' TO ERRMSG8O
                  MOVE 1 TO WS-STATE   
                  MOVE -1 TO USRIDL
                  MOVE DFHUNIMD TO USRIDA
                  MOVE 'Y' TO WS-FLAG
             WHEN REQTAGI = LOW-VALUES 
                  MOVE 'REQURESTOR TAG IS REQUIRED' TO ERRMSG8O
                  MOVE 1 TO WS-STATE
                  MOVE -1 TO REQTAGL
                  MOVE DFHUNIMD TO REQTAGA
                  MOVE 'Y' TO WS-FLAG
             WHEN ADMTAGI = LOW-VALUES
                  MOVE 'ADMIN TAG IS REQUIRED' TO ERRMSG8O
                  MOVE 1 TO WS-STATE   
                  MOVE -1 TO ADMTAGL
                  MOVE DFHUNIMD TO ADMTAGA
                  MOVE 'Y' TO WS-FLAG
             WHEN APRTAGI = LOW-VALUES
                  MOVE 'APPROVER TAG IS REQUIRED' TO ERRMSG8O
                  MOVE 1 TO WS-STATE
                  MOVE -1 TO APRTAGL
                  MOVE DFHUNIMD TO APRTAGA
                  MOVE 'Y' TO WS-FLAG
             WHEN SRVTAGI = LOW-VALUES
                  MOVE 'SERVICE TAG IS REQUIRED' TO ERRMSG8O  
                  MOVE 1 TO WS-STATE 
                  MOVE -1 TO SRVTAGL 
                  MOVE DFHUNIMD TO SRVTAGA  
                  MOVE 'Y' TO WS-FLAG               
           END-EVALUATE   

           IF REQTAGI NOT = LOW-VALUES   
              IF REQTAGI = 'Y' OR REQTAGI = 'N'
                 MOVE 2 TO WS-STATE   
              ELSE
                 MOVE WS-INVALID-VALUE TO ERRMSG8O
                 MOVE 'Y' TO WS-FLAG
                 MOVE -1 TO REQTAGL
                 MOVE DFHUNIMD TO REQTAGA
              END-IF 
           END-IF          
           IF ADMTAGI NOT = LOW-VALUES   
              IF ADMTAGI = 'Y' OR ADMTAGI = 'N'
                 MOVE 2 TO WS-STATE   
              ELSE
                 MOVE WS-INVALID-VALUE TO ERRMSG8O
                 MOVE 'Y' TO WS-FLAG
                 MOVE -1 TO ADMTAGL
                 MOVE DFHUNIMD TO ADMTAGA
              END-IF
           END-IF           
           IF APRTAGI NOT = LOW-VALUES   
              IF APRTAGI = 'Y' OR APRTAGI = 'N'
                 MOVE 2 TO WS-STATE   
              ELSE
                 MOVE WS-INVALID-VALUE TO ERRMSG8O
                 MOVE 'Y' TO WS-FLAG
                 MOVE -1 TO APRTAGL
                 MOVE DFHUNIMD TO APRTAGA
              END-IF
           END-IF         
           IF SRVTAGI NOT = LOW-VALUES   
              IF SRVTAGI = 'Y' OR SRVTAGI = 'N'
                 MOVE 2 TO WS-STATE   
              ELSE
                 MOVE WS-INVALID-VALUE TO ERRMSG8O
                 MOVE 'Y' TO WS-FLAG
                 MOVE -1 TO SRVTAGL
                 MOVE DFHUNIMD TO SRVTAGA   
              END-IF     
           END-IF.   
       310-EXIT.
           EXIT.
       
       320-SET-PROTECTED.
           MOVE DFHBMPRO TO USRIDA
           MOVE DFHBMPRO TO REQTAGA
           MOVE DFHBMPRO TO ADMTAGA
           MOVE DFHBMPRO TO APRTAGA
           MOVE DFHBMPRO TO SRVTAGA.
       320-EXIT.
           EXIT.
       
       330-SET-UNPROTECTED.
           MOVE DFHBMUNP TO USRIDA
           MOVE DFHBMUNP TO REQTAGA
           MOVE DFHBMUNP TO ADMTAGA
           MOVE DFHBMUNP TO APRTAGA
           MOVE DFHBMUNP TO SRVTAGA.
       330-EXIT.
           EXIT.    

       400-WRITE-USER.
           
           EXEC CICS WRITE
                FILE('uaf001')
                FROM(WS-USER-REC)
                RIDFLD(WS-USERID)
           END-EXEC

      *    Will display error message if user id already in records  
           EVALUATE TRUE
            WHEN EIBRESP = +14
                 STRING 'USER ID ALREADY EXIST' DELIMITED BY SIZE
                   ', USER NOT CREATED.' DELIMITED BY SIZE
                   'PRESS ENTER TO INPUT AGAIN.' DELIMITED BY SIZE
                    INTO ERRMSG8O
                 END-STRING   
                 MOVE 3 TO WS-STATE  
                 PERFORM 330-SET-UNPROTECTED    
            WHEN EIBRESP = +0
                 MOVE 3 TO WS-STATE
            WHEN OTHER 
                 MOVE 'USER NOT ADDED' TO ERRMSG8O     
           END-EVALUATE.     

       400-EXIT.
           EXIT.   
        