       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM012.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-MAP  VALUE 'SM012M'        PIC X(7).
       01  WS-TIME				       PIC 9(15) COMP-3.
       01  WS-DATE                               PIC 9(10).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(10). 
       01  WS-LENGHTS.
           05 WS-LENGTH                          PIC S9(4) COMP. 
           05 WS-COMMAREA-LEN                    PIC S9(9) COMP-3.
       01  WS-REC-LENGTH                         PIC S9(4) 
           COMP VALUE +228. 

       01  WS-QNAME                              PIC X(08). 
       01  WS-FLAGS.
           05 WS-FLAG                            PIC X    VALUE 'N'.
           05 WS-PAGE-CHANGED                    PIC X VALUE 'N'.
           05 WS-PAGE-INVALID                    PIC X.   
       01  WS-END                                PIC X(14) VALUE
           'END PROCESSING'.
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
           05 WS-INVALID-PFKEY                   PIC X(21) VALUE
               'INAVLID PFKEY PRESSED'.   

       01  WS-KEYB.                       
           05 WS-KEYB6                   PIC X(06) VALUE LOW-VALUES.
           05 FILLER REDEFINES WS-KEYB6.
              10 FILLER                  PIC X(03).
              10 WS-KEYB3                PIC 9(03). 
      *    05 WS-KEYB1                   PIC X(01) VALUE HIGH-VALUES.    
       01  WS-LOG-REC. 
           05 WS-LOG-KEYS.
              10 WS-LOG-TICKET-ID                   PIC X(06).  
              10 WS-TIX-ID                  REDEFINES WS-LOG-TICKET-ID.
                 15 WS-T-ID                         PIC X(06).
      *    10 10 FILLER                          PIC X.
              10 WS-LOG-SEQ-NUMBER                  PIC 9(03).
           05 WS-LOG-LAST-UPD                    PIC X(20). 
           05 WS-LOG-LAST-UPD-BY                 PIC X(08). 
           05 WS-LOG-UPD-REMARKS                 PIC X(50).
           05 FILLER                             PIC X(02).

       COPY SM012S.
      *----------------------------------------------------------------*     
      *                     COPYBOOK REDEFINES                         *
      *----------------------------------------------------------------*
       01  DETL-LINE REDEFINES SM012MI.
           05 FILLER                              PIC X(85).
           05 DETL-DETAIL                         OCCURS 6.
              10 DETL-DETAILL                     PIC S9(4) COMP.
              10 DETL-DETAILF                     PIC X.
              10 FILLER REDEFINES DETL-DETAILF.
                 15 DETL-DETAILA                  PIC X.
	          10 DETL-DETAILI. 
                 15 DETAILS-UPD-BY                PIC X(08). 
                 15 FILLER1                       PIC X(02).  
                 15 DETAILS-UPD-DATE.
                    20  DETAIL-DD                 PIC X(02).
                    20  SLASH1                    PIC X.
                    20  DETAIL-MM                 PIC X(02).
                    20  SLASH2                    PIC X.
                    20  DETAIL-YYYY               PIC X(04).
                 15 FILLER2                       PIC X.
                 15 DETAILS-TIME                  PIC X(09).   
                 15 FILLER4                       PIC X(02).
                 15 DETAILS-REMARKS               PIC X(25).  
      *    05 FILLER                              PIC X(32).
           05 DETL-REMARKS                         OCCURS 6.
              10 DETL-REMARKSL                     PIC S9(4) COMP.
              10 DETL-REMARKSF                     PIC X.
              10 FILLER REDEFINES DETL-REMARKSF.
                 15 DETL-REMARKSA                  PIC X.
	          10 DETL-REMARKSI. 
                 15 REMARKS                        PIC X(25).
       01  WS-STATE-HOLDER                       PIC X.
       
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
           05 WS-TIX-DETL.
              10 WS-TIX-KEY                      PIC X(07).
              10 WS-TIX-KEY-NUM REDEFINES WS-TIX-KEY PIC 9(06).
              10 WS-TIX-REQUESTOR                PIC X(8).
              10 WS-TIX-STATUS                   PIC X(10).
              10 WS-TIX-TITLE                    PIC X(25).
              10 WS-TIX-DESC                     PIC X(100).
              10 WS-TIX-LAST-UPD                 PIC X(20). 
              10 WS-TIX-LAST-UPDBY               PIC X(8). 
              10 WS-TIX-UPD-REMARKS              PIC X(50).  
           05 WS-STF01-REC.
              10 WS-STF01-ID                     PIC X(07).
              10 WS-FILLER                       PIC X(03).
              10 WS-STF01-TITLE                  PIC X(25). 
              10 WS-FILLER                       PIC X(01).
              10 WS-STF01-STATUS                 PIC X(10).
              10 WS-FILLER                       PIC X(04).
              10 WS-STF01-LAST-UPD               PIC X(10). 
              10 WS-FILLER4                      PIC X(02). 
              10 WS-STF01-LAST-UPDBY             PIC X(8). 
              10 WS-STF01-REQ                    PIC X(8).  
           05 WS-FUSER                           PIC X(07).
           05 WS-PAGE                            PIC 9(02).
           05 WS-PAGE-END                        PIC 9(01).
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
   
	    COPY DFHAID.
       COPY DFHBMSCA.

      *----------------------------------------------------------------*
      *                          LINKAGE SECTION                       *
      *----------------------------------------------------------------*
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
           05 DF-TIX-DETL.
              10 DF-TIX-KEY                      PIC X(07).
              10 DF-TIX-KEY-NUM REDEFINES DF-TIX-KEY PIC 9(06).
              10 DF-TIX-REQUESTOR                PIC X(8).
              10 DF-TIX-STATUS                   PIC X(10).
              10 DF-TIX-TITLE                    PIC X(25).
              10 DF-TIX-DESC                     PIC X(100).
              10 DF-TIX-LAST-UPD                 PIC X(20). 
              10 DF-TIX-LAST-UPDBY               PIC X(8). 
              10 DF-TIX-UPD-REMARKS              PIC X(50).    
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
           05 DF-FUSER                           PIC X(07).
           05 DF-PAGE                            PIC 9(02).
           05 DF-PAGE-END                        PIC 9(01).
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
       100-PROCESS.
           STRING EIBTRMID DELIMITED BY SIZE
	               'SM12' DELIMITED BY SIZE
                  INTO WS-QNAME 
           EXEC CICS IGNORE CONDITION
                     ERROR 
           END-EXEC
           MOVE DFHCOMMAREA TO WS-COMMAREA
           MOVE WS-STATE TO WS-STATE-HOLDER
           IF WS-SM012-PGMID = 'SM003' OR WS-SM012-PGMID = 'SM004' OR 
              WS-SM012-PGMID = 'SM005' OR WS-SM012-PGMID = 'SM006'
              IF WS-SM012-STATE NOT = LOW-VALUES 
                 PERFORM 200-REC-MAP
                 PERFORM 300-CHECK-EIBAID
                 PERFORM 600-MOVE-Q-TO-SCREEN
                 PERFORM 110-NEW-MAP
              ELSE
                  MOVE 1 TO WS-SM012-STATE
                  MOVE 1 TO WS-PAGE
                  MOVE WS-PAGE TO PAGEO
                  PERFORM 900-MOVE-FILES-TO-Q
                  PERFORM 600-MOVE-Q-TO-SCREEN
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
	            ABSTIME  (WS-TIME)
	            DATESEP  ('/')
	            MMDDYYYY (DATEO)
                TIME     (TIMEO)
                TIMESEP  (':')    
           END-EXEC
           MOVE DFHBMASB TO TIMEA
           MOVE DFHBMASB TO DATEA
           MOVE LENGTH OF SM012MO TO WS-LENGTH 
           EXEC CICS SEND 
                MAP('SM012M')
                MAPSET('SM012S')
                FROM(SM012MO)
                LENGTH(WS-LENGTH)
                ERASE
           END-EXEC
           EXEC CICS RETURN
	        TRANSID('SM12')
		    COMMAREA(WS-COMMAREA)
	       END-EXEC.
       110-EXIT.
           EXIT.

       200-REC-MAP.
            EXEC CICS RECEIVE 
                MAP('SM012M')
                MAPSET('SM012S')
                INTO (SM012MI)
            END-EXEC.
       200-EXIT.
           EXIT.
       
       600-MOVE-Q-TO-SCREEN.
            MOVE PAGEO TO WS-PREV-PAGE
            MOVE LENGTH OF WS-LOG-REC TO WS-REC-LENGTH
            MOVE WS-QITEM-PAGE TO WS-QITEM
            EXEC CICS READQ TS
                      QUEUE(WS-QNAME)
                      INTO (WS-LOG-REC)
                      LENGTH(WS-REC-LENGTH)
                      ITEM (WS-QITEM)
             END-EXEC  
          
            MOVE 1 TO WS-INDEX

            PERFORM UNTIL WS-INDEX > 6
                IF EIBRESP = DFHRESP(NORMAL) AND 
                   WS-QITEM <= WS-QITEM-END  
                   MOVE WS-LOG-LAST-UPD-BY TO DETAILS-UPD-BY(WS-INDEX)
                   MOVE WS-LOG-LAST-UPD(1:10) TO 
                        DETAILS-UPD-DATE(WS-INDEX)
                   MOVE WS-LOG-LAST-UPD(11:9) TO DETAILS-TIME(WS-INDEX)
                   MOVE WS-LOG-UPD-REMARKS(1:25) TO 
                   DETAILS-REMARKS(WS-INDEX)
                   MOVE WS-LOG-UPD-REMARKS(26:24) TO REMARKS(WS-INDEX)
                   ADD 1 TO WS-INDEX
                   ADD 1 TO WS-QITEM
                   EXEC CICS READQ TS
                        QUEUE(WS-QNAME)
                        INTO (WS-LOG-REC)
                        ITEM (WS-QITEM)
                  END-EXEC  
               ELSE
                  MOVE SPACES TO DETL-DETAILI(WS-INDEX)
                  MOVE SPACES TO DETL-REMARKSI(WS-INDEX)
                  MOVE WS-LAST-PAGE  TO ERRMSGO
                  ADD 1 TO WS-INDEX
               END-IF
            END-PERFORM
            MOVE DETAILS-UPD-BY(1) TO WS-FUSER.

       600-EXIT.
           EXIT.

       900-MOVE-FILES-TO-Q.
           MOVE LOW-VALUES TO WS-LOG-KEYS. 
           MOVE LENGTH OF WS-LOG-REC TO WS-REC-LENGTH
            EXEC CICS 
               STARTBR FILE('STF002E')
               RIDFLD (WS-LOG-KEYS)
               GTEQ
            END-EXEC
                 
            EXEC CICS 
                READNEXT FILE('STF002E')
                INTO (WS-LOG-REC)
                RIDFLD (WS-LOG-KEYS)
            END-EXEC

           EXEC CICS WRITEQ TS
                     QUEUE(WS-QNAME)
                     FROM (WS-LOG-REC)
                     LENGTH (WS-REC-LENGTH)
                     ITEM (WS-QITEM)
           END-EXEC
           MOVE WS-QITEM TO WS-QITEM-START
           MOVE 1 TO WS-TOTAL-QITEM
           PERFORM UNTIL EIBRESP NOT = DFHRESP(NORMAL)
               MOVE WS-QITEM TO WS-QITEM-END
               EXEC CICS 
                    READNEXT FILE('STF002E')
                    INTO (WS-LOG-REC)
                    RIDFLD (WS-LOG-KEYS)
               END-EXEC
               IF EIBRESP = DFHRESP(NORMAL)
                  EXEC CICS WRITEQ TS
                       QUEUE(WS-QNAME)
                       FROM (WS-LOG-REC)
                       LENGTH (WS-REC-LENGTH)
                       ITEM (WS-QITEM)
                  END-EXEC
                 MOVE WS-T-ID TO WS-KEYB6
                  ADD 1 TO WS-TOTAL-QITEM
               END-IF
           END-PERFORM
               DIVIDE WS-TOTAL-QITEM BY 6 GIVING WS-MAX-PAGE
               ADD 1 TO WS-MAX-PAGE
           EXEC CICS
                ENDBR FILE('STF002E')
           END-EXEC
           MOVE WS-QITEM-START TO WS-QITEM-PAGE.

       900-EXIT.
           EXIT.        
       
       300-CHECK-EIBAID.
           EVALUATE EIBAID
             WHEN DFHCLEAR  
                MOVE WS-INVALID-PFKEY TO ERRMSGO
                PERFORM 600-MOVE-Q-TO-SCREEN  
                PERFORM 110-NEW-MAP
             WHEN DFHPF3
                  EXEC CICS 
                      SEND CONTROL 
                      ERASE
                  END-EXEC
                  EXEC CICS DELETEQ TS
                       QUEUE(WS-QNAME)
                  END-EXEC  
                  EVALUATE WS-SM012-PGMID
                    WHEN 'SM003'
                       MOVE 'SM012' TO WS-SM012-PGMID
                       MOVE WS-STATE-HOLDER TO WS-STATE
                       EXEC CICS XCTL
                            PROGRAM('SM003')
                       END-EXEC
                    WHEN 'SM004'  
                       MOVE 'SM012' TO WS-SM012-PGMID
                       MOVE WS-STATE-HOLDER TO WS-STATE
                       EXEC CICS XCTL
                            PROGRAM('SM004')
                       END-EXEC
                    WHEN 'SM005'
                       MOVE 'SM012' TO WS-SM012-PGMID
                       MOVE WS-STATE-HOLDER TO WS-STATE
                       EXEC CICS XCTL
                            PROGRAM('SM005')
                            COMMAREA(WS-COMMAREA)
                       END-EXEC 
                    WHEN 'SM006'
                       MOVE 'SM012' TO WS-SM012-PGMID
                       MOVE WS-STATE-HOLDER TO WS-STATE
                       EXEC CICS XCTL
                            PROGRAM('SM006')
                       END-EXEC     
                  END-EVALUATE      
             WHEN DFHPF7         
                PERFORM 310-PF7-PAGE-UP

             WHEN DFHPF8         
                PERFORM 320-PF8-PAGE-DOWN
             WHEN DFHENTER
                     PERFORM 550-PAGE-CHANGED
     
           END-EVALUATE.   
       300-EXIT. 
           EXIT.

               
       310-PF7-PAGE-UP.
           SUBTRACT 6 FROM WS-QITEM-PAGE
           IF WS-QITEM-PAGE = WS-QITEM-START OR 
              WS-QITEM-PAGE <  WS-QITEM-START
              MOVE WS-FIRST-PAGE TO ERRMSGO
              MOVE 1 TO WS-PAGE
              MOVE WS-QITEM-START TO WS-QITEM-PAGE
           ELSE
      *      MOVE WS-SELECT-OPTION TO ERRMSGO
             SUBTRACT 1 FROM WS-PAGE 
             MOVE WS-PAGE TO PAGEO
             CONTINUE
           END-IF
           MOVE WS-PAGE TO PAGEO
           PERFORM 600-MOVE-Q-TO-SCREEN   
           PERFORM 110-NEW-MAP.
       310-EXIT.
           EXIT.    
       

       320-PF8-PAGE-DOWN.
           ADD 6 TO WS-QITEM-PAGE
           IF WS-QITEM-PAGE > WS-QITEM-END 
              SUBTRACT 6 FROM WS-QITEM-PAGE
              MOVE WS-LAST-PAGE TO ERRMSGO
           ELSE
      *       MOVE WS-SELECT-OPTION TO ERRMSGO
              ADD 1 TO WS-PAGE
              MOVE WS-PAGE TO PAGEO
              PERFORM 600-MOVE-Q-TO-SCREEN
           END-IF.   
           PERFORM 110-NEW-MAP.
       320-EXIT.
           EXIT.

       530-PAGE-UP-ENTER.
           IF WS-QITEM-PAGE > WS-QITEM-END 
              SUBTRACT 6 FROM WS-QITEM-PAGE
              MOVE WS-LAST-PAGE TO ERRMSGO
           ELSE
              MOVE CURR-PAGE TO WS-PAGE
              MOVE WS-PAGE TO PAGEO
              PERFORM 600-MOVE-Q-TO-SCREEN
           END-IF
           PERFORM 110-NEW-MAP.
       530-EXIT.
           EXIT.

       540-PAGEDOWN-ENTER.
           IF WS-QITEM-PAGE = WS-QITEM-START OR 
              WS-QITEM-PAGE <  WS-QITEM-START
              MOVE WS-FIRST-PAGE TO ERRMSGO
              MOVE 1 TO WS-PAGE
              MOVE WS-QITEM-START TO WS-QITEM-PAGE
           ELSE
             SUBTRACT CURR-PAGE FROM WS-PAGE 
             MOVE WS-PAGE TO PAGEO
           END-IF
           MOVE WS-PAGE TO PAGEO
           PERFORM 600-MOVE-Q-TO-SCREEN   
           PERFORM 110-NEW-MAP.
       540-EXIT.
           EXIT.

       550-PAGE-CHANGED.
           MOVE PAGEI TO WS-CURR-PAGE
           IF CURR-PAGE IS NUMERIC  
              PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL 
                      WS-INDEX > WS-MAX-PAGE      
                MOVE 'Y' TO WS-PAGE-INVALID       
                IF CURR-PAGE = WS-INDEX
                   MOVE 'N' TO WS-PAGE-INVALID
                   SUBTRACT PREV-PAGE FROM CURR-PAGE GIVING 
                            WS-PAGE-UPDOWN    
                   EVALUATE TRUE            
                    WHEN CURR-PAGE > PREV-PAGE
                      IF WS-PAGE-UPDOWN = 1       
                         ADD 6 TO WS-QITEM-PAGE
                         PERFORM 530-PAGE-UP-ENTER
                          MOVE 'Y' TO WS-PAGE-CHANGED
                      ELSE    
                         MOVE 1 TO WS-I
                         PERFORM UNTIL WS-I = CURR-PAGE
                                 ADD 6 TO WS-QITEM-PAGE
                                 ADD 1 TO WS-I
                         END-PERFORM
                         PERFORM 530-PAGE-UP-ENTER
                          MOVE 'Y' TO WS-PAGE-CHANGED
                      END-IF             
                    WHEN CURR-PAGE < PREV-PAGE
                      IF WS-PAGE-UPDOWN = 1
                         SUBTRACT 6 FROM WS-QITEM-PAGE
                        PERFORM 540-PAGEDOWN-ENTER
                         MOVE 'Y' TO WS-PAGE-CHANGED
                      ELSE 
                         MOVE PREV-PAGE TO WS-I
                         PERFORM UNTIL WS-I = CURR-PAGE
                                 SUBTRACT 6 FROM WS-QITEM-PAGE
                                 SUBTRACT 1 FROM WS-I
                         END-PERFORM   
                         PERFORM 540-PAGEDOWN-ENTER
                          MOVE 'Y' TO WS-PAGE-CHANGED
                      END-IF   
      *             WHEN OTHER
      *                  PERFORM 560-CHECK-OPTION-FIELDS
                   END-EVALUATE
                END-IF
              END-PERFORM
              IF WS-PAGE-INVALID = 'Y'
                 EVALUATE TRUE
                   WHEN CURR-PAGE > WS-MAX-PAGE 
                        MOVE 1 TO WS-I
                        PERFORM UNTIL WS-I = WS-MAX-PAGE
                                ADD 6 TO WS-QITEM-PAGE
                                ADD 1 TO WS-I
                        END-PERFORM
                        MOVE -1 TO PAGEL
                        MOVE WS-MAX-PAGE TO PAGEO
                        MOVE WS-MAX-PAGE TO WS-PAGE
                       IF WS-QITEM-PAGE > WS-QITEM-END 
                          SUBTRACT 6 FROM WS-QITEM-PAGE
                          MOVE WS-LAST-PAGE TO ERRMSGO
                       ELSE
                          PERFORM 600-MOVE-Q-TO-SCREEN
                       END-IF
                       PERFORM 110-NEW-MAP         
                   WHEN CURR-PAGE < 1
                        MOVE PREV-PAGE TO WS-I
                        PERFORM UNTIL WS-I = CURR-PAGE
                                SUBTRACT 6 FROM WS-QITEM-PAGE
                                SUBTRACT 1 FROM WS-I
                        END-PERFORM   
                        PERFORM 540-PAGEDOWN-ENTER
                 END-EVALUATE   
              ELSE 
                  MOVE 'N' TO WS-PAGE-INVALID   
              END-IF   
           ELSE 
               PERFORM 600-MOVE-Q-TO-SCREEN
               PERFORM 110-NEW-MAP  
           END-IF.
       550-EXIT. 
           EXIT.        