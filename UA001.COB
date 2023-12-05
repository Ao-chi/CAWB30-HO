       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UA001.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-MAP  VALUE 'UA001M'         PIC X(7).
       01  WS-TIME				 PIC 9(15).
       01  WS-DATE                               PIC 9(7).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(7).
       01  WS-LENGTH                             PIC S9(4) COMP. 
      *    minodify ko ng onti ung COMMAREA kasi cinonnect ko sa sm000
       01  WS-COMMAREA. 
           05 WS-PGMID                           PIC X(06).
           05 WS-STATE                           PIC X.
           05 WS-USERID.
            10 WS-USERID1         PIC X(07).
            10 FILLER             PIC X(01).
           05 USR-TYPE.
              15 USR-REQUESTOR                   PIC X.
              15 USR-ADMIN                       PIC X.  
              15 USR-APPROVER                    PIC X.
              15 USR-SERVICE                     PIC X. 
      *   05 WS-STATE             PIC X(01).
          05 WS-FLAG              PIC X(01).
          05 WS-CTL.
               10  WS-PAGE                PIC 9(02). 
               10  WS-QNAME               PIC X(08).
               10  WS-QITEM               PIC S9(4) COMP.
               10  WS-QITEM-START         PIC S9(4) COMP.
               10  WS-QITEM-END           PIC S9(4) COMP.
               10  WS-QITEM-PAGE          PIC S9(4) COMP.
               10  WS-CNT-UP              PIC 9(02) VALUE 0.
               10  WS-PAGE-X              PIC 9(02).
               10  WS-LASTPAGE            PIC 9(02).
          05	WS-BACKUP OCCURS 11.
	           10  WS-B-UD                PIC X(01).
	           10  WS-B-REQ               PIC X(01).
	           10  WS-B-ADMN              PIC X(01).
	           10  WS-B-APP               PIC X(01).
	           10  WS-B-SP                PIC X(01).


       01  WS-END                 PIC X(15) VALUE 'INVALID ACCESS'.
       77 WS-RETNCODE             PIC S9(8) COMP.

      *NEW STORAGE
  
       01  WS-RECB-LENGTH         PIC S9(4) COMP VALUE +20.
       01  WS-INDX                PIC 9(02). 
       01  WS-RID.
           05 WS-RID7              PIC X(07) VALUE LOW-VALUES.
           05 FILLER REDEFINES WS-RID7.
              10 FILLER                  PIC X(04).
              10 WS-RID3                 PIC 9(03).  
           05 WS-RID1                    PIC X(01) VALUE HIGH-VALUES.

       01  WS-REMAINDER           PIC 9(01).
       01  WS-FIRSTPAGE           PIC X(01).    
       01  WS-PAGEDIFF            PIC 9(02).
       01  FLAG-SEARCH            PIC X(01).
       01  WS-TEST                PIC X(07).
       01  WS-SUB-HOLDER          PIC 9(02).
       01  WS-PAGE-I              PIC 9(02).



       01  WS-REC.
          05  WS-UID.
               10 FILLER               PIC X(04).
               10 WS-UID2              PIC 9(03).
               10 FILLER               PIC X.   
          05  WS-UID-REDEF REDEFINES WS-UID.
               10  WS-USERID7      PIC X(7).
               10  FILLER          PIC X(1).
          05  WS-REQ              PIC X(01).
          05  WS-ADMN             PIC X(01).
          05  WS-APP              PIC X(01).
          05  WS-SP               PIC X(01).
          05  WS-UPBY             PIC X(08).

      *GALING SA COMMAREA
       01  WS-ID              PIC X(07) VALUE 'ISCB124'.

       01  WS-AADMIN          PIC X(01) VALUE 'Y'.


       COPY UA001S.

        01  WS-UA001 REDEFINES UA001MI.
           05  FILLER                  PIC X(52).
           05  UD  OCCURS 11. 
               10  UDL                 PIC S9(4) COMP.
               10  UDF                 PIC X.
               10  FILLER REDEFINES UDF.
                   15  UDA             PIC X.
               10  UDI                 PIC X(001).
           05  UID     OCCURS 11.
               10  UIDL                PIC S9(4) COMP.
               10  UIDF                PIC X.
               10  FILLER REDEFINES UIDF.
                   15  UIDA            PIC X.
               10  UIDI.
                   15 FILLER           PIC X(04).
                   15 WS-USERNUM       PIC 9(03). 
                   15 FILLER           PIC X. 
           05  REQ     OCCURS 11.
               10  REQL                PIC S9(4) COMP.
               10  REQF                PIC X.
               10  FILLER REDEFINES REQF.
                   15 REQA             PIC X.
               10  REQI                PIC X(001).
           05  ADMN      OCCURS 11.
               10  ADMNL               PIC S9(4) COMP.
               10  ADMNF               PIC X.
               10  FILLER REDEFINES ADMNF.
                   15  ADMNA           PIC X.
               10  ADMNI               PIC X(001).
           05  APP     OCCURS 11.
               10  APPL                PIC S9(4) COMP.
               10  APPF                PIC X.
               10  FILLER REDEFINES APPF.
                   15  APPA            PIC X.
               10  APPI                PIC X(001).
           05  SP          OCCURS 11.
               10  SPL                 PIC S9(4) COMP.
               10  SPF                 PIC X.
               10  FILLER REDEFINES SPF.
                   15  SPA             PIC X.
               10  SPI                 PIC X(001).

      
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
             10 DF-USER-REQUESTOR  PIC X(01).
             10 DF-USER-ADMIN      PIC X(01).
             10 DF-USER-APPROVER   PIC X(01).
             10 DF-USER-SERVICE    PIC X(01).
      *    05 DF-STATE             PIC X(01).
           05 DF-TICKET-NUM        PIC X(07).
           05 DF-FLAG              PIC X(01).
           05 DF-CTL.
             10  DF-PAGE                PIC 9(02). 
             10  DF-QNAME               PIC X(08).
             10  DF-QITEM               PIC S9(4) COMP.
             10  DF-QITEM-START         PIC S9(4) COMP.
             10  DF-QITEM-END           PIC S9(4) COMP.
             10  DF-QITEM-PAGE          PIC S9(4) COMP.
             10  DF-CNT-UP              PIC 9(02).
             10  DF-PAGE-X              PIC 9(02).
             10  DF-LASTPAGE            PIC 9(02).
           05 DF-BACKUP OCCURS 11.
	         10  DF-B-UD                PIC X(01).
	         10  DF-B-REQ               PIC X(01).
	         10  DF-B-ADMN              PIC X(01).
	         10  DF-B-APP               PIC X(01).
	         10  DF-B-SP                PIC X(01).

      
       PROCEDURE DIVISION.
       100-PROCESS SECTION.
           EXEC CICS IGNORE CONDITION
                     ERROR
           END-EXEC     
           MOVE DFHCOMMAREA  TO WS-COMMAREA
           MOVE LENGTH OF UA001MI TO WS-LENGTH
           STRING EIBTRMID DELIMITED BY SIZE
	              'UA01' DELIMITED BY SIZE
                  INTO WS-QNAME
           END-STRING

           IF WS-STATE  = 'A' OR 'B' OR 'C' OR  SPACE
              CONTINUE
           ELSE
              MOVE SPACE TO WS-STATE
           END-IF

           MOVE LENGTH OF UA001MI TO WS-LENGTH
      *    ginamit ko ung values ng sa commarea
           IF WS-PGMID = 'SM000' OR 'UA001' OR 'UA002' AND 
              USR-ADMIN = 'Y'
      *    IF EIBTRNID = 'SW00' OR 'SM00' OR 'UA01' AND WS-AADMIN = 'Y'

      *        IF EIBCALEN = +0 
      *      ung state piinang check ko kasi un ung galing sa commarea
      *    pag ung eibcalen kasi di sya pwede = +0 kasi may laman ung pinasang commarea.
               IF WS-STATE = LOW-VALUES or SPACES
                   PERFORM 2000-READ-TRANSFER-FILE
                   MOVE 1 TO WS-PAGE
                   MOVE WS-PAGE TO PAGEO

               END-IF

               PERFORM 1100-RMAP
               PERFORM 3000-CHECK-AID
           

                   EVALUATE TRUE
                       WHEN WS-STATE = SPACE
                          PERFORM 2100-TSQ-DISPLAY
                       
                       WHEN WS-STATE = 'A'
                           CONTINUE
                       
                       WHEN WS-STATE = 'B'
                           CONTINUE
                       
                       WHEN WS-STATE = 'C'
                           CONTINUE


                   END-EVALUATE
       
               PERFORM 3100-MSG-AID
               MOVE WS-PAGE TO WS-PAGE-X
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
           EXEC CICS ASSIGN
               USERID(WS-USERID)
           END-EXEC

           EXEC CICS ASKTIME
               ABSTIME  (WS-TIME)
           END-EXEC
       
           EXEC CICS FORMATTIME
               ABSTIME  (WS-TIME)
               DATESEP  ('/')
               DDMMYYYY   (DATEO)
               TIMESEP    (':')
               TIME       (TIMEO)
           END-EXEC
         
           EXEC CICS
               SEND MAP ('UA001M')
               MAPSET ('UA001S')
               FROM (UA001MO)
               LENGTH(WS-LENGTH)
               ERASE
           END-EXEC
           
           EXEC CICS RETURN
               TRANSID('UA01')
               COMMAREA(WS-COMMAREA)
           END-EXEC.

       1000-EXIT. EXIT.

       1100-RMAP.
           EXEC CICS 
             RECEIVE MAP('UA001M')
             MAPSET('UA001S')
             INTO (UA001MI)
           END-EXEC.
       1100-EXIT. EXIT.

       2000-READ-TRANSFER-FILE.
           MOVE LOW-VALUES TO WS-RID
           EXEC CICS
               STARTBR FILE('UAF001')
               RIDFLD (WS-RID)
               GTEQ
           END-EXEC

           EXEC CICS
               READNEXT FILE('UAF001')
               INTO (WS-REC)
               RIDFLD (WS-RID)
           END-EXEC

           EXEC CICS WRITEQ TS
               QUEUE (WS-QNAME)
               FROM (WS-REC)
               LENGTH (WS-RECB-LENGTH)
               ITEM (WS-QITEM)
           END-EXEC

           MOVE    WS-QITEM TO WS-QITEM-START

           PERFORM UNTIL EIBRESP NOT = DFHRESP (NORMAL)
               MOVE WS-QITEM TO WS-QITEM-END
               EXEC CICS
                   READNEXT FILE('UAF001')
                   INTO (WS-REC)
                   RIDFLD (WS-RID)
               END-EXEC

               IF EIBRESP = DFHRESP (NORMAL)
                   EXEC CICS WRITEQ TS
                       QUEUE (WS-QNAME)
                       FROM (WS-REC)
                       LENGTH (WS-RECB-LENGTH)
                       ITEM (WS-QITEM)
                   END-EXEC
                   MOVE WS-USERID7 TO WS-RID7
                   MOVE HIGH-VALUES TO WS-RID1


               END-IF
           END-PERFORM

           EXEC CICS
               ENDBR FILE('UAF001')
           END-EXEC
           PERFORM 2001-SET-LASTPAGE
           MOVE WS-QITEM-START TO WS-QITEM-PAGE.
       2000-EXIT. EXIT.

       2001-SET-LASTPAGE.
           SUBTRACT WS-QITEM-START FROM WS-QITEM-END GIVING
                    WS-SUB-HOLDER  

           DIVIDE WS-SUB-HOLDER   BY 11 GIVING WS-LASTPAGE 
                   REMAINDER WS-REMAINDER 

           IF WS-REMAINDER NOT = 0 
           
               ADD 1 TO WS-LASTPAGE

           END-IF.

       2001-EXIT. EXIT.

       2100-TSQ-DISPLAY. 

           MOVE WS-QITEM-PAGE TO WS-QITEM
           EXEC CICS READQ TS
               QUEUE(WS-QNAME)
               INTO (WS-REC)
               LENGTH(WS-RECB-LENGTH)
               ITEM (WS-QITEM)
           END-EXEC  

           MOVE 1 TO WS-INDX

           PERFORM UNTIL WS-INDX > 11
               IF EIBRESP = DFHRESP (NORMAL) AND
                  WS-QITEM <= WS-QITEM-END
                   
      *             MOVE '_'    TO UDI(WS-INDX)
                   MOVE WS-UID TO UIDI(WS-INDX)
                   MOVE WS-REQ TO REQI(WS-INDX)
                   MOVE WS-ADMN TO ADMNI(WS-INDX)
                   MOVE WS-APP TO APPI(WS-INDX)
                   MOVE WS-SP TO SPI(WS-INDX)

                   ADD 1 TO WS-QITEM

                   EXEC CICS READQ TS
                       QUEUE(WS-QNAME)
                       INTO (WS-REC)
                       LENGTH(WS-RECB-LENGTH)
                       ITEM (WS-QITEM)
                   END-EXEC 
               ELSE
                   
                  
                  MOVE SPACES TO UDI(WS-INDX)
                  MOVE SPACES TO UIDI(WS-INDX)
                  MOVE SPACES TO REQI(WS-INDX)
                  MOVE SPACES TO ADMNI(WS-INDX)
                  MOVE SPACES TO APPI(WS-INDX)
                  MOVE SPACES TO SPI(WS-INDX)
                  MOVE 'THIS IS THE LAST PAGE'  TO MSGO

                  MOVE DFHBMPRO TO UDA(WS-INDX)
                  MOVE DFHBMPRO TO UDA(WS-INDX)
                  MOVE DFHBMPRO TO UIDA(WS-INDX)
                  MOVE DFHBMPRO TO REQA(WS-INDX)
                  MOVE DFHBMPRO TO ADMNA(WS-INDX)
                  MOVE DFHBMPRO TO APPA(WS-INDX)
                  MOVE DFHBMPRO TO SPA(WS-INDX)
               END-IF
               ADD 1 TO WS-INDX
           END-PERFORM.

       2100-EXIT. EXIT.

      


       3000-CHECK-AID.


           EVALUATE TRUE

               WHEN EIBAID = DFHENTER
                   IF WS-STATE =  'A' AND WS-FLAG = 'N'
                   IF PAGEO IS NUMERIC
                       IF PAGEO =  WS-PAGE-X
                           CONTINUE
                       ELSE
                           MOVE PAGEI TO WS-PAGE-I
                           IF WS-PAGE-I > WS-LASTPAGE
                               MOVE 'X' TO WS-FLAG
                           END-IF

                           IF WS-PAGE-I >= 1 AND WS-FLAG  = 'N'

                               SUBTRACT 1 FROM WS-PAGE-I 
                                   GIVING WS-SUB-HOLDER

                               IF WS-SUB-HOLDER = 0
                                   MOVE WS-QITEM-START TO WS-QITEM-PAGE
                               END-IF                          

                               IF WS-SUB-HOLDER  >= 1 
                                   COMPUTE WS-QITEM-PAGE =
                                   WS-QITEM-PAGE + (WS-SUB-HOLDER * 11)
                               END-IF 
                               
                               MOVE 'O' TO WS-FLAG                   
                         END-IF
                   END-IF
                       MOVE 'Y' TO WS-FLAG

                   ELSE
                       MOVE '1' TO WS-FLAG
                   END-IF
                   END-IF


                   IF WS-STATE = SPACE AND WS-FLAG = NULL
                       MOVE 'N' TO WS-FLAG
                   END-IF

               WHEN EIBAID = DFHPF2
                   MOVE 'GO TO UA002'  TO MSGO


               WHEN EIBAID = DFHPF3
                   MOVE 'GO TO SM00'  TO MSGO

               WHEN EIBAID = DFHPF5
                   MOVE 'N' TO WS-FLAG 
                   MOVE 'A' TO WS-STATE
                   PERFORM 7000-FIND-SEARCH

               WHEN EIBAID = DFHPF7
                   CONTINUE


               WHEN EIBAID = DFHPF8
                   CONTINUE




               WHEN EIBAID = DFHPF12
                   EXEC CICS DELETEQ TS
                    QUEUE(WS-QNAME)
                   END-EXEC

               WHEN EIBAID = DFHCLEAR
                   CONTINUE

               WHEN OTHER
                   CONTINUE
           END-EVALUATE.


       3000-EXIT. EXIT.

       
       3100-MSG-AID.
           
           EVALUATE TRUE

               WHEN EIBAID = DFHENTER
                   IF WS-STATE = 'A' AND WS-FLAG = '1'
                       PERFORM 2100-TSQ-DISPLAY

                       MOVE 'PAGE NUMBER ERROR. NUMERIC ONLY ' TO MSGO
                       MOVE 'N' TO WS-FLAG
                   END-IF

                   IF WS-STATE = SPACE AND WS-FLAG = 'N'
                       MOVE 'A' TO WS-STATE
                   END-IF

                   IF WS-STATE = 'A' AND WS-FLAG = 'N'
                       PERFORM 2100-TSQ-DISPLAY
                    END-IF 

                   IF WS-STATE = 'A' AND WS-FLAG = 'X'
                       PERFORM 2100-TSQ-DISPLAY
                       MOVE 'PAGE NUMBER INPUT ERROR. THE LASTPAGE IS ' 
                          TO MSGO(01:41)
                       MOVE WS-LASTPAGE TO MSGO(42:02)
                       MOVE 'N' TO WS-FLAG
                   END-IF

                    IF WS-STATE = 'A' AND WS-FLAG = 'O'
                    
                       PERFORM 2100-TSQ-DISPLAY
                       MOVE WS-PAGE-I TO WS-PAGE
                       MOVE 'N' TO WS-FLAG
                    END-IF 

              

               WHEN EIBAID = DFHPF5
               IF WS-STATE = 'A' AND WS-FLAG = 'N' AND FLAG-SEARCH = 'Y'
                   MOVE 'RECORD FOUND' TO MSGO
               END-IF

               IF WS-STATE = 'A' AND WS-FLAG = 'N' AND FLAG-SEARCH = 'N'
                   MOVE 'RECORD NOT FOUND' TO MSGO
               END-IF
            

               WHEN EIBAID = DFHENTER
                   IF WS-STATE = 'A' AND WS-FLAG = 'N'
                     PERFORM 4000-CHECK-UPDATE
                   END-IF

               WHEN EIBAID = DFHPF7
                   SUBTRACT 11 FROM WS-QITEM-PAGE
                   IF WS-QITEM-START = WS-QITEM-PAGE OR
                      WS-QITEM-START > WS-QITEM-PAGE
                       MOVE 'THIS IS THE FIRST PAGE' TO MSGO
                       MOVE WS-QITEM-START TO WS-QITEM-PAGE
                       MOVE 1 TO WS-PAGE
                   ELSE
                       SUBTRACT 1 FROM WS-PAGE
                   END-IF
                   MOVE WS-PAGE TO PAGEO

                   PERFORM 2100-TSQ-DISPLAY


               WHEN EIBAID = DFHPF8
                   ADD 11 TO WS-QITEM-PAGE
                   IF WS-QITEM-PAGE  > WS-QITEM-END
                       SUBTRACT 11 FROM WS-QITEM-PAGE
                       MOVE 'THIS IS THE LAST PAGE' TO MSGO
                   ELSE
                       ADD 1 TO WS-PAGE
                   END-IF

                   MOVE WS-PAGE TO PAGEO
                   PERFORM 2100-TSQ-DISPLAY
           

               WHEN EIBAID = DFHCLEAR
                   MOVE WS-PAGE TO PAGEO
                   IF WS-STATE = 'A' AND WS-FLAG = 'N'
                       PERFORM 2100-TSQ-DISPLAY
                       MOVE 'INVALID PFKEY PRESSED' TO MSGO
                   END-IF

               WHEN OTHER
                   MOVE 'INVALID PFKEY PRESSED' TO MSGO
           END-EVALUATE.


       3100-EXIT. EXIT.

       4000-CHECK-UPDATE.
           
           MOVE 1 TO WS-INDX
           MOVE 0 TO WS-CNT-UP
           MOVE 'Y' TO WS-FLAG

           PERFORM UNTIL WS-INDX > 11

               MOVE UDI(WS-INDX)   TO WS-B-UD(WS-INDX)
               MOVE REQI(WS-INDX)  TO WS-B-REQ(WS-INDX)
               MOVE ADMNI(WS-INDX) TO WS-B-ADMN(WS-INDX)
               MOVE APPI(WS-INDX)  TO WS-B-APP(WS-INDX)
               MOVE SPI(WS-INDX)   TO WS-B-SP(WS-INDX)

               
                   ADD 1 TO WS-CNT-UP

                   EVALUATE UDI(WS-INDX)
                   WHEN 'U' 
                       PERFORM 4200-CHECK-ROLE
                   WHEN 'D'
                       CONTINUE
                   WHEN '_'
                       PERFORM 4200-CHECK-ROLE
                   WHEN OTHER
                       MOVE 'Q' TO WS-FLAG
                       PERFORM 4200-CHECK-ROLE
                       MOVE  DFHUNIMD TO UDA(WS-INDX)
                   END-EVALUATE


               ADD 1 TO WS-INDX
           END-PERFORM.

       4000-EXIT. EXIT.

       4100-DISPLAY-BACKUP.
           MOVE 1 TO WS-INDX
           PERFORM UNTIL  WS-INDX > 11
               MOVE WS-B-UD(WS-INDX)   TO UDI(WS-INDX)   
               MOVE WS-B-REQ(WS-INDX)  TO REQI(WS-INDX) 
               MOVE WS-B-ADMN(WS-INDX) TO ADMNI(WS-INDX) 
               MOVE WS-B-APP(WS-INDX)  TO APPI(WS-INDX)  
               MOVE WS-B-SP(WS-INDX)   TO SPI(WS-INDX)  

               ADD 1 TO WS-INDX
           END-PERFORM.
       4100-EXIT. EXIT.

       4200-CHECK-ROLE.
      *************************************************
           IF REQI(WS-INDX) = 'Y' OR 'N'
               CONTINUE
           ELSE
               MOVE  DFHUNIMD TO REQA(WS-INDX)
               MOVE 'Q' TO WS-FLAG
           END-IF

           IF ADMNI(WS-INDX)  = 'Y' OR 'N'
               CONTINUE
           ELSE
               MOVE  DFHUNIMD TO ADMNA(WS-INDX)
               MOVE 'Q' TO WS-FLAG
           END-IF

           IF APPI(WS-INDX) = 'Y' OR 'N'
               CONTINUE
           ELSE
               MOVE  DFHUNIMD TO APPA(WS-INDX) 
               MOVE 'Q' TO WS-FLAG
           END-IF

           IF SPI(WS-INDX) = 'Y' OR 'N'
               CONTINUE
           ELSE
               MOVE  DFHUNIMD TO SPA(WS-INDX) 
               MOVE 'Q' TO WS-FLAG
           END-IF.

           

       4200-EXIT. EXIT.


       
       7000-FIND-SEARCH.

      *     USERIDI- user input
           MOVE 'N' TO  FLAG-SEARCH
           MOVE WS-QITEM-START TO WS-QITEM
           MOVE USERIDI TO WS-TEST
           PERFORM UNTIL WS-QITEM > WS-QITEM-END OR FLAG-SEARCH = 'Y'

               EXEC CICS READQ TS
                   QUEUE(WS-QNAME)
                   INTO (WS-REC)
                   LENGTH(WS-RECB-LENGTH)
                   ITEM (WS-QITEM)
               END-EXEC 

               IF WS-TEST = WS-UID
                   MOVE 'Y' TO FLAG-SEARCH
               END-IF

               ADD 1 TO WS-QITEM

           END-PERFORM

               IF FLAG-SEARCH = 'Y'

                   MOVE 1 TO WS-INDX
               
                   MOVE '_'    TO UDI(WS-INDX)
                   MOVE WS-UID TO UIDI(WS-INDX)
                   MOVE WS-REQ TO REQI(WS-INDX)
                   MOVE WS-ADMN TO ADMNI(WS-INDX)
                   MOVE WS-APP TO APPI(WS-INDX)
                   MOVE WS-SP TO SPI(WS-INDX)

                   MOVE UDI(WS-INDX)   TO WS-B-UD(WS-INDX)
                   MOVE REQI(WS-INDX)  TO WS-B-REQ(WS-INDX)
                   MOVE ADMNI(WS-INDX) TO WS-B-ADMN(WS-INDX)
                   MOVE APPI(WS-INDX)  TO WS-B-APP(WS-INDX)
                   MOVE SPI(WS-INDX)   TO WS-B-SP(WS-INDX)
                   

                   ADD 1 TO WS-INDX

                   PERFORM UNTIL WS-INDX > 11

                       MOVE SPACE TO UDI(WS-INDX)
                       MOVE SPACE TO UIDI(WS-INDX)
                       MOVE SPACE TO REQI(WS-INDX)
                       MOVE SPACE TO ADMNI(WS-INDX)
                       MOVE SPACE TO APPI(WS-INDX)
                       MOVE SPACE TO SPI(WS-INDX) 

                       MOVE DFHBMPRO TO UDA(WS-INDX)
                       MOVE DFHBMPRO TO UDA(WS-INDX)
                       MOVE DFHBMPRO TO UIDA(WS-INDX)
                       MOVE DFHBMPRO TO REQA(WS-INDX)
                       MOVE DFHBMPRO TO ADMNA(WS-INDX)
                       MOVE DFHBMPRO TO APPA(WS-INDX)
                       MOVE DFHBMPRO TO SPA(WS-INDX)

                       ADD 1 TO WS-INDX
                   END-PERFORM
               END-IF.

       7000-EXIT. EXIT.
