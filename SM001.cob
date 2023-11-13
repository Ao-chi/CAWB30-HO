       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM001.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-MAP  VALUE 'SM00S'         PIC X(7).
       01  WS-TIME                               PIC 9(15) COMP-3.
       01  WS-DATE                               PIC 9(7).
       01  WS-DATE-X REDEFINES WS-DATE           PIC X(7).
       01  WS-LENGTH                             PIC S9(4) COMP.
       01  WS-COMMAREA.
           05 WS-PROG-STATE                      PIC X(15).
           05 WS-PGMID                           PIC X(06).
           COPY SM000.
           COPY DFHAID.
           COPY DFHBMSCA.
       01  WS-ERRMSGS.
           05 WS-INVALID-PGMID                   PIC X(20) VALUE
           'INVALID USER ACCESS'.   
       01  WS-PAGE-NO                            PIC 9(02) VALUE 1.

       LINKAGE SECTION.
       01  DFHCOMMAREA                           PIC X(21).

       PROCEDURE DIVISION.
       100-MAIN.
           EXEC CICS IGNORE CONDITION
                     ERROR
           END-EXEC
           MOVE DFHCOMMAREA TO WS-COMMAREA
           
           IF WS-PGMID = 'SM0000' OR 'SM001' OR 'SM002' OR 'SM003' 
              OR 'SM004' OR 'SM005' OR 'SM006'   
              MOVE LENGTH OF SM001MO TO WS-LENGTH
             
              PERFORM 110-DATE-TIME
      *       MOVE WS-PAGE-NO TO 
              EXEC CICS SEND
                   MAP('SM001M')
                   MAPSET('SM000')
                   FROM(SM001MO)
                   LENGTH(WS-LENGTH)
                   CURSOR
                   ERASE
              END-EXEC
              EXEC CICS RETURN
                   TRANSID(EIBTRNID)
                   COMMAREA(WS-COMMAREA)
              END-EXEC
           ELSE 
              MOVE SPACES TO ERRMSGO
              MOVE LENGTH OF WS-COMMAREA TO WS-LENGTH
              EXEC CICS SEND TEXT
                      FROM (WS-INVALID-PGMID)
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
                DATESEP    ('-')
                MMDDYYYY   (DATE1O)
                TIME       (TIME1O)
                TIMESEP    (':')
           END-EXEC
           MOVE DFHBMASB TO TIME1A
           MOVE DFHBMASB TO DATE1A.
       110-EXIT.
           EXIT.