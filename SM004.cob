       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SM004.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-MAP  VALUE 'SM00S'         PIC X(7).
       01  WS-LENGTH                             PIC S9(4) COMP.
       01  WS-COMMAREA.
           05 WS-PROG-STATE                      PIC X(15).

      *
           COPY SM00S.
      *
           COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA                           PIC X(15).


       PROCEDURE DIVISION.
       100-MAIN.
           MOVE 'SM004 CALLED' TO DFHCOMMAREA
           EXEC CICS
            RETURN
           END-EXEC.

       100-EXIT.
           EXIT.
