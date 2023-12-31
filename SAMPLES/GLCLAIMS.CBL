 IDENTIFICATION DIVISION.
 PROGRAM-ID. GLCLAIMS.
*
*
* GLCLAIM - Notes:
*
* Setup Notes:
* ************

* 1) Press PF10 to exit the program.
*
* 2) Data Files Needed:  GLCLAIMS.DAT in the same directory as
*    the finished executable
*
* 3) Use the following client account numbers to run the program:
*    11111, 22222, 33333, 44444, 55555
*
* Problem 1:
*
*  Misplaced Go To is causing 4th Quarter data to not be displayed.
*
*  How to use the Workbench to show this:
*
*   o  Flow Diagram can be used.  It will be obvious that
*      A5640-DISPLAY-FOURTH-QUARTER is not showing up.
*
*   o  Navigation can be used to show this problem to.
*
*           IF W-REPORT-PARM <  12
*              Go To A2000-DISPLAY-TOTALS.
*              Go To A2000-DISPLAY-TOTALS.   <<-----------*****
*
*          PERFORM A5640-DISPLAY-FOURTH-QUARTER
*             VARYING WN-MONTH FROM 10 BY 1
*                   UNTIL WN-MONTH > 12.
*
*  Solution: Move the second Go To line below
*            the code "UNTIL WN-MONTH > 12."
*
*
* Problem 2:
*      The working storage group (WN-TOTAL-REC) was
*      accidentally given a 10 level instead of
*      an 05 level. Because of this, when the group
*      level above it (05 WRK-CLAIMS-REC) is read into,
*      the values for WN-TOTAL-AMOUNT-DUE and PAID
*      are being overlaid.  Thus the totals will be wrong.
*
*   Where:
*         10 WRK-CLAIM-MONTH PIC 9(02).
*         10 FILLER          PIC X(22).
*     10  WN-TOTAL-REC.
*                15 WN-TOTAL-AMOUNT-DUE   PIC 9(09) COMP.
*                15 WN-TOTAL-AMOUNT-PAID  PIC 9(09) COMP.
*     05  WN-SUBSCRIPT       PIC s9(04) comp.
*
*  How to use the Workbench to show this:
*
*   o  Using Show Implied Data Reference will identify where
*          WN-TOTAL-AMOUNT-DUE is being overlaid by a
*          group move
*
*         A4210-LOOP.
*         ...
*         ...
*             MOVE CHF-CLAIMS-REC
*                           TO WRK-CLAIMS-REC    <<-------*******
*
*   Solution: Change WN-TOTAL-REC to 05 group level, and
*             change WN-TOTAL-AMOUNT-DUE and PAID to 10 levels.
*
*
* Problem 3:  Poor indentation and misplaced period
*             causes an unconditional Go To.
*   Where:
*
*        A5500-RECEIVE-INQ-SCREEN  Section.
*        A5500-START.
*         ...
*        ...
*         ...
*     IF  FIELD-TERMINATOR is equal K-F10
*                        MOVE 'Y' TO WS-INQUIRY-DONE
*                        GO TO A5500-EXIT.
*     IF  FIELD-TERMINATOR is equal K-F1
*                MOVE 3  TO CUR-ROW.         <<<<--------****
*                        GO TO A5500-EXIT.           <<------- Becomes
*                                                                 uncondit
*
* How to use the Workbench to show this:
*        o  Flow Diagram will show an unconditional Branch
*        o  Using Show Unreferenced Prococdure Div will identify this!
*
*
* Problem 4:
*    Developer Left in working storage elements
*    that are no longer used.
*

 AUTHOR.     COMPUTER ASSOCIATES INTERNATIONAL, INC.
 SECURITY.       COPYRIGHT 1995
                 COMPUTER ASSOCIATES INTERNATIONAL INC.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
 SOURCE-COMPUTER.   IBM-3090.
 OBJECT-COMPUTER.   IBM-3090.
    SPECIAL-NAMES.
       CONSOLE   is CONSOLE
       SYSIN     is Sysin
            ALPHABET  CHAR-ASCII   is STANDARD-1
                      CHAR-EBCDIC  is EBCDIC.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
      SELECT CLAIMS-HISTORY-FILE
         ASSIGN TO 'GLCLAIMS.DAT[T]'
           FILE STATUS is WS-CLAIMS-FILE-STATUS.
 DATA DIVISION.
 FILE SECTION.
 FD CLAIMS-HISTORY-FILE
       LABEL RECORDS ARE STANDARD.
 01 CHF-CLAIMS-REC.
    05 CHF-ACC-NUM     PIC X(05).
         05 CHF-FIRST-NAME  PIC X(15).
         05 CHF-LAST-NAME   PIC X(15).
         05 CHF-MI          PIC X(01).

         05 CHF-AMOUNT-DUE  PIC 9(07).
         05 CHF-AMOUNT-PAID PIC 9(07).
         05 CHF-CLAIM-MONTH PIC 9(02).
         05 FILLER          PIC X(22).

 WORKING-STORAGE SECTION.
*
* Constants!
*
 01  K-CLEAR-SCREEN              PIC S9(04) COMP VALUE +1.
 01  K-SET-ATTRIBUTE             PIC S9(04) COMP VALUE +2.
 01  K-ATTRIBUTE-WHITE-ON-BLACK  PIC S9(04) COMP VALUE ZERO.
 01  K-ATTRIBUTE-HIGH-INTENSITY  PIC S9(04) COMP VALUE +1.
 01  K-ATTRIBUTE-UNDERSCORE      PIC S9(04) COMP VALUE +4.
 01  K-ATTRIBUTE-BLINK           PIC S9(04) COMP VALUE +5.
 01  K-ATTRIBUTE-REVERSE-VIDEO   PIC S9(04) COMP VALUE +7.
 01  K-ATTRIBUTE-INVISIBLE       PIC S9(04) COMP VALUE +8.
 01  K-ATTRIBUTE-BLACK-TEXT      PIC S9(04) COMP VALUE +30.
 01  K-ATTRIBUTE-RED-TEXT        PIC S9(04) COMP VALUE +31.
 01  K-ATTRIBUTE-GREEN-TEXT      PIC S9(04) COMP VALUE +32.
 01  K-ATTRIBUTE-YELLOW-TEXT     PIC S9(04) COMP VALUE +33.
 01  K-ATTRIBUTE-BLUE-TEXT       PIC S9(04) COMP VALUE +34.
 01  K-ATTRIBUTE-MAGENTA-TEXT    PIC S9(04) COMP VALUE +35.
 01  K-ATTRIBUTE-CYAN-TEXT       PIC S9(04) COMP VALUE +36.
 01  K-ATTRIBUTE-WHITE-TEXT      PIC S9(04) COMP VALUE +37.
 01  K-ATTRIBUTE-BLACK-BCKG      PIC S9(04) COMP VALUE +40.
 01  K-ATTRIBUTE-RED-BCKG        PIC S9(04) COMP VALUE +41.
 01  K-ATTRIBUTE-GREEN-BCKG      PIC S9(04) COMP VALUE +42.
 01  K-ATTRIBUTE-YELLOW-BCKG     PIC S9(04) COMP VALUE +43.
 01  K-ATTRIBUTE-BLUE-BCKG       PIC S9(04) COMP VALUE +44.
 01  K-ATTRIBUTE-MAGENTA-BCKG    PIC S9(04) COMP VALUE +45.
 01  K-ATTRIBUTE-CYAN-BCKG       PIC S9(04) COMP VALUE +46.
 01  K-ATTRIBUTE-WHITE-BCKG      PIC S9(04) COMP VALUE +47.
 01  K-F1  PIC S9(04) COMP VALUE 59.
 01  K-F10 PIC S9(04) COMP VALUE 68.
 01  W-MULT-VAL  PIC S9(04) COMP.
 01  W-SUB-VAL  PIC S9(04) COMP.
 01  WS-CLAIMS-FILE-STATUS  PIC X(02).

 01  TEST-FIELDS.
     05  TEST-CLAIMS-REC.
         10 TEST-ACC-NUM     PIC X(05).
              10 TEST-FIRST-NAME  PIC X(15).
              10 TEST-LAST-NAME   PIC X(15).
              10 TEST-MI          PIC X(01).
              10 TEST-AMOUNT-DUE  PIC 9(07).
              10 TEST-AMOUNT-PAID PIC 9(07).
                  10 TEST-CLAIM-MONTH PIC 9(02).
              10 FILLER          PIC X(22).
 01  CUR-BLOCK.
     05  CUR-ROW           PIC 9(4) COMP-5.
     05  CUR-COL           PIC 9(4) COMP-5.

 01  FILLER.
     05  TIME-AREA.
          10  HH                  PIC 99.
          10  MM                  PIC 99.
          10  SS                  PIC 99.
          10  HUN                 PIC 99.
     05  TIME-SYNC                PIC X(8).
     05  MILLI-SECONDS            PIC S9(8) COMP.
     05  BGN-MILLI-SECONDS        PIC S9(8) COMP.
     05  DISPLAY-MILLI-SECONDS    PIC Z(8).
 01  WS-WORK-AREAS.
     05  WS-REC-FOUND  PIC X(01).
     05  PARM-REC.
         10  PARM-NUM                 PIC S9(04) COMP.
              10  PARM-ATTRIBUTE           PIC S9(04) COMP.
              10  WS-INQUIRY-DONE          PIC X(01) VALUE SPACES.
          05  WC-CLIENT-ACC-NUM            PIC X(05).
          05  WC-CLIENT-FIRST-NAME         PIC X(15).
          05  WC-CLIENT-LAST-NAME          PIC X(15).
          05  WC-CLIENT-MI                 PIC X(01).
          05  WN-MONTH                     PIC s9(04) COMP.
     05  WRK-CLAIMS-REC.
         10 WRK-ACC-NUM     PIC X(05).
              10 WRK-FIRST-NAME  PIC X(15).
              10 WRK-LAST-NAME   PIC X(15).
              10 WRK-MI          PIC X(01).
              10 WRK-AMOUNT-DUE  PIC 9(07).
              10 WRK-AMOUNT-PAID PIC 9(07).

                 10 WRK-CLAIM-MONTH PIC 9(02).
              10 FILLER          PIC X(22).
            10  WN-TOTAL-REC.
                  15 WN-TOTAL-AMOUNT-DUE   PIC 9(09) COMP.
                  15 WN-TOTAL-AMOUNT-PAID  PIC 9(09) COMP.
     05  WN-SUBSCRIPT       PIC s9(04) comp.
          05  WRK-OUT-TBL.
              10 WRK-OUT-TBL-O OCCURS 12 TIMES.
                     15  WRK-OUT-AMOUNT-DUE-X.
                     20  WRK-OUT-AMOUNT-DUE  PIC $ZZ,ZZZ,ZZ9.
                     15  WRK-OUT-AMOUNT-PAID-X.
                     20  WRK-OUT-AMOUNT-PAID PIC $ZZ,ZZZ,ZZ9.
          05  WRK-TOTAL-AMOUNT-DUE  PIC $ZZ,ZZZ,ZZ9.
          05  WRK-TOTAL-AMOUNT-PAID PIC $ZZ,ZZZ,ZZ9.
          05  W-WRK-I  PIC S9(04) COMP.

 01  CHAR-X  PIC X(12).

*    Parameter block for DISPLAY_CONTROL and sample calling
*    sequence
*
 01  DISP-PARMS.
     05  DISP-IO-TYPE-FLAG                 PIC X.
         88  DISP-IO-TYPE-NO-CHG           VALUE LOW-VALUE.
         88  DISP-IO-TYPE-DOS              VALUE 'D'.
         88  DISP-IO-TYPE-BIOS             VALUE 'B'.
         88  DISP-IO-TYPE-MAPPED           VALUE 'M'.
     05  DISP-AUTOWRAP-FLAG                PIC X.
         88  DISP-AUTOWRAP-NO-CHG          VALUE LOW-VALUE.
         88  DISP-SET-AUTOWRAP             VALUE 'Y'.
         88  DISP-SET-NOAUTOWRAP           VALUE 'N'.
     05  DISP-VERT-RETRACE-FLAG            PIC X.
         88  DISP-VERT-RETRACE-NO-CHG      VALUE LOW-VALUE.
         88  DISP-SET-WAIT-VERT-RETRACE    VALUE 'Y'.
         88  DISP-SET-NOWAIT-VERT-RETRACE  VALUE 'N'.
     05  DISP-CR-SCROLL-FLAG               PIC X.
         88  DISP-CR-SCROLL-NO-CHG         VALUE LOW-VALUE.
         88  DISP-SET-CR-SCROLL            VALUE 'Y'.
         88  DISP-SET-NO-CR-SCROLL         VALUE 'N'.
     05  DISP-LAST-BYTE-SCROLL-FLAG        PIC X.
         88  DISP-LAST-BYTE-SCROLL-NO-CHG  VALUE LOW-VALUE.
         88  DISP-SET-LAST-BYTE-SCROLL     VALUE 'Y'.
         88  DISP-SET-NO-LAST-BYTE-SCROLL  VALUE 'N'.
     05  DISP-BORDER-COLOR                 PIC 9(4) COMP-5.
         88  DISP-BORDER-COLOR-NO-CHG      VALUE  0.
         88  DISP-BORDER-COLOR-VALID       VALUE  1 THRU 16.
         88  DISP-BORDER-COLOR-BLACK       VALUE  1.
         88  DISP-BORDER-COLOR-BLUE        VALUE  2.
         88  DISP-BORDER-COLOR-GREEN       VALUE  3.
         88  DISP-BORDER-COLOR-CYAN        VALUE  4.
         88  DISP-BORDER-COLOR-RED         VALUE  5.
         88  DISP-BORDER-COLOR-MAGENTA     VALUE  6.
         88  DISP-BORDER-COLOR-BROWN       VALUE  7.
         88  DISP-BORDER-COLOR-WHITE       VALUE  8.
         88  DISP-BORDER-COLOR-GRAY        VALUE  9.
         88  DISP-BORDER-COLOR-HI-BLUE     VALUE 10.
         88  DISP-BORDER-COLOR-HI-GREEN    VALUE 11.
         88  DISP-BORDER-COLOR-HI-CYAN     VALUE 12.
         88  DISP-BORDER-COLOR-HI-RED      VALUE 13.
         88  DISP-BORDER-COLOR-HI-MAGENTA  VALUE 14.
         88  DISP-BORDER-COLOR-YELLOW      VALUE 15.
         88  DISP-BORDER-COLOR-HI-WHITE    VALUE 16.
     05  DISP-SCREEN-DEPTH                 PIC S9(4) COMP-5.
         88  DISP-SCREEN-DEPTH-NO-CHG      VALUE 0.
         88  DISP-SCREEN-USE-PHYS-DEPTH    VALUE -1.
     05  FILLER                            PIC X(7).

 01  W-REPORT-PARM PIC s9(04) COMP-4 VALUE +12.

 PROCEDURE DIVISION.
*
     PERFORM A1000-PGM-INITS.
          PERFORM A2000-MANAGE-INQ-SCREEN
             UNTIL WS-INQUIRY-DONE = 'Y'.
          PERFORM A9000-PGM-TERMINATION.

     STOP RUN.

*
 A1000-PGM-INITS   SECTION.
 A1000-START.
          MMOVE 'N' TO WS-INQUIRY-DONE.
          PERFORM A5100-BLD-INQ-SCREEN.
 A1000-EXIT.
     Exit.
 A1100-OPEN-FILE   SECTION.
 A1100-START.

 A1100-EXIT.
       EXIT.

 A2000-MANAGE-INQ-SCREEN  SECTION.
 A2000-START.

           PPERFORM A5500-RECEIVE-INQ-SCREEN.

           PERFORM A4200-READ-INQ-DATA-BASE.

            PERFORM A5610-DISPLAY-FIRST-QUARTER
                   VARYING WN-MONTH FROM 1 BY 1
                      UNTIL WN-MONTH > 3.

             IF   W-REPORT-PARM < 6
                   Go To A2000-DISPLAY-TOTALS.


           PERFORM A5620-DISPLAY-SECOND-QUARTER
                         VARYING WN-MONTH FROM 4 BY 1
                           UNTIL WN-MONTH > 6.

             IF   W-REPORT-PARM <  9
                   Go To A2000-DISPLAY-TOTALS.

           PERFORM A5630-DISPLAY-THIRD-QUARTER
                        VARYING WN-MONTH FROM 7 BY 1
                         UNTIL WN-MONTH > 9.

             IF   W-REPORT-PARM <  12
                   Go To A2000-DISPLAY-TOTALS.
                   Go To A2000-DISPLAY-TOTALS.

           PERFORM A5640-DISPLAY-FOURTH-QUARTER
              VARYING WN-MONTH FROM 10 BY 1
                    UNTIL WN-MONTH > 12.

 A2000-DISPLAY-TOTALS.

      PERFORM A5700-DISPLAY-TOTALS.

 A2000-EXIT.
            Exit.
*
* IO Related Sections
*
 A4100-OPEN-FILE  SECTION.
 A4100-START.

     OPEN INPUT CLAIMS-HISTORY-FILE.
     IF   (WS-CLAIMS-FILE-STATUS NOT EQUAL "00")
     THEN
          DISPLAY "Could not find GLCLAIMS.DAT data file."
          DISPLAY "This file must reside in same directory that"
          DISPLAY "GLCLAIMS.EXE does."
          STOP RUN.

 A4100-EXIT.
       EXIT.

 A4200-READ-INQ-DATA-BASE  SECTION.
 A4200-START.

          PERFORM A4100-OPEN-FILE.
          MOVE 'N'  TO WS-REC-FOUND.
          MOVE ZERO TO WN-TOTAL-AMOUNT-DUE.
          MOVE ZERO TO WN-TOTAL-AMOUNT-PAID.
          MOVE ZERO TO W-WRK-I.
 A4210-LOOP.

     READ CLAIMS-HISTORY-FILE
             AT END GO TO A4220-EOF.

          IF ( (CHF-ACC-NUM = WC-CLIENT-ACC-NUM)
          OR   ( (CHF-FIRST-NAME = WC-CLIENT-FIRST-NAME)
          AND    (CHF-LAST-NAME  = WC-CLIENT-LAST-NAME) ) )
          THEN
               MOVE CHF-CLAIMS-REC
               TO WRK-CLAIMS-REC
               MOVE 'Y' TO WS-REC-FOUND
                   COMPUTE WN-SUBSCRIPT = WRK-CLAIM-MONTH
                   MOVE WRK-AMOUNT-DUE  TO

                       WRK-OUT-AMOUNT-DUE(WN-SUBSCRIPT)
                   MOVE WRK-AMOUNT-PAID  TO
                       WRK-OUT-AMOUNT-PAID(WN-SUBSCRIPT)
                   ADD WRK-AMOUNT-PAID TO WN-TOTAL-AMOUNT-PAID
                   ADD WRK-AMOUNT-DUE  TO WN-TOTAL-AMOUNT-DUE.

          GO TO A4210-LOOP.

 A4220-EOF.

          PERFORM A4900-CLOSE-FILE.

     IF  (WS-REC-FOUND = 'N')
         MOVE 4  TO CUR-ROW
         MOVE 30 TO CUR-COL
         CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK
              MOVE K-ATTRIBUTE-RED-TEXT TO PARM-ATTRIBUTE
         CALL 'CASCREEN' USING PARM-REC
         DISPLAY '<Record Not Found!>'
         MOVE ZERO TO WN-TOTAL-AMOUNT-PAID
         MOVE ZERO TO WN-TOTAL-AMOUNT-DUE
          ELSE
         MOVE 4  TO CUR-ROW
         MOVE 30 TO CUR-COL
         CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK
              MOVE K-ATTRIBUTE-RED-TEXT TO PARM-ATTRIBUTE
         CALL 'CASCREEN' USING PARM-REC
         DISPLAY '                   '
                  PERFORM A5800-FORMAT-NAME-INFO.

 A4200-EXIT.
          EXIT.
 A4900-CLOSE-FILE  SECTION.
 A4900-START.

     CLOSE CLAIMS-HISTORY-FILE.

 A4900-EXIT.
     EXIT.
*
* Screen Input/Output
*
 A5200-CLEAR-INQ-SCREEN  Section.
 A5200-START.

 A5200-EXIT.
       EXIT.
 A5500-RECEIVE-INQ-SCREEN  Section.
 A5500-START.

          MOVE K-ATTRIBUTE-WHITE-TEXT TO PARM-ATTRIBUTE.
          MOVE K-ATTRIBUTE-MAGENTA-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.

     MOVE 3  TO CUR-ROW.
     MOVE 34 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     ACCEPT WC-CLIENT-ACC-NUM.
     IF  FIELD-TERMINATOR is equal K-F10
                         MOVE 'Y' TO WS-INQUIRY-DONE
                         GO TO A5500-EXIT.
     IF  FIELD-TERMINATOR is equal K-F1
                 MOVE 3  TO CUR-ROW.
                         GO TO A5500-EXIT.


 A5510-GET-FIRST-NAME.
     MOVE 5  TO CUR-ROW.
     MOVE 28 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     ACCEPT WC-CLIENT-FIRST-NAME.
     IF  FIELD-TERMINATOR is equal K-F10
                         MOVE 'Y' TO WS-INQUIRY-DONE
                         GO TO A5500-EXIT.
     IF  FIELD-TERMINATOR is equal K-F1
                         GO TO A5500-EXIT.

 A5520-GET-LAST-NAME.
     MOVE 6  TO CUR-ROW.
     MOVE 28 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     ACCEPT WC-CLIENT-LAST-NAME.
     IF  FIELD-TERMINATOR is equal K-F10
                         MOVE 'Y' TO WS-INQUIRY-DONE
                         GO TO A5500-EXIT.
     IF  FIELD-TERMINATOR is equal K-F1
                         GO TO A5500-EXIT.

 A5530-GET-MI.
     MOVE 7  TO CUR-ROW.
     MOVE 28 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     ACCEPT WC-CLIENT-MI.
     IF  FIELD-TERMINATOR is equal K-F10
                         MOVE 'Y' TO WS-INQUIRY-DONE
                         GO TO A5500-EXIT.
     IF  FIELD-TERMINATOR is equal K-F1
                         GO TO A5500-EXIT.

 A5500-EXIT.
       EXIT.
 A5610-DISPLAY-FIRST-QUARTER Section.
 A5610-START.

     COMPUTE CUR-ROW = WN-MONTH + 10.
     MOVE 20 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
          MOVE K-ATTRIBUTE-RED-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     DISPLAY WRK-OUT-AMOUNT-DUE-X(WN-MONTH).
     MOVE 40 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY WRK-OUT-AMOUNT-PAID-X(WN-MONTH).

  A5610-EXIT.
     EXIT.
 A5620-DISPLAY-SECOND-QUARTER Section.
 A5620-START.

     COMPUTE CUR-ROW = WN-MONTH + 10.
     MOVE 20 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
          MOVE K-ATTRIBUTE-RED-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     DISPLAY WRK-OUT-AMOUNT-DUE-X(WN-MONTH).
     MOVE 40 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY WRK-OUT-AMOUNT-PAID-X(WN-MONTH).

 A5620-EXIT.
     EXIT.
 A5630-DISPLAY-THIRD-QUARTER Section.
 A5630-START.

     COMPUTE CUR-ROW = WN-MONTH + 10.
     MOVE 20 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
          MOVE K-ATTRIBUTE-RED-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     DISPLAY WRK-OUT-AMOUNT-DUE-X(WN-MONTH).
     MOVE 40 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY WRK-OUT-AMOUNT-PAID-X(WN-MONTH).

 A5630-EXIT.
     EXIT.
 A5640-DISPLAY-FOURTH-QUARTER Section.
 A5640-START.

     COMPUTE CUR-ROW = WN-MONTH + 10.
     MOVE 20 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
          MOVE K-ATTRIBUTE-RED-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     DISPLAY WRK-OUT-AMOUNT-DUE-X(WN-MONTH).
     MOVE 40 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY WRK-OUT-AMOUNT-PAID-X(WN-MONTH).

 A5640-EXIT.
     EXIT.

 A5700-DISPLAY-TOTALS   Section.
 A5700-START.

     COMPUTE CUR-ROW = 23.
     MOVE 20 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
          MOVE K-ATTRIBUTE-RED-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.


          MOVE WN-TOTAL-AMOUNT-DUE TO WRK-TOTAL-AMOUNT-DUE.
     DISPLAY WRK-TOTAL-AMOUNT-DUE.


     MOVE 40 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.

          MOVE WN-TOTAL-AMOUNT-PAID TO WRK-TOTAL-AMOUNT-PAID.
     DISPLAY WRK-TOTAL-AMOUNT-PAID.




 A5700-EXIT.
     EXIT.
 A5800-FORMAT-NAME-INFO   Section.
 A5800-START.

          MOVE K-ATTRIBUTE-MAGENTA-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     MOVE 3  TO CUR-ROW.
     MOVE 34 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY WRK-ACC-NUM.

     MOVE 5  TO CUR-ROW.
     MOVE 28 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY WRK-FIRST-NAME.

     MOVE 6  TO CUR-ROW.
     MOVE 28 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY WRK-LAST-NAME.

     MOVE 7  TO CUR-ROW.
     MOVE 28 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY WRK-MI.

          MOVE K-ATTRIBUTE-MAGENTA-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     MOVE 3  TO CUR-ROW.
     MOVE 34 TO CUR-COL.

 A5800-EXIT.
     EXIT.
 A5100-BLD-INQ-SCREEN     Section.
 A5100-Start.
     MOVE K-CLEAR-SCREEN TO PARM-NUM.
     CALL 'CASCREEN' USING PARM-REC.
     MOVE K-SET-ATTRIBUTE TO PARM-NUM.
          MOVE K-ATTRIBUTE-RED-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     SET DISP-IO-TYPE-BIOS To True.
     SET DISP-SET-NOAUTOWRAP To True.
     SET DISP-SET-NOWAIT-VERT-RETRACE to True.
     SET DISP-SET-NO-CR-SCROLL To True.
     SET DISP-BORDER-COLOR-RED To True.
     SET DISP-SCREEN-USE-PHYS-DEPTH To True.

     CALL 'DISPLAY_CONTROL' USING DISP-PARMS.

     MOVE 1  TO CUR-ROW.
     MOVE 30 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
          MOVE K-ATTRIBUTE-RED-TEXT TO PARM-ATTRIBUTE.
          MOVE K-ATTRIBUTE-GREEN-TEXT TO PARM-ATTRIBUTE.
          MOVE K-ATTRIBUTE-YELLOW-TEXT TO PARM-ATTRIBUTE.

     CALL 'CASCREEN' USING PARM-REC.
     DISPLAY ' Claims Inquiry Screen '.

          MOVE K-ATTRIBUTE-GREEN-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     MOVE 3  TO CUR-ROW.
     MOVE 10 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY ' Client Account Number: '.

     MOVE 5  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     Display ' First Name.....:'.
     MOVE 6  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     Display ' Last Name......:'.
     MOVE 7  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     Display ' Middle Initial.:'.

     MOVE 8  TO CUR-ROW.
     MOVE 30 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
          MOVE K-ATTRIBUTE-YELLOW-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.

     DISPLAY ' Monthly Payment Summary '.

     MOVE 9  TO CUR-ROW.
     MOVE 9 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY ' Month      Amount Due         Amount Paid '.

     MOVE 11  TO CUR-ROW.
     MOVE 10 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
          MOVE K-ATTRIBUTE-GREEN-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     DISPLAY 'Jan'.

     MOVE 12  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Feb'.

     MOVE 13  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Mar'.

     MOVE 14  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Apr'.

     MOVE 15  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'May'.

     MOVE 16  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Jun'.

     MOVE 17  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Jul'.

     MOVE 18  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Aug'.

     MOVE 19  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Sep'.

     MOVE 20  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Oct'.

     MOVE 21  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Nov'.

     MOVE 22  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Dec'.

          MOVE K-ATTRIBUTE-RED-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     MOVE 23  TO CUR-ROW.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY 'Totals'.

          MOVE K-ATTRIBUTE-YELLOW-TEXT TO PARM-ATTRIBUTE.
     CALL 'CASCREEN' USING PARM-REC.
     MOVE 24  TO CUR-ROW.
     MOVE 19 TO CUR-COL.
     CALL 'DISPLAY_SET_CURSOR' USING CUR-BLOCK.
     DISPLAY '    PF1=Inquire          PF10=Exit'.


 A5100-EXIT.
       EXIT.
*
* CleanUp
*
 A9000-PGM-TERMINATION SECTION.
     MOVE K-CLEAR-SCREEN  TO PARM-NUM.
     CALL 'CASCREEN' USING PARM-REC.

 A9000-EXIT.

     Exit.
*
 END-PROGRAM-PREVENT-FALLTHRU.
* Last Line Before Editing Additions.
