*$PP-DEF HEADER NI EXTERNAL PROGRAM
*$PP-NOLIST HEADER NI
*$PP-RELATIVE-LINE-ADJ +000000003
 IDENTIFICATION DIVISION.
 PROGRAM-ID.  DB2CALL EXTERNAL.
 DATA DIVISION.
 LINKAGE SECTION.
     copy sqlca.
     copy sqlda.
 01  SQLPIB			 PIC X(40).
 01  SQLNULL			 PIC s9(9) comp-5.
 01  SQLSID			 PIC S9(4) COMP-5.
 01  SQLCMD			 PIC S9(4) COMP-5.
*01  SQLUSE			 PIC S9(4) COMP-5.
 01  SQLDAIN			 PIC S9(4) COMP-5.
 01  SQLDAOUT			 PIC S9(4) COMP-5.
 01  SQLDA-ID			 PIC S9(4) COMP-5.
 01  SQLVARN			 PIC S9(4) COMP-5.
 01  SQLVARI			 PIC S9(4) COMP-5.
 01  SQLVARLEN			 PIC S9(4) COMP-5.
 01  SQLVARLENL 		 PIC S9(9) COMP-5.
 01  SQLVARTYP			 PIC S9(4) COMP-5.
*01  SQLDBNAME			 PIC X(10).
 01  SQL-STRING 		 PIC X.
 01  SQL-VAR-NAME		 PIC X.
 01  SQL-IND-NAME		 PIC S9(4) COMP-5.
 01  SQL-RC			 PIC S9(9) COMP-5.
 01  SQLRTI.
     05  SQL-ID 		 PIC X(8).
     05  SQL-WC-SIZE		 PIC 9(4) COMP-5.
     05  SQL-WC-TYPE		 PIC 9(4) COMP-5.
     05  FILLER 		 PIC X(4).
 PROCEDURE DIVISION.
 ENTRIES-PARAGRAPH.
     ENTRY 'S_sqlgaloc'
	USING BY CONTENT SQLDAIN
	      BY CONTENT SQLVARN
	      BY CONTENT SQLSID
	      BY CONTENT SQLNULL
	GIVING SQL-RC.
     ENTRY 'S_sqlgcall'
	USING BY CONTENT SQLCMD
	      BY CONTENT SQLSID
	      BY CONTENT SQLDAIN
	      BY CONTENT SQLDAOUT
	      BY CONTENT SQLNULL
	GIVING SQL-RC.
     ENTRY 'S_sqlgcmpd'
	USING BY CONTENT SQLDAIN
	      BY CONTENT SQLNULL
	GIVING SQL-RC.
     ENTRY 'S_sqlgdloc'
	USING BY CONTENT SQLCMD
	      BY CONTENT SQLSID
	      BY CONTENT SQLDAIN
	      BY CONTENT SQLDAOUT
	      BY CONTENT SQLNULL
	GIVING SQL-RC.
* NOTE: sqlgsets has been obsoleted in favor of sqlgstls.
*	sqlgsets will be unavailable in future releases.
     ENTRY 'S_sqlgsets'
	USING BY CONTENT SQLVARLEN
	      BY REFERENCE SQL-STRING
	      BY CONTENT SQLNULL
	GIVING SQL-RC.
     ENTRY 'S_sqlgstls'
	USING BY CONTENT SQLVARLENL
	      BY REFERENCE SQL-STRING
	      BY CONTENT SQLNULL
	GIVING SQL-RC.
* NOTE: sqlgsetv has been obsoleted in favor of sqlgstlv.
*	sqlgsetv will be unavailable in future releases.
     ENTRY 'S_sqlgsetv'
	USING BY CONTENT SQLDAIN
	      BY CONTENT SQLVARI
	      BY CONTENT SQLVARTYP
	      BY CONTENT SQLVARLEN
	      BY REFERENCE SQL-VAR-NAME DELIMITED
	      BY REFERENCE SQL-IND-NAME
	      BY CONTENT SQLNULL
	GIVING SQL-RC.
     ENTRY 'S_sqlgstlv'
	USING BY CONTENT SQLDAIN
	      BY CONTENT SQLVARI
	      BY CONTENT SQLVARTYP
	      BY CONTENT SQLVARLENL
	      BY REFERENCE SQL-VAR-NAME DELIMITED
	      BY REFERENCE SQL-IND-NAME
	      BY CONTENT SQLNULL
	GIVING SQL-RC.
     ENTRY 'S_sqlgstop'
	USING BY CONTENT SQLNULL
	GIVING SQL-RC.
     ENTRY 'S_sqlgstrt'
	USING BY REFERENCE SQLPIB
	      BY REFERENCE SQLRTI
	      BY REFERENCE SQLCA
	GIVING SQL-RC.
     ENTRY 'S_sqlgusda'
	USING BY CONTENT SQLDA-ID
	      BY REFERENCE SQLDA
	      BY CONTENT SQLNULL
	GIVING SQL-RC.
*    ENTRY 'S_sqlgstrd'
*	USING BY REFERENCE SQLNULL
*	      BY CONTENT SQLVARN
*	      BY CONTENT SQLUSE
*	      BY REFERENCE SQLCA
*	      BY REFERENCE SQLNULL2
*	      BY REFERENCE SQLDBNAME.
*	GIVING SQL-RC.
*    ENTRY 'S_sqlgstpd'
*	USING BY REFERENCE SQLCA.
*	GIVING SQL-RC.
 END PROGRAM DB2CALL.
*$PP-LIST HEADER
