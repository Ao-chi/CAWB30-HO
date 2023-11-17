      * SYMBOLIC MAP of UA002S Created by CACXBMS Version 6.0.27
       01  UA002MI.
	   02  FILLER				      PIC X(12).
	   02  USRIDL				      PIC S9(4) COMP.
	   02  USRIDF				      PIC X.
	   02  FILLER REDEFINES USRIDF.
	       03  USRIDA			      PIC X.
	   02  USRIDI				      PIC X(008).
	   02  REQTAGL				      PIC S9(4) COMP.
	   02  REQTAGF				      PIC X.
	   02  FILLER REDEFINES REQTAGF.
	       03  REQTAGA			      PIC X.
	   02  REQTAGI				      PIC X(001).
	   02  ADMTAGL				      PIC S9(4) COMP.
	   02  ADMTAGF				      PIC X.
	   02  FILLER REDEFINES ADMTAGF.
	       03  ADMTAGA			      PIC X.
	   02  ADMTAGI				      PIC X(001).
	   02  APRTAGL				      PIC S9(4) COMP.
	   02  APRTAGF				      PIC X.
	   02  FILLER REDEFINES APRTAGF.
	       03  APRTAGA			      PIC X.
	   02  APRTAGI				      PIC X(001).
	   02  SRVTAGL				      PIC S9(4) COMP.
	   02  SRVTAGF				      PIC X.
	   02  FILLER REDEFINES SRVTAGF.
	       03  SRVTAGA			      PIC X.
	   02  SRVTAGI				      PIC X(001).
	   02  ERRMSG8L 			      PIC S9(4) COMP.
	   02  ERRMSG8F 			      PIC X.
	   02  FILLER REDEFINES ERRMSG8F.
	       03  ERRMSG8A			      PIC X.
	   02  ERRMSG8I 			      PIC X(071).
	   02  DATEL				      PIC S9(4) COMP.
	   02  DATEF				      PIC X.
	   02  FILLER REDEFINES DATEF.
	       03  DATEA			      PIC X.
	   02  DATEI				      PIC X(010).
	   02  TIMEL				      PIC S9(4) COMP.
	   02  TIMEF				      PIC X.
	   02  FILLER REDEFINES TIMEF.
	       03  TIMEA			      PIC X.
	   02  TIMEI				      PIC X(008).
       01  UA002MO REDEFINES UA002MI.
	   02  FILLER				      PIC X(12).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  USRIDO				      PIC X(008).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  REQTAGO				      PIC X(001).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  ADMTAGO				      PIC X(001).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  APRTAGO				      PIC X(001).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  SRVTAGO				      PIC X(001).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  ERRMSG8O 			      PIC X(071).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  DATEO				      PIC X(010).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  TIMEO				      PIC X(008).
