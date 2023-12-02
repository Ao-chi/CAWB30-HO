      * SYMBOLIC MAP of SM02S Created by CACXBMS Version 6.0.27
       01  SM002MI.
	   02  FILLER				      PIC X(12).
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
	   02  IDL				      PIC S9(4) COMP.
	   02  IDF				      PIC X.
	   02  FILLER REDEFINES IDF.
	       03  IDA				      PIC X.
	   02  IDI				      PIC X(006).
	   02  TICKETTL 			      PIC S9(4) COMP.
	   02  TICKETTF 			      PIC X.
	   02  FILLER REDEFINES TICKETTF.
	       03  TICKETTA			      PIC X.
	   02  TICKETTI 			      PIC X(025).
	   02  TICKTD1L 			      PIC S9(4) COMP.
	   02  TICKTD1F 			      PIC X.
	   02  FILLER REDEFINES TICKTD1F.
	       03  TICKTD1A			      PIC X.
	   02  TICKTD1I 			      PIC X(040).
	   02  TICKTD2L 			      PIC S9(4) COMP.
	   02  TICKTD2F 			      PIC X.
	   02  FILLER REDEFINES TICKTD2F.
	       03  TICKTD2A			      PIC X.
	   02  TICKTD2I 			      PIC X(040).
	   02  STL				      PIC S9(4) COMP.
	   02  STF				      PIC X.
	   02  FILLER REDEFINES STF.
	       03  STA				      PIC X.
	   02  STI				      PIC X(020).
	   02  STATSL				      PIC S9(4) COMP.
	   02  STATSF				      PIC X.
	   02  FILLER REDEFINES STATSF.
	       03  STATSA			      PIC X.
	   02  STATSI				      PIC X(007).
	   02  UPML				      PIC S9(4) COMP.
	   02  UPMF				      PIC X.
	   02  FILLER REDEFINES UPMF.
	       03  UPMA 			      PIC X.
	   02  UPMI				      PIC X(020).
	   02  UPR1L				      PIC S9(4) COMP.
	   02  UPR1F				      PIC X.
	   02  FILLER REDEFINES UPR1F.
	       03  UPR1A			      PIC X.
	   02  UPR1I				      PIC X(025).
	   02  UPR2L				      PIC S9(4) COMP.
	   02  UPR2F				      PIC X.
	   02  FILLER REDEFINES UPR2F.
	       03  UPR2A			      PIC X.
	   02  UPR2I				      PIC X(025).
	   02  UPDTL				      PIC S9(4) COMP.
	   02  UPDTF				      PIC X.
	   02  FILLER REDEFINES UPDTF.
	       03  UPDTA			      PIC X.
	   02  UPDTI				      PIC X(020).
	   02  UPDATEL				      PIC S9(4) COMP.
	   02  UPDATEF				      PIC X.
	   02  FILLER REDEFINES UPDATEF.
	       03  UPDATEA			      PIC X.
	   02  UPDATEI				      PIC X(010).
	   02  UPTIMEL				      PIC S9(4) COMP.
	   02  UPTIMEF				      PIC X.
	   02  FILLER REDEFINES UPTIMEF.
	       03  UPTIMEA			      PIC X.
	   02  UPTIMEI				      PIC X(008).
	   02  UPBYL				      PIC S9(4) COMP.
	   02  UPBYF				      PIC X.
	   02  FILLER REDEFINES UPBYF.
	       03  UPBYA			      PIC X.
	   02  UPBYI				      PIC X(020).
	   02  UPBL				      PIC S9(4) COMP.
	   02  UPBF				      PIC X.
	   02  FILLER REDEFINES UPBF.
	       03  UPBA 			      PIC X.
	   02  UPBI				      PIC X(007).
	   02  ERRMSGL				      PIC S9(4) COMP.
	   02  ERRMSGF				      PIC X.
	   02  FILLER REDEFINES ERRMSGF.
	       03  ERRMSGA			      PIC X.
	   02  ERRMSGI				      PIC X(058).
       01  SM002MO REDEFINES SM002MI.
	   02  FILLER				      PIC X(12).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  DATEO				      PIC X(010).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  TIMEO				      PIC X(008).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  IDO				      PIC X(006).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  TICKETTO 			      PIC X(025).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  TICKTD1O 			      PIC X(040).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  TICKTD2O 			      PIC X(040).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  STO				      PIC X(020).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  STATSO				      PIC X(007).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  UPMO				      PIC X(020).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  UPR1O				      PIC X(025).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  UPR2O				      PIC X(025).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  UPDTO				      PIC X(020).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  UPDATEO				      PIC X(010).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  UPTIMEO				      PIC X(008).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  UPBYO				      PIC X(020).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  UPBO				      PIC X(007).
	   02  FILLER				      PIC XX.
	   02  FILLER				      PIC X.
	   02  ERRMSGO				      PIC X(058).
