@echo off
cls
echo CA-Realia CICS build sample application Version 6.0.27
echo Copyright (c) 1993, 1997 Computer Associates International, Inc.
rem
rem BUILDSAM regenerates all of the Sample System Application Files
rem
rem Revision Information
rem ^{File:buildsam.bch}
rem ^{Timestamp:Thu Jun 19 07:39:24 1997}
rem ^{Revision:8.0}
rem
rem test for valid directories
if .%coblink%==. call real2set
if .%cicslink%==. call real2set
if exist %cicslink%\cacx6exe.lib goto ok
echo %cicslink% directory is not valid for CICS
goto error
:ok
set _step=0
set tmp_cobdirec=%cobdirec%
set cobdirec=
set tmp_copydir=%copydir%
set copydir=
set tmp_objdir=%objdir%
set objdir=
set tmp_lstdir=%lstdir%
set lstdir=
set tmp_mapdir=%mapdir%
set mapdir=
set tmp_dlldir=%dlldir%
set dlldir=
rem
rem Merge Sample into System CSD
set _step=2
call csdin HANDSON
if errorlevel 1 goto builderr
rem
rem Generate Sample System Maps
REM if exist SM00S.bin erase SM00S.bin
REM if exist SM00S.cob erase SM00S.cob
REM if exist SM01S.bin erase SM01S.bin
REM if exist SM01S.cob erase SM01S.cob
REM if exist SM02S.bin erase SM02S.bin
REM if exist SM02S.cob erase SM02S.cob
@REM if exist SM03S.bin erase SM03S.bin
@REM if exist SM03S.cob erase SM03S.cob
if exist SM04S.bin erase SM04S.bin
if exist SM04S.cob erase SM04S.cob
if exist SM05S.bin erase SM05S.bin
if exist SM05S.cob erase SM05S.cob
@REM if exist SM06S.bin erase SM06S.bin
@REM if exist SM06S.cob erase SM06S.cob
@REM if exist SM012S.bin erase SM012S.bin
@REM if exist SM012S.cob erase SM012S.cob
if exist UA001S.bin erase UA001S.bin
if exist UA001S.cob erase UA001S.cob
REM if exist UA002S.bin erase UA002S.bin
REM if exist UA002S.cob erase UA002S.cob
set _step=3
@REM call bms SM00S
@REM call bms SM01S
@REM call bms SM02S
@REM call bms SM03S
call bms SM04S
call bms SM05S
@REM call bms SM06S
@REM call bms SM012S
call bms UA001S
REM call bms UA002S
if errorlevel 1 goto builderr
rem Generate Sample System Programs: Translate, Compile, and Link
rem if exist SMADDSTF.dll erase SMADDSTF.dll
rem if exist SMADDSTF.obj erase SMADDSTF.obj
rem call cicstran SMADDSTF /hvw
REM if exist STFADD.dll erase STFADD.dll
REM if exist STFADD.obj erase STFADD.obj
REM call cicstran STFADD /hvw
REM if exist SM0004.dll erase SM0004.dll
REM if exist SM0004.obj erase SM0004.obj
REM call cicstran SM0004 /hvw
if exist DFHLock.DAT erase DFHLock.DAT
if exist DFHQueue.DAT erase DFHQueue.DAT
if exist DFHTEMP.DAT erase DFHTEMP.DAT
if exist SM000.dll erase SM000.dll
if exist SM000.obj erase SM000.obj
call cicstran SM000 /hvw
if exist SM001.dll erase SM001.dll
if exist SM001.obj erase SM001.obj
call cicstran SM001 /hvw
if exist SM002.dll erase SM002.dll
if exist SM002.obj erase SM002.obj
call cicstran SM002 /hvw
if exist SM003.dll erase SM003.dll
if exist SM003.obj erase SM003.obj
call cicstran SM003 /hvw
@REM if exist SM004.dll erase SM004.dll
@REM if exist SM004.obj erase SM004.obj
@REM call cicstran SM004 /hvw
@REM if exist SM005.dll erase SM005.dll
@REM if exist SM005.obj erase SM005.obj
@REM call cicstran SM005 /hvw
@REM if exist SM006.dll erase SM006.dll
@REM if exist SM006.obj erase SM006.obj
@REM call cicstran SM006 /hvw
@REM if exist SM012.dll erase SM012.dll
@REM if exist SM012.obj erase SM012.obj
@REM call cicstran SM012 /hvw
if exist UA001.dll erase UA001.dll
if exist UA001.obj erase UA001.obj
call cicstran UA001 /hvw
@REM if exist UA002.dll erase UA002.dll
@REM if exist UA002.obj erase UA002.obj
@REM call cicstran UA002 /hvw
REM if exist UAADD.dll erase UAADD.dll
REM if exist UAADD.obj erase UAADD.obj
REM call cicstran UAADD /hvw
if errorlevel 1 goto builderr
goto end
:builderr
echo Error occured at step %_step%
pause
goto end
:error
echo.
echo Please modify the environment values for CICSLINK and COBLINK
echo and re execute this batch file
goto exit
:end
set cobdirec=%tmp_cobdirec%
set tmp_cobdirec=
set copydir=%tmp_copydir%
set tmp_copydir=
set objdir=%tmp_objdir%
set tmp_objdir=
set lstdir=%tmp_lstdir%
set tmp_lstdir=
set mapdir=%tmp_mapdir%
set tmp_mapdir=
set dlldir=%tmp_dlldir%
set tmp_dlldir=
:exit
