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
if exist SM000.bin erase SM000.bin
if exist SM000.cob erase SM000.cob
set _step=3
call bms SM000
if errorlevel 1 goto builderr
rem Generate Sample System Programs: Translate, Compile, and Link
if exist SM0000.dll erase SM0000.dll
if exist SM0000.obj erase SM0000.obj
call cicstran SM0000 /hvw
if exist SM001.dll erase SM001.dll
if exist SM001.obj erase SM001.obj
call cicstran SM001 /hvw
if exist SM002.dll erase SM002.dll
if exist SM002.obj erase SM002.obj
call cicstran SM002 /hvw
if exist SM003.dll erase SM003.dll
if exist SM003.obj erase SM003.obj
call cicstran SM003 /hvw
if exist SM004.dll erase SM004.dll
if exist SM004.obj erase SM004.obj
call cicstran SM004 /hvw
if exist SM005.dll erase SM005.dll
if exist SM005.obj erase SM005.obj
call cicstran SM005 /hvw
if exist SM006.dll erase SM006.dll
if exist SM006.obj erase SM006.obj
call cicstran SM006 /hvw
if exist UA001.dll erase UA001.dll
if exist UA001.obj erase UA001.obj
call cicstran UA001 /hvw
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
