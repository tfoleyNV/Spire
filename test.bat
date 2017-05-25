@echo off
setlocal
pushd %~dp0

:: Argument parsing loop, for arguments that we need to handle at the .bat level

:ARGLOOP

if "%1"=="-debug" (
	set SPIRE_TEST_CONFIG=Debug
	shift
	goto :ARGLOOP
)
if "%1"=="-release" (
	set SPIRE_TEST_CONFIG=Release
	shift
	goto :ARGLOOP
)

:: When done with arguments, we'll just fall through here

SET "SPIRE_TEST_ROOT=%~dp0"

IF "%SPIRE_TEST_PLATFORM%" == "" ( SET "SPIRE_TEST_PLATFORM=x86" )
IF "%SPIRE_TEST_CONFIG%" == "" ( SET "SPIRE_TEST_CONFIG=Debug" )

set "SPIRE_TEST_BIN_DIR=%SPIRE_TEST_ROOT%bin\%SPIRE_TEST_PLATFORM%\%SPIRE_TEST_CONFIG%\\"

:: ensure that any built tools are visible
SET "PATH=%PATH%;%SPIRE_TEST_BIN_DIR%"

:: TODO: ensure that everything is built?

"%SPIRE_TEST_BIN_DIR%SpireTestTool.exe" --bindir "%SPIRE_TEST_BIN_DIR%" %*
