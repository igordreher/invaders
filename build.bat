@echo off

if not exist build mkdir build

if "%1" == "debug" (
	odin build src -debug -out:build\debug.exe
) else if "%1" == "release" (
	odin build src -out:build\invaders.exe -o:speed -subsystem:windows
) else (
	odin build src -debug -out:build\debug.exe
)
