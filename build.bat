@echo off

if not exist build mkdir build

odin build src -debug -out:build\debug.exe
::odin build src -debug -out:build\test.exe -define:CPU_DIAG=true
