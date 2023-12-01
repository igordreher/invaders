@echo off

if not exist build mkdir build

odin build src -debug -out:build\emu.exe
odin build src -debug -out:build\test.exe -define:CPU_DIAG=true
