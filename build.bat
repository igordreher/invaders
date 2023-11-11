@echo off

if not exist build mkdir build

odin build src -debug -out:build\emu.exe
