#!/usr/bin/env bash
mkdir -p build

if [[ $1 == "debug" ]]
then 
  odin build src -debug -out:build/debug
elif [[ $1 == "release" ]]
then
  odin build src -o:speed -out:build/invaders
else
  odin build src -debug -out:build/debug
fi
