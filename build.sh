#!/bin/bash

mkdir -p bin

rm -r bin/*

ghc -O2 Main.hs -outputdir bin/

