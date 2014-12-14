#!/bin/bash
echo "Running test" $1
echo "###############################"
../preprocessor.py  `pwd`/$1.sht
cat $1.proc.sht | ../generator
echo "###############################"
echo "###############################"
