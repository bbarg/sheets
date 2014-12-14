#!/bin/bash
echo "-----------------------------------"
echo "Running Test:" $1
echo "-----------------------------------"
../preprocessor.py  `pwd`/$1.sht
cat $1.proc.sht | ../generator

