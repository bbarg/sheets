#!/bin/bash
echo "-----------------------------------"
echo "Running Test:" $1
echo "-"
head -n 1 $1.sht| grep "#~"  | sed "s/#~ //" | sed "s/~#//"


echo "-----------------------------------"
../preprocessor.py  `pwd`/$1.sht
cat $1.proc.sht | ../generator

