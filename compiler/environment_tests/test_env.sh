#!/bin/bash

../preprocessor.py  `pwd`/$1.sht
cat $1.proc.sht | ../generator
