#!/bin/bash

if [ -z "$1" ]
  then
    echo "No input to make refout"
else
    file=$1
fi

preproc_path="../compiler/preprocessor.py"

path_to_name()
{
    local fullpath=$1
    testpath="${fullpath%.*}"   # Strip Extension
    test_name="${testpath##*/}" # Strip Preceding Path
    ref_out=$(dirname $fullpath)/refout/$test_name.refout
}

path_to_name "$file"

echo "Making Reference Output for "$test_name
python $preproc_path $file
proc_out=$testpath.proc.sht
$(mv $proc_out $ref_out)
exit