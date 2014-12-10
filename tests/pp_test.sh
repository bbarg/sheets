#!/bin/bash

preproc_test=$(find preprocessor_tests -name *\.sht)
preproc_path="../compiler/preprocessor.py"

path_to_name()
{
    local fullpath=$1
    testpath="${fullpath%.*}"   # Strip Extension
    test_name="${testpath##*/}" # Strip Preceding Path
    ref_out=$(dirname $fullpath)/refout/$test_name.refout
}

printf "Preprocessor Unit Tests\n"

for file in $preproc_test
do
    path_to_name "$file"
    printf "=== $test_name ===\n"
    
    python $preproc_path $file
    proc_out=$testpath.proc.sht
    

    if [[ outcome ]] && [[ !$(diff $proc_out $ref_out) ]]
    then
        echo "success: $test_name"
    fi

done

rm -f preprocessor_tests/*.proc.sht

exit
