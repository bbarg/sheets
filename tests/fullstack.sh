#!/bin/bash

compiler_tests=$(find suite -name *\.sht)
preproc_path="../compiler/preprocessor.py"

path_to_name()
{
    local fullpath=$1
    testpath="${fullpath%.*}"   # Strip Extension
    test_name="${testpath##*/}" # Strip Preceding Path
}

printf "Full Stack Sheets Test Suite\n"

for file in $compiler_tests
do
    path_to_name "$file"
    printf "=== $test_name ===\n"
    
    python $preproc_path $file
    proc_out=$testpath.proc.sht

    printf "$proc_out\n"
    
    # # stdout of parser
    # output=$(./sast < "$file" 2> "$tmp_file")
    # # boolean result
    # outcome=$(echo "$output" | grep $success)
    # # true if test expected to fail
    # fail_test=$(echo "$file" |  grep "fail_")
    
done

rm -f suite/*.proc.sht

exit
