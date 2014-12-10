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

check_if_fail()
{
    local name=$1
    failure=false
    if [[ "$name" == "_fail_"* ]] ; then
        failure=true
    fi
}

echo "Preprocessor Unit Tests"

for file in $preproc_test ; do
    path_to_name "$file"
    error=$( { python $preproc_path $file > outputFile; } 2>&1 )
    proc_out=$testpath.proc.sht
    check_if_fail "$test_name"

    if [[ "$failure" = true ]] ; then
        if [[ $error ]] ; then
            echo "[ ] success: $test_name"   
        else
            echo "[x] failed:  $test_name"
        fi
    else
        OUT=$(diff $proc_out $ref_out 2> errFile)
        ERR=$(<errFile)

        if [[ $ERR != "" ]] ; then
            echo "[?] no_ref:  $test_name"
        elif [[ $OUT != "" ]] ; then
            echo "[x] failed:  $test_name"
        else
            echo "[ ] success: $test_name"
        fi
    fi
done

rm -f preprocessor_tests/*.proc.sht
rm -f errFile
rm -f outputFile

exit