#!/bin/bash

executable="./test_env.sh"
output_file="envtests.output"

rm -f *.proc.sht
rm -f envtests.output
environment_tests=$(find . -name "*\.sht")

path_to_name()
{
    local fullpath=$1
    test_path="${fullpath%.*}"   # Strip Extension
    test_name="${test_path##*/}" # Strip Preceding Path
}

check_if_fail()
{
    local name=$1
    shouldfail=false
    if [[ "$name" == *"_n_"* ]] ; then
        shouldfail=true
    fi
}

echo "Running all environment tests in current directory"
echo ""
echo "  Note: an 'x' next to a test indicates that the test" 
echo "  is failing when it is expected to pass, or passing" 
echo "  when expected to fail. This does not guarantee that" 
echo "  a successful test is passing or failing for the" 
echo "  expected reason, so make sure to verify that all" 
echo "  outputs are correct"
echo ""

echo "Environment Testing Suite" >> $output_file

for file in $environment_tests ; do
    path_to_name $file
    check_if_fail $test_name

    echo "" >> $output_file
    echo "===================================" >> $output_file
    
    VAR=$( ./$executable $test_name 2>&1 )
    echo "$VAR" >> $output_file

    fatal_error=$(echo $VAR | grep 'Fatal error:')

    if [[ $shouldfail == true ]] ; then
        if [[ $fatal_error == '' ]] ; then
            echo "[x] Running Test: $test_name"
        else
            echo "[ ] Running Test: $test_name"
        fi
    else
        if [[ $fatal_error == '' ]] ; then
            echo "[ ] Running Test: $test_name"
        else
            echo "[x] Running Test: $test_name"
        fi
    fi


    echo "===================================" >> $output_file
    echo "" >> $output_file
    
done

echo "done"

echo "OUTPUT FILE: '$output_file'"

rm -f *.proc.sht

exit
