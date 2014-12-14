#!/bin/bash

rm -f *.proc.sht
rm -f envtests.output
environment_tests=$(find . -name "*\.sht")
executable="./test_env.sh"

path_to_name()
{
    local fullpath=$1
    test_path="${fullpath%.*}"   # Strip Extension
    test_name="${test_path##*/}" # Strip Preceding Path
}

echo "Running all environment tests in current directory"

echo "Environment Testing Suite" >> envtests.output

for file in $environment_tests ; do
    path_to_name $file

    echo "Running Test: $test_name"
    
    echo "" >> envtests.output
    echo "===================================" >> envtests.output
    
    $( ./$executable $test_name >> envtests.output 2>&1)

    echo "===================================" >> envtests.output
    echo "" >> envtests.output
    
done

echo "done"

rm -f *.proc.sht

exit
