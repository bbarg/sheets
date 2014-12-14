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

echo "Running all environment tests in current directory"

echo "Environment Testing Suite" >> $output_file

for file in $environment_tests ; do
    path_to_name $file

    echo "Running Test: $test_name"
    
    echo "" >> $output_file
    echo "===================================" >> $output_file
    
    $( ./$executable $test_name >> $output_file 2>&1)

    echo "===================================" >> $output_file
    echo "" >> $output_file
    
done

echo "done"

echo "OUTPUT FILE: '$output_file'"

rm -f *.proc.sht

exit
