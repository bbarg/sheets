#!/bin/bash

compiler_tests=$(find suite -name *\.sht)
preprocessor_path="../compiler/preprocessor.py"

path_to_name() {
    local fullpath=$1 #strip file type & set global variable
    testpath="${fullpath%.*}"
    test_name="${testpath##*/}" #strip the preceding path
}

for file in $compiler_tests
do
    path_to_name "$file"
    printf "$file\n"

    python $preprocessor_path $file

done

exit