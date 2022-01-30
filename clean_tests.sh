#!/usr/bin/env bash

shopt -s extglob
bad_tests_dir1="tests/examples/bad/*"
bad_tests_dir2="tests/lattests/bad/*"

good_tests_dir1="tests/examples/good/*/*"
good_tests_dir2="tests/lattests/good/*"

extension_tests_dir1="tests/lattests/extensions/*/*"

function clean {
    for file in $1; do
    if [[ "$file" == *".lat" ]]
    then
        compiled_file=${file%.lat}".s"
        binary_file=${file%.lat}".o"
        exec_file=${file%.lat}
        echo "$file"
        rm $binary_file
        rm $exec_file
        rm $compiled_file
    fi
done
}

clean "$bad_tests_dir1" 
clean "$bad_tests_dir2" 
clean "$good_tests_dir1" 
clean "$good_tests_dir2" 
clean "$extension_tests_dir1"
