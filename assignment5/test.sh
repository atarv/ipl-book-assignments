#!/bin/bash

set -euo pipefail

colorecho () {
    echo "$(tput setaf "$1")""$2""$(tput sgr0)"
}

error () {
    colorecho 1 "$*"
}

ok () {
    colorecho 2 "$*"
}

if [[ ! -d "./testsuite" ]]
then
    wget -R index.html --cut-dirs=3 -nH -r -np http://www.grammaticalframework.org/ipl-book/assignments/assignment5/testsuite/
fi

for bad in ./testsuite/bad*.fun
do
    printf "Testing %-21s : " "$bad"
    if ! ./ifun "$bad" 2> /dev/null
    then
        ok "OK"
    else 
        error "FAILED"
    fi
done

for program in ./testsuite/good*.fun
do
    printf "Testing %-21s : " "$program"
    if timeout 2s ./ifun "$program" > /dev/null || timeout 2s ./ifun -n "$program" > /dev/null
    then 
        ok "OK"
    else 
        error "FAILED"
    fi
done
