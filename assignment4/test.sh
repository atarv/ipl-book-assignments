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

TESTSUITE_PATH="${1-}"

if [ -z "$TESTSUITE_PATH" ]
then
    echo "Usage: ./test.sh /path/to/lab3-testsuite/good/"
    exit 1
fi

PROGRAMS=$(ls -1 "$TESTSUITE_PATH"*.cc)

for PROGRAM in $PROGRAMS
do
    # Compile program
    COMPILELOG=$(./ccpp "$PROGRAM")

    if [ "$?" -ne "0" ]
    then
        echo "$COMPILELOG"
    fi

    echo -n "Running $PROGRAM: "
    INPUT="$PROGRAM.input"
    if [ -e "$INPUT" ]
    then
        RESULT=$(java Foo < "$INPUT")
    else
        RESULT=$(java Foo)
    fi

    EXPECTED=$(cat "$PROGRAM.output")

    if [ "$RESULT" = "$EXPECTED" ]
    then
        ok "OK"
    else 
        error "FAILED"
    fi
done
