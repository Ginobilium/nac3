#!/usr/bin/env bash

set -e

if [ -z "$1" ]; then
    echo "No argument supplied"
    exit 1
fi

rm -f *.o
../../target/release/nac3standalone $1
clang -Wall -O2 -o $1.elf demo.c module*.o
./$1.elf
