#!/usr/bin/env bash

set -e

if [ -z "$1" ]; then
    echo "No argument supplied"
    exit 1
fi

rm -f *.o
../../target/release/nac3standalone $1
rustc -o $1.elf demo.rs -Clink-arg=./module.o
./$1.elf
