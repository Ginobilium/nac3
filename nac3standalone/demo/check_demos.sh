#!/usr/bin/env bash

set -e

for demo in src/*.py; do
    echo "checking $demo..."
    diff -Nau <(./interpret_demo.py $demo) <(./run_demo.sh $demo)
done

echo "PASSED"
