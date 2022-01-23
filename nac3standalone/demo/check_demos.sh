#!/usr/bin/env bash

set -e

for demo in src/*.py; do
    echo "checking $demo..."
    ./interpret_demo.py $demo > interpreted.log
    ./run_demo.sh $demo > run.log
    diff -Nau interpreted.log run.log
done

echo "PASSED"
