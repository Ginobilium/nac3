#!/usr/bin/env bash

set -e

count=0
for demo in src/*.py; do
    echo -n "checking $demo... "
    ./interpret_demo.py $demo > interpreted.log
    ./run_demo.sh $demo > run.log
    diff -Nau interpreted.log run.log
    echo "ok"
    let "count+=1"
done

echo "Ran $count demo checks - PASSED"
