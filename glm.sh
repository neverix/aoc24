#!/bin/bash
if [ -z "$1" ]
then
    echo "Usage: ./glm.sh <example_name>"
    exit 1
fi

set -e
cd glm
cp ../input.txt .
gleam run -m $1