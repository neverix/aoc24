#!/bin/bash
if [ -z "$1" ]
then
    echo "Usage: ./ful.sh <example_name>"
    exit 1
fi

set -e
futhark literate $1.fut
rm $1