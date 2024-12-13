#!/bin/bash
if [ -z "$1" ]
then
    echo "Usage: ./fut.sh <example_name>"
    exit 1
fi

set -e
futhark multicore $1.fut -o executable
chmod a+x executable
python fut.py