#!/bin/sh

if [[ "$1" = /* ]]; then
    echo "file://$1"
else
    echo "file://`pwd`/$1"
fi
