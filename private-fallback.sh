#!/bin/bash

for f in *.in; do
    ln -sf "$f" "`echo "$f" | sed 's/\.in$//'`"
done
