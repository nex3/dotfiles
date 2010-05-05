#!/bin/bash

if [ -z "$2" ]; then
    name=`basename $1`
else
    name="$2"
fi

filename="$HOME/Dropbox/Public/$name"
cp "$1" "$filename"
echo "Uploading $filename..."
until dropbox filestatus "$filename" | grep -q ': up to date$'; do sleep 0.1; done
dropbox puburl "$filename"
