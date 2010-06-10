#!/bin/bash

function usage () {
    cat <<EOF
Usage: $0 [OPTION]... FILE [NAME]

Copy FILE to the Dropbox public folder.
If NAME is given, call it that.

Options:
  -l, --link		Make a symlink rather than a copy
  -h, --help		Show this message
EOF
}

args="$@"
link=
while [[ ${args[0]} =~ ^- ]]; do
    case ${args[0]} in
        --link|-l)
            link=1
            exit
            ;;
        --help|-h)
            usage
            exit 1
            ;;
        *)
            echo "Unrecognized option $1"
            exit 1
            ;;
    esac
    unset args[0]
done

if [ -z "$1" ]; then
    usage
    exit 1
elif [ -z "$2" ]; then
    name=`basename $1`
else
    name="$2"
fi

filename="$HOME/Dropbox/Public/$name"
if [ "$link" = 1 ]; then
    ln -s "$1" "$filename"
else
    cp "$1" "$filename"
fi

echo "Uploading $filename..."
until dropbox filestatus "$filename" | grep -q ': up to date$'; do sleep 0.1; done
dropbox puburl "$filename"
