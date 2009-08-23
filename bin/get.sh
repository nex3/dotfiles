#!/bin/bash

if [ -z "$*" -o "$1" = "h" -o "$1" = "-h" -o "$1" = "--help" ]; then
    cat <<EOF
Usage: get [FLAG] [WGET-OPTIONS]... [URL]

A simple wget wrapper for piping files to stdout or extracting them in place.

Flags:
  x	Extract a tar, gzip, or bzip2 file, guess the type
  z	Extracting and assume tar.gzip
  j	Extracting and assume tar.bzip2
  t	Extracting and assume tar
  h	Show this help
  w	Show wget help
EOF
    exit 0
elif [ "$1" = "w" -o "$1" = "-w" -o "$1" = "--wget-help" ]; then
    wget --help
    exit $?
fi

if [[ -z "$2" || "$1" =~ [^xzjt] ]]; then
    flags=""
    wget="wget -O- $@"
else
    flags="$1"
    wget="wget -O- ${@:2}"
fi

# Downcase the URL to determine its type
url=`echo "${!#}" | tr [A-Z] [a-z]`

if [ -z "$flags" ]; then
    $wget
elif [[ "$flags" =~ z ]]; then
    $wget | tar xz
elif [[ "$flags" =~ j ]]; then
    $wget | tar xj
elif [[ "$flags" =~ t ]]; then
    $wget | tar x
# By this point, there must only be the x flag
elif [[ "$url" =~ \.t(ar\.)?gz(ip)?$ ]]; then
    $wget | tar xz
elif [[ "$url" =~ \.t(ar\.)?bz(ip)?2?$ ]]; then
    $wget | tar xj
elif [[ "$url" =~ \.gz(ip)?$ ]]; then
    $wget | gunzip
elif [[ "$url" =~ \.bz(ip)?2$ ]]; then
    $wget | bunzip2
elif [[ "$url" =~ \.tar$ ]]; then
    $wget | tar x
else
    # Assume .tar.gzip if we have no idea
    $wget | tar xz
fi
