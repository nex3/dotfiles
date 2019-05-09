#!/bin/bash

set -e

params=""

while (( "$#" )); do
    case "$1" in
        --activate)
            activate=true
            shift
            ;;
        --) # end argument parsing
            shift
            break
            ;;
        -*|--*=) # unsupported flags
            echo "Error: Unsupported flag $1" >&2
            exit 1
            ;;
        *) # preserve positional arguments
            params="$params $1"
            shift
            ;;
    esac
done

# set positional arguments in their proper place
eval set -- "$params"

version=$1

if [ "$1" = dev ]; then
    url="https://storage.googleapis.com/dart-archive/channels/dev/release/latest/sdk/dartsdk-linux-x64-release.zip"
elif [ "$1" = stable ]; then
    url="https://storage.googleapis.com/dart-archive/channels/stable/release/latest/sdk/dartsdk-linux-x64-release.zip"
elif [[ "$1" == *"-dev."* ]]; then
    url="https://storage.googleapis.com/dart-archive/channels/dev/release/$1/sdk/dartsdk-linux-x64-release.zip"
else
    url="https://storage.googleapis.com/dart-archive/channels/stable/release/$1/sdk/dartsdk-linux-x64-release.zip"
fi

wget "$url"
actual_version=$(unzip -qc dartsdk-linux-x64-release.zip dart-sdk/version)
if [[ ! -d ~/src/dart-$actual_version ]]; then
    unzip -q dartsdk-linux-x64-release.zip
    mv dart-sdk ~/src/dart-$actual_version

    for file in `ls ~/src/dart-$actual_version/bin/`; do
        ln -s "$HOME/src/dart-$actual_version/bin/$file" "$HOME/bin/$file-$actual_version"
    done

    echo "Downloaded Dart $actual_version!"
else
    echo "Dart $actual_version is already installed!"
fi
rm dartsdk-linux-x64-release.zip


if [[ ! -z "$activate" ]]; then
    rm -f ~/src/dart-current
    ln -sf ~/src/dart-$actual_version ~/src/dart-current
    echo "Activated Dart $actual_version!"
fi
