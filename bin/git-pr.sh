#!/bin/bash -e

user="$1"
branch="$2"
repo=`git config --get remote.origin.url | \
    sed 's/.*\/\([^.]*\)\(\.git\)\?$/\1/'`

git remote add "$user" "git://github.com/$user/$repo"
git fetch "$user"

if [ -n "$branch" ]; then
    git checkout -b "$user.$branch" "$user/$branch"
fi
