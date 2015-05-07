#!/bin/bash

user="$1"
branch="$2"
repo=`git config --get remote.origin.url | \
    sed 's/.*\/\([^.]*\)\(\.git\)\?$/\1/'`

remote=`git config --get "remote.$user.url"`
success=$?

# Only set -e here because we don't want to exit if getting the remote fails.
set -e

if [[ "$success" != 0 ]]; then
    echo git remote add "$user" "git://github.com/$user/$repo"
    git remote add "$user" "git://github.com/$user/$repo"
else
    echo "remote $user is $remote"
fi

echo git fetch "$user"
git fetch "$user"

if [ -n "$branch" ]; then
    echo git checkout -b "$user.$branch" "$user/$branch"
    git checkout -b "$user.$branch" "$user/$branch"
fi
