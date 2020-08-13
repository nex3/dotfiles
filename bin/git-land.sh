#!/bin/bash
set -e

if [ -z "$1" ]; then
    branch=`git rev-parse --abbrev-ref HEAD`
    git co master
else
    branch="$1"
fi

git br -D "$branch"
git pull origin master
rm -f ".git/remotes/origin/$branch"
