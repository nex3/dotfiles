#!/bin/bash

if which git; then
    git clone git://github.com/nex3/dotfiles.git config
elif which wget; then
    wget -O- http://github.com/nex3/dotfiles/tarball/master | tar xz
    mv dotfiles config
elif which curl; then
    curl http://github.com/nex3/dotfiles/tarball/master | tar xz
    mv dotfiles config
fi

cd config
./setup.sh

