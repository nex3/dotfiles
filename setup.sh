#!/bin/bash

conf=`pwd`
cd ~

rm -rf .{emacs,elisp,bashrc,inputrc,irbrc,screenrc,gitconfig}
ln -s {$conf/,.}emacs
ln -s {$conf/,.}elisp
ln -s {$conf/,.}bashrc
ln -s {$conf/,.}inputrc
ln -s {$conf/,.}irbrc
ln -s {$conf/,.}screenrc
ln -s {$conf/,.}gitconfig
