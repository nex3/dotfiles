#!/bin/bash

conf=`pwd`
cd ~

rm .{emacs,elisp,bashrc,inputrc}
ln -s {$conf/,.}emacs
ln -s {$conf/,.}elisp
ln -s {$conf/,.}bashrc
ln -s {$conf/,.}inputrc