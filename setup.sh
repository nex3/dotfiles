#!/bin/bash

if [ ! -e private ]; then
    scp -r nex3@nex-3.com:config-private private
fi

if [ -e private ]; then
    bash private/setup.sh
else
    bash private-fallback.sh
fi

conf=`pwd`
cd ~

rm -rf .{emacs,elisp,bashrc,inputrc,irbrc,screenrc,gitconfig,yasnippets}
ln -s $conf/emacs.el .emacs
ln -s $conf/bashrc.sh .bashrc
ln -s $conf/irbrc.rb .irbrc
ln -s {$conf/,.}elisp
ln -s {$conf/,.}inputrc
ln -s {$conf/,.}screenrc
ln -s {$conf/,.}gitconfig

ln -s $conf/elisp/yasnippet/snippets .yasnippets

mkdir -p ~/.config
cd ~/.config
rm -rf awesome
ln -s {$conf/,}awesome

mkdir -p ~/bin
cd ~/bin
for f in $conf/bin/*; do
    newfile="`echo "$f" | sed 's/.*\/\(.*\)\..*$/\1/'`"
    rm -f "$newfile"
    ln -s "$f" "$newfile"
done
