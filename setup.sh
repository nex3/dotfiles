#!/bin/bash

if [ ! -e private ]; then
    scp -r nex3@nex-3.com:config-private private
    chmod -R go-rwx private
fi

if [ -e private ]; then
    bash private/setup.sh
else
    bash private-fallback.sh
fi

rm -rf .git/hooks
ln -s ../git-hooks .git/hooks

conf=`pwd`
cd ~

rm -rf .{info,emacs,elisp,bashrc,inputrc,irbrc,factor-rc,screenrc,Xresources,gitconfig,yasnippets}
ln -s $conf/emacs.el .emacs
ln -s $conf/bashrc.sh .bashrc
ln -s $conf/irbrc.rb .irbrc
ln -s $conf/rc.factor .factor-rc
ln -s {$conf/,.}elisp
ln -s {$conf/,.}info
ln -s {$conf/,.}inputrc
ln -s {$conf/,.}screenrc
ln -s {$conf/,.}Xresources
ln -s {$conf/,.}gitconfig

$conf/git-hooks/post-commit
ln -s $conf/elisp/yasnippet/snippets .yasnippets

which xrdb &> /dev/null && [ ! -z "$DISPLAY" ] && xrdb -merge .Xresources

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
