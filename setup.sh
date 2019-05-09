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

rm -rf .{emacs,bashrc,irbrc,factor-rc}
ln -s $conf/emacs.el .emacs
ln -s $conf/bashrc.sh .bashrc
ln -s $conf/irbrc.rb .irbrc
ln -s $conf/rc.factor .factor-rc

for file in elisp info inputrc screenrc Xresources Xmodmap gitconfig gitignore.global; do
    rm -rf .$file
    ln -s {$conf/,.}$file
done

$conf/git-hooks/post-commit

which xrdb &> /dev/null && [ ! -z "$DISPLAY" ] && xrdb -merge .Xresources

mkdir -p ~/.config
cd ~/.config
rm -rf awesome
ln -s {$conf/,}awesome

mkdir -p ~/bin
for executable in dart dart2aot dartaotruntime dart2js dartanalyzer dartdevc dartdoc dartfmt pub; do
    ln -sf ~/src/dart-current/bin/$executable ~/bin/$executable
done

cd ~/bin
for f in $conf/bin/*; do
    newfile="`echo "$f" | sed 's/.*\/\(.*\)\..*$/\1/'`"
    rm -f "$newfile"
    ln -s "$f" "$newfile"
done

mkdir -p ~/src
if [ ! -d ~/src/dart-current ]; then
    ~/bin/get-dart --activate latest
fi
