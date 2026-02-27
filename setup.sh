#!/bin/bash

rm -rf .git/hooks
ln -s ../git-hooks .git/hooks

conf=`pwd`
cd ~

rm -rf .{emacs,bashrc,irbrc,factor-rc}
mkdir -p ~/.emacs.d
ln -sf $conf/emacs-early.el .emacs.d/early-init.el
ln -sf $conf/emacs.el .emacs
ln -sf $conf/bashrc.sh .bashrc
ln -sf $conf/irbrc.rb .irbrc
ln -sf $conf/rc.factor .factor-rc

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

if [ -d ~/var/app/io.gitlab.librewolf-community/.librewolf/ ]; then
    ln -sf $conf/librewolf.overrides.cfg ~/var/app/io.gitlab.librewolf-community/.librewolf/librewolf.overrides.cfg
fi

cd ~/bin
for f in $conf/bin/*; do
    newfile="`echo "$f" | sed 's/.*\/\(.*\)\..*$/\1/'`"
    rm -f "$newfile"
    ln -s "$f" "$newfile"
done

git clone https://github.com/deficient/deficient awesome/deficient

mkdir -p ~/src
if [ ! -d ~/src/dart-current ]; then
    ~/bin/get-dart --activate latest
fi

## ----------
## -- Tiling-Esque Acommodations
## ----------

if which gsettings > /dev/null; then
    # The default binding is M-SPC which I use in Emacs.
    gsettings set org.gnome.desktop.wm.keybindings activate-window-menu "@as []"

    # The default binding also includes S-m which I use to switch workspaces.
    gsettings set org.gnome.shell.keybindings toggle-message-tray "['<Super>v']"

    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "['<Super>c']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2 "['<Super>p']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3 "['<Super>m']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4 "['<Super>i']"
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-5 "['<Super>e']"

    gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-1 "['<Super><Shift>c']"
    gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-2 "['<Super><Shift>p']"
    gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-3 "['<Super><Shift>m']"
    gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-4 "['<Super><Shift>i']"
    gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-5 "['<Super><Shift>e']"
fi
