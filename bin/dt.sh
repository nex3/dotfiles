#!/bin/bash

socket="/tmp/emacs$UID/server"
if [[ -e "$socket" ]]; then
    ssh -t -o StreamLocalBindUnlink=yes \
        -R "/tmp/emacs-ssh-server:$socket" \
        nweiz@nweiz1.sea.corp.google.com \
        'mkdir "/tmp/emacs$UID"; mv /tmp/emacs-ssh-server "/tmp/emacs$UID/server"; bash'
else
    ssh nweiz@nweiz1.sea.corp.google.com
fi
