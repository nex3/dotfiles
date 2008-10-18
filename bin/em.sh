#!/bin/sh
if [ "$TERM" = screen ]; then
    emacsclient -t -a 'emacs -nw' "$@"
else
    emacsclient -c -a 'emacs' "$@"
fi
