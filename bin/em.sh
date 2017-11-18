#!/bin/sh
if [ "$TERM" = screen ]; then
    emacsclient -t -a 'temacs' "$@"
elif [ ! -z "$INSIDE_EMACS" ]; then
    emacsclient -a 'emacs' "$@"
else
    emacsclient -c -a 'emacs' "$@"
fi
