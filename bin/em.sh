#!/bin/sh
if [ "$TERM" = screen ]; then
    emacsclient -t -a 'temacs' "$@"
else
    emacsclient -c -a 'emacs' "$@"
fi
