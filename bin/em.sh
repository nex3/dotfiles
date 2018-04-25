#!/bin/sh

if [ ${#@} -eq 0 ]; then
    tempfile=$(mktemp emacs-stdin-$USER.XXXXXXX --tmpdir)
    cat - > "$tempfile"
    args="$tempfile"
else
    args="$@"
fi

if [ "$TERM" = screen ]; then
    emacsclient -t -a 'temacs' "$args"
elif [ ! -z "$INSIDE_EMACS" ]; then
    emacsclient -a 'emacs' "$args"
else
    emacsclient -c -a 'emacs' "$args"
fi

code=$?
if [ ! -z "$tempfile" ]; then
    rm -f "$tempfile"
fi
exit $code
