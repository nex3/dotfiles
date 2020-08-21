#!/bin/sh

if [ "$1" = "-" ]; then
    tempfile=$(mktemp emacs-stdin-$USER.XXXXXXX --tmpdir)
    cat - > "$tempfile"
    args="$tempfile"
else
    args="$@"
fi

if [ "$TERM" = screen ]; then
    remote_system="$(emacsclient --eval '(system-name)' 2> /dev/null)"
    if [[ "\"$HOSTNAME\"" != "$remote_system" ]]; then
        emacsclient -a 'temacs' "--tramp=/ssh:$USER@$HOSTNAME:" $args
    else
        emacsclient -a 'temacs' $args
    fi
elif [ ! -z "$INSIDE_EMACS" ]; then
    emacsclient -a 'emacs' $args
else
    emacsclient -c -a 'emacs' $args
fi

code=$?
if [ ! -z "$tempfile" ]; then
    rm -f "$tempfile"
fi
exit $code
