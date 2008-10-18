#!/bin/sh
emacsclient -t "$@" || emacs -nw "$@"
