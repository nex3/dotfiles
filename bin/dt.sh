#!/bin/bash

socket="$(emacsclient -e 'server-socket-dir' 2>/dev/null)/socket"
if [[ $? == 0 ]]; then
    ssh -R "$socket:$socket" nweiz@nweiz1.sea.corp.google.com
else
    ssh nweiz@nweiz1.sea.corp.google.com
fi
