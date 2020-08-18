#!/bin/bash

tmp="$(tempfile -p screenshot -s .png)"
rm "$tmp"
scrot -s "$tmp"
xclip -selection clipboard -t image/png "$tmp"
rm "$tmp"
