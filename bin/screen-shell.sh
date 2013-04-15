#!/bin/sh

screen -t `basename "$1"` sh -c "$1; bash"
