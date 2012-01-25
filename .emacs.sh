#!/usr/bin/env bash

ED="emacs"
FN="$1"
SF="$HOME/.emacs.d/server/server"
PORT="1"
HN=$(hostname)

# if server/server file exists, then grep it for the port
[[ -r $SF ]] && PORT=$(egrep -o '127.0.0.1:([0-9]*)' $SF | sed 's/127.0.0.1://')

# check to see if port is open; apparently, ubuntu needs the -v flag
[[ -n $(nc -zv 127.0.0.1 $PORT 2>&1 | grep succeeded) || $HN == seanfarley* ]] && ED="emacsclient -f $SF"

# build the tramp filename or local filename also, I'm assuming any
# hostname that starts with 'seanfarley' is my local
# (e.g. seanfarley.local, seanfarley.mcs.anl.gov)
[[ $HN != seanfarley* ]] && FN="/$(whoami)@$HN:$1"

$ED $FN
