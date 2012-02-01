#!/usr/bin/env bash

ED="emacs"
OPTS=""
SF="$HOME/.emacs.d/server/server"
PORT="1"
HN=$(hostname)

# dev note: since we're using bash 'getopts' arguments MUST be before the filename provided
while getopts ":h" opt; do
  case $opt in
    h)
    cat << EOF
usage: $0 options

This script calls emacsclient if the file $SF exists and the reverse port is open, else if it calls emacs

OPTIONS:
  -h      Show this message
  any other option will be passed to emacs or emacsclient
EOF
    exit 0
    ;;
    \?)
    OPTS="$OPTS -$OPTARG"
    ;;
  esac
done

# shift the optional flags provided before the filename out of the way
shift $(( OPTIND-1 ))

FN=$(readlink -f $1)

# if $FN is empty then this usually means a non-existent directory was provided
[[ -z $FN ]] && echo "error: cannot create directories (check the file path provided)" && exit 1

# if server/server file exists, then grep it for the port
[[ -r $SF ]] && PORT=$(egrep -o '127.0.0.1:([0-9]*)' $SF | sed 's/127.0.0.1://')

# check to see if port is open; apparently, ubuntu needs the -v flag
[[ -n $(nc -zv 127.0.0.1 $PORT 2>&1 | grep 'succeeded\|open') || $HN == seanfarley* ]] && ED="emacsclient -f $SF"

# build the tramp filename or local filename also, I'm assuming any
# hostname that starts with 'seanfarley' is my local
# (e.g. seanfarley.local, seanfarley.mcs.anl.gov)
[[ $HN != seanfarley* ]] && FN="/$(whoami)@$HN:$FN"

$ECHO $ED $OPTS $FN
