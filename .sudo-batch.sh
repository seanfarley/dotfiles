#!/usr/bin/env bash

[ $# -ne 1 ] && echo "usage: $0 FILENAME" && exit 1

while read line; do eval "$line"; done < $1
