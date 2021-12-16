#!/usr/bin/env bash
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# make sure we are in the directory of the script so relative paths work
cd "$DIR"

[[ ! -f "$HOME/.ssh/id_rsa" ]] && echo "No ssh id_rsa!" && exit 3

exit 0
