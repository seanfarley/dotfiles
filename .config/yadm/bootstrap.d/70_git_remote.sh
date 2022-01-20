#!/bin/sh

echo "Updating the yadm repo origin URL"

yadm_bin="$(command -v yadm)"
[ -z "$yadm_bin" ] && yadm_bin="$HOME/.local/bin/yadm"
"$yadm_bin" remote set-url origin "git@github.com:seanfarley/dotfiles.git"
