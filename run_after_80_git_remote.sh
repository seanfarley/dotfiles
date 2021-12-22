#!/usr/bin/env bash

cd ~/projects/dotfiles || exit

if [[ "$(git remote get-url --all origin)" != "git@github.com:seanfarley/dotfiles.git" ]]; then
    git remote set-url origin "git@github.com:seanfarley/dotfiles.git"
fi
