#!/usr/bin/env bash

cd ~/projects/dotfiles || exit

if [[ "$(git remote -v | tee /dev/null | grep origin | grep -c git@github.com:seanfarley/dotfiles.git)" == 0 ]]; then
    git remote set-url origin "git@github.com:seanfarley/dotfiles.git"
fi
