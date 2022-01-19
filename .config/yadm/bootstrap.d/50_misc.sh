#!/usr/bin/env bash

# setup ~/sandbox first
if [[ -d /hadron/euclid/home/sean && ! -L "$HOME/sandbox" ]]; then
    ln -s /hadron/euclid/home/sean/sandbox "$HOME/sandbox"
elif [[ ! -d "$HOME/sandbox" ]]; then
    mkdir -p "$HOME/sandbox"
fi

if command -v security >/dev/null 2>&1; then
    # ensure the ssh key agent is open
    ssh-add -K
fi

git_dir=~/.dotfiles-private
if [[ ! -d "$git_dir" ]]; then
    echo "=============================================================================="
    echo "Cloning private dotfiles"
    echo "=============================================================================="

    cd ~ || return

    git --git-dir="$git_dir" init
    git --git-dir="$git_dir" config core.bare false
    git --git-dir="$git_dir" config status.showuntrackedfiles no
    git --git-dir="$git_dir" remote add origin git@github.com:seanfarley/dotfiles-private.git
    git --git-dir="$git_dir" fetch
    git --git-dir="$git_dir" reset origin/main
    git --git-dir="$git_dir" branch -u origin/main
    git --git-dir="$git_dir" checkout -- .
fi

# in case we run this script multiple times, we've already cloned the repo above
z_len="$( ( wc -l ~/.z 2>/dev/null || echo 0 foo ) | awk '{print $1}')"
if [[ $z_len -lt 50 ]]; then
    cat ~/.z_initial ~/.z 2>/dev/null | sed "s,\$HOME,$HOME," > ~/z-new
    mv ~/z-new ~/.z
fi
