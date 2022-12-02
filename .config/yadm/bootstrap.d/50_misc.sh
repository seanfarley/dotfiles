#!/usr/bin/env zsh

# this should exist by the time this script runs
. ~/.zshenv
. $ZDOTDIR/exports.zsh

# setup ~/sandbox first
if [ -d /hadron/euclid/home/sean ] && [ ! -L "$HOME/sandbox" ]; then
    ln -s /hadron/euclid/home/sean/sandbox "$HOME/sandbox"
elif [ ! -d "$HOME/sandbox" ]; then
    mkdir -p "$HOME/sandbox"
fi

if command -v security >/dev/null 2>&1; then
    # ensure the ssh key agent is open
    # (first time only: will prompt for password to store in keychain)
    ssh-add --apple-use-keychain
fi

if [ ! -d "$ZDOTDIR_PRIVATE" ]; then
    echo "=============================================================================="
    echo "Cloning private dotfiles"
    echo "=============================================================================="

    git clone git@github.com:seanfarley/dotfiles-private.git "$ZDOTDIR_PRIVATE"
fi

# NOTE we could simplify and cleanup these dotfiles a bunch if we used a zsh
# plugin manager but that would increase our dependencies and require another
# installation if we're not root; so far we just need {zsh, git, yadm}

# in case we run this script multiple times, we've already cloned the repo above
z_len="$( ( wc -l $_Z_DATA 2>/dev/null || echo 0 foo ) | awk '{print $1}')"
if [ $z_len -lt 50 ]; then
    cat $ZDOTDIR_PRIVATE/.z_initial $_Z_DATA 2>/dev/null | sed "s,\$HOME,$HOME," > ~/z-new
    mv ~/z-new $_Z_DATA
fi

as_dir="$XDG_DATA_HOME/zsh-autosuggestions"
if [ ! -d "$as_dir" ]; then
    git clone https://github.com/zsh-users/zsh-autosuggestions "$as_dir"
else
    cd "$as_dir" && git pull --rebase --autostash
fi

p10k_dir="$XDG_DATA_HOME/powerlevel10k"
if [ ! -d "$p10k_dir" ]; then
    git clone https://github.com/romkatv/powerlevel10k "$p10k_dir"
else
    cd "$p10k_dir" && git pull --rebase --autostash
fi

zaw_dir="$XDG_DATA_HOME/zaw"
if [ ! -d "$zaw_dir" ]; then
    git clone https://github.com/zsh-users/zaw "$zaw_dir"
else
    cd "$zaw_dir" && git pull --rebase --autostash
fi
