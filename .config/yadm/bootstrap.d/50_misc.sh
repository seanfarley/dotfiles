#!/bin/sh

# just in case this script is run without sourcing custom ~/.zshenv first
 export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}

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

git_dir=~/.dotfiles-private
if [ ! -d "$git_dir" ]; then
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

# NOTE we could simplify and cleanup these dotfiles a bunch if we used a zsh
# plugin manager but that would increase our dependencies and require another
# installation if we're not root; so far we just need {zsh, git, yadm}

# in case we run this script multiple times, we've already cloned the repo above
z_len="$( ( wc -l ~/.z 2>/dev/null || echo 0 foo ) | awk '{print $1}')"
if [ $z_len -lt 50 ]; then
    cat ~/.z_initial ~/.z 2>/dev/null | sed "s,\$HOME,$HOME," > ~/z-new
    mv ~/z-new ~/.z
fi

if [ ! -d "$HOME/.zsh/zsh-autosuggestions" ]; then
    git clone https://github.com/zsh-users/zsh-autosuggestions "$HOME/.zsh/zsh-autosuggestions"
fi

if [ ! -d "$HOME/.zsh/powerlevel10k" ]; then
    git clone https://github.com/romkatv/powerlevel10k "$HOME/.zsh/powerlevel10k"
fi

zaw_dir="$XDG_DATA_HOME/zaw"
if [ ! -d "$zaw_dir" ]; then
    git clone https://github.com/zsh-users/zaw "$zaw_dir"
fi
