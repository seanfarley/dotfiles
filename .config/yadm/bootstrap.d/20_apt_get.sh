#!/bin/sh

system_type=$(uname -s)
is_sudo=$(sh -c "sudo -vn 2>&1 && echo password" | grep -c password)

if [ "$system_type" = "Linux" ] && [ $is_sudo = 1 ] && command -v apt-get >/dev/null 2>&1; then
    sudo apt-get -y install \
        openssh-server \
        ripgrep \
        fd-find \
        gcc \
        git \
        python3-dev \
        python3-pip \
        python3-venv \
        gawk \
        zsh
fi
