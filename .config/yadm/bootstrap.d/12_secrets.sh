#!/bin/sh

system_type=$(uname -s)
is_sudo=$(sh -c "sudo -vn 2>&1 && echo password" | grep -c password)

if [ "$system_type" = "Darwin" ] && [ $is_sudo = 1 ]; then

    echo "Configuring bitwarden server"

    [[ "$(bw config server)" == *"bitwarden.farley.io"* ]] || bw config server "https://bitwarden.farley.io"

    bw login --check &> /dev/null || bw login sean@farley.io "$(/usr/bin/security find-internet-password -w -a sean@farley.io -s bitwarden.farley.io)"

fi
