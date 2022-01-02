#!/usr/bin/env bash

# setup ~/sandbox first
if [[ -d /hadron/euclid/home/sean && ! -L "$HOME/sandbox" ]]; then
    ln -s /hadron/euclid/home/sean/sandbox "$HOME/sandbox"
elif [[ ! -d "$HOME/sandbox" ]]; then
    mkdir -p "$HOME/sandbox"
fi

if command -v fd-find >/dev/null 2>&1; then
    # ensure the ssh key agent is open
    ssh-add -K
fi

[ -f "$HOME/.local/share/blesh/ble.sh" ] && exit

echo "=============================================================================="
echo "Installing ble.sh"
echo "=============================================================================="

git clone https://github.com/akinomyoga/ble.sh "$HOME/sandbox/blesh"

make -C "$HOME/sandbox/blesh" install

rm -rf "$HOME/sandbox/blesh"
