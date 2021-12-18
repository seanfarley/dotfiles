#!/usr/bin/env bash

[ -f "$HOME/.local/share/blesh/ble.sh" ] && exit

echo "=============================================================================="
echo "Installing ble.sh"
echo "=============================================================================="

git clone https://github.com/akinomyoga/ble.sh "$HOME/sandbox/blesh"

make -C "$HOME/sandbox/blesh" install

rm -rf "$HOME/sandbox/blesh"
