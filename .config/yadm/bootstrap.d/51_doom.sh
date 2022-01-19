#!/bin/sh

[ -L "$HOME/.emacs.d" ] && exit

echo "=============================================================================="
echo "Installing doom emacs"
echo "=============================================================================="

[ ! -d "$HOME/projects/doom-emacs" ] && git clone https://github.com/hlissner/doom-emacs.git "$HOME/projects/doom-emacs"
ln -s "$HOME/projects/doom-emacs" "$HOME/.emacs.d"
