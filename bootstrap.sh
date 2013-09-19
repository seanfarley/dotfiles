#!/usr/bin/env bash
set -e

function ensure_link {
  local DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
  local NEW="$2"
  test -z "$NEW" && NEW=".$1"
  test -f "$HOME/$NEW" && rm "$HOME/$NEW"
  test -L "$HOME/$NEW" || ln -s "$DIR/$1" "$HOME/$NEW"
}

test -d $HOME/.hg && echo "$HOME is still a repo, please check and remove" && exit 1

mkdir -p ~/projects
mkdir -p ~/sandbox

ensure_link "aliases"
ensure_link "aspell.en.prepl"
ensure_link "aspell.en.pws"
ensure_link "bash_prompt"
ensure_link "ctags"
ensure_link "debugshell.py"
ensure_link "dir_colors"
ensure_link "emacs" ".emacs.d"
ensure_link "edit.sh"
ensure_link "exports"
ensure_link "functions"
ensure_link "gdbinit"
ensure_link "gitconfig"
ensure_link "globalrc"
ensure_link "hgrc"
ensure_link "hgignore"
ensure_link "inputrc"
ensure_link "lynx.lss"
ensure_link "lynxrc"
ensure_link "msmtprc"
ensure_link "offlineimap.py"
ensure_link "offlineimaprc"
ensure_link "osx"
ensure_link "port-rdependents.py"
ensure_link "profile"
ensure_link "ssh/authorized_keys"
ensure_link "ssh/config"
ensure_link "ssh/environment"

ensure_link "profile" ".bashrc"
ensure_link "hgignore" ".gitignore"
