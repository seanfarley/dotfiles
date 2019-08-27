#!/usr/bin/env bash
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# make sure we are in the directory of the script so relative paths work
cd "$DIR"

function ensure_link {
  local DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
  local NEW="$2"
  local is_diff=0
  test -z "$NEW" && NEW=".$1"

  if [[ -f "$HOME/$NEW" ]]; then
    diff -u "$DIR/$1" "$HOME/$NEW" || is_diff=1
    if [[ is_diff -ne 0 ]]
    then
      cat <<-EOF


>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Please check the diff! Either remove the target file $HOME/$NEW or copy the changes to the dotfiles repo.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
EOF
      exit 2
    else
      rm -f "$HOME/$NEW"
    fi
  fi

  test -d "$HOME/$NEW" && rm -r "$HOME/$NEW"
  # special-case authorized_keys because some systems don't like it being a
  # symlink; but we keep it in this function so we can check the diff
  if [[ "$NEW" != ".ssh/authorized_keys" ]]; then
    test -L "$HOME/$NEW" || ln -s "$DIR/$1" "$HOME/$NEW"
  else
    cp "$DIR/$1" "$HOME/$NEW"
    chmod 600 "$HOME/$NEW"
  fi
}

test -d $HOME/.hg && echo "$HOME is still a repo, please check and remove" && exit 1

mkdir -p ~/projects
mkdir -p ~/sandbox
mkdir -p ~/.ipython
mkdir -p ~/.ssh
mkdir -p ~/.gnupg
mkdir -p ~/.config/karabiner
mkdir -p ~/.config/alacritty
mkdir -p ~/.hammerspoon

if [[ ! -d "$HOME/projects/doom-emacs" ]]; then
  echo "doom not cloned yet!"
  echo "git clone git@github.com:hlissner/doom-emacs.git ~/projects/doom-emacs"
  exit 128
fi

ensure_link "aliases"
ensure_link "bash_prompt"
ensure_link "dir_colors"
ensure_link "../doom-emacs" ".emacs.d"
ensure_link "doom" ".doom.d"
ensure_link "ec"
ensure_link "exports"
ensure_link "functions"
ensure_link "gitconfig"
ensure_link "hammerspoon" ".hammerspoon"
ensure_link "hgrc"
ensure_link "hgignore"
ensure_link "inputrc"
ensure_link "ipython/profile_default"
ensure_link "latexmkrc"
ensure_link "mbsyncrc"
ensure_link "profile"
ensure_link "ssh/authorized_keys"
ensure_link "ssh/config"
ensure_link "ssh/environment"
ensure_link "flake8" ".config/flake8"
ensure_link "profile" ".bashrc"
ensure_link "hgignore" ".gitignore"
ensure_link "karabiner.json" ".config/karabiner/karabiner.json"
ensure_link "gpg-agent.conf" ".gnupg/gpg-agent.conf"
ensure_link "gpg.conf" ".gnupg/gpg.conf"
ensure_link "xonshrc"
ensure_link "ripgreprc"
ensure_link "vault_pass.txt"
ensure_link "alacritty.yml" ".config/alacritty/alacritty.yml"
ensure_link "epass.sh"

[[ ! -f "$HOME/.ssh/id_rsa" ]] && echo "No ssh id_rsa!" && exit 3

exit 0
