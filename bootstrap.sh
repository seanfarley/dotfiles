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
mkdir -p ~/.config/beets
mkdir -p ~/Music/beets
mkdir -p ~/Music/playlists
mkdir -p ~/.mpd
mkdir -p ~/.hammerspoon

if [[ ! -d "$HOME/projects/doom-emacs" ]]; then
  echo "doom not cloned yet!"
  echo "git clone git@github.com:hlissner/doom-emacs.git ~/projects/doom-emacs"
  git clone git@github.com:hlissner/doom-emacs.git ~/projects/doom-emacs
fi

ensure_link "../doom-emacs" ".emacs.d"
ensure_link "doom" ".doom.d"
ensure_link "hammerspoon" ".hammerspoon"

[[ ! -f "$HOME/.ssh/id_rsa" ]] && echo "No ssh id_rsa!" && exit 3

exit 0
