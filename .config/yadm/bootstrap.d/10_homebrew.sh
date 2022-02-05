#!/bin/sh

system_type=$(uname -s)
is_sudo=$(sh -c "sudo -vn 2>&1 && echo password" | grep -c password)

if [ "$system_type" = "Darwin" ] && [ $is_sudo = 1 ]; then

  # install homebrew if it's missing
  if ! command -v brew >/dev/null 2>&1; then
    echo "Installing homebrew"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
  fi

  echo "Installing homebrew packages"
  brew bundle -v --no-lock --file=/dev/stdin <<EOF
tap "d12frosted/emacs-plus"
tap "homebrew/bundle"
tap "homebrew/cask"
tap "homebrew/cask-fonts"
tap "homebrew/cask-versions"
tap "homebrew/core"
tap "homebrew/services"
brew "ansible"
brew "aspell"
brew "automake"
brew "bitwarden-cli"
brew "python"
brew "pkg-config"
brew "cairo"
brew "gnutls"
brew "harfbuzz"
brew "lz4"
brew "chromaprint"
brew "cmake"
brew "editorconfig"
brew "findutils"
brew "gawk"
brew "git"
brew "glslang"
brew "gnuplot"
brew "imagemagick"
brew "inetutils"
brew "isync"
brew "jq"
brew "libvterm"
brew "lz4"
brew "mas"
brew "meson"
brew "mp3gain"
brew "mu"
brew "ncurses"
brew "pinentry-mac"
brew "pngpaste"
brew "poppler"
brew "ripgrep"
brew "rsync"
brew "shellcheck"
brew "tectonic"
brew "terminal-notifier"
brew "wordnet"
brew "d12frosted/emacs-plus/emacs-plus@28", args: ["with-dbus", "with-elrumo1-icon", "with-xwidgets"]
cask "appcleaner"
cask "discord"
cask "docker"
cask "element"
cask "firefox"
cask "firefox-developer-edition"
cask "font-fira-code-nerd-font"
cask "font-fontawesome"
cask "font-jsmath-cmbx10"
cask "hammerspoon"
cask "karabiner-elements"
cask "mactex-no-gui"
cask "mysides"
cask "nextcloud"
cask "stats"
mas "AdGuard for Safari", id: 1440147259
mas "Bitwarden", id: 1352778147
mas "GoodNotes", id: 1444383602
mas "Numbers", id: 409203825
mas "Vectornator", id: 1219074514
mas "WireGuard", id: 1451685025
mas "World Clock", id: 956377119
mas "Xcode", id: 497799835
EOF

# start hammerspoon at login
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Hammerspoon.app", hidden:false}' &> /dev/null

# start nextcloud at login
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Nextcloud.app", hidden:false}' &> /dev/null

# open so that we can grant permissions
open -a Karabiner-Elements
open -a Stats
open -a Hammerspoon

fi
