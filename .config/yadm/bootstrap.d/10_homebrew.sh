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
brew "python"
brew "ansible"
brew "autojump"
brew "aspell"
brew "bitwarden-cli"
brew "cairo"
brew "chromaprint"
brew "cmake"
brew "editorconfig"
brew "gawk"
brew "git"
brew "glslang"
brew "gnuplot"
brew "findutils"
brew "imagemagick"
brew "inetutils"
brew "isync"
brew "jq"
brew "lz4"
brew "mu"
brew "mas"
brew "meson"
brew "mp3gain"
brew "npm"
brew "pinentry-mac"
brew "pngpaste"
brew "shellcheck"
brew "tectonic"
brew "terminal-notifier"
brew "ncurses"
brew "wordnet"
# ==================================== fonts ===================================
cask "font-fontawesome"
cask "font-jsmath-cmbx10"
cask "font-fira-code-nerd-font"
cask "font-iosevka-nerd-font"
cask "font-monoid-nerd-font"
# ==================================== emacs ===================================
brew "libvterm"
brew "d12frosted/emacs-plus/emacs-plus@28", args: ["with-dbus", "with-elrumo1-icon", "with-native-comp", "with-xwidgets"]
# ==================================== casks ===================================
cask "firefox"
cask "firefox-developer-edition"
cask "hammerspoon"
cask "nextcloud"
cask "docker"
cask "mysides"
cask "stats"
cask "appcleaner"
# ================================ mac app store ===============================
mas "AdGuard for Safari", id: 1440147259
mas "Bitwarden", id: 1352778147
mas "GoodNotes", id: 1444383602
mas "Numbers", id: 409203825
mas "Vectornator", id: 1219074514
mas "WireGuard", id: 1451685025
mas "Xcode", id: 497799835
EOF

# start hammerspoon at login
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Hammerspoon.app", hidden:false}' &> /dev/null

# start nextcloud at login
osascript -e 'tell application "System Events" to make login item at end with properties {path:"/Applications/Nextcloud.app", hidden:false}' &> /dev/null

fi