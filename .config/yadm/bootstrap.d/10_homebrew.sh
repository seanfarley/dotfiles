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
brew "act"
brew "ansible"
brew "aspell"
brew "automake"
brew "bear"
brew "binutils"
brew "bitwarden-cli"
brew "lz4"
brew "boost"
brew "pkg-config"
brew "python"
brew "cairo"
brew "gnutls"
brew "harfbuzz"
brew "chromaprint"
brew "clang-format"
brew "cmake"
brew "coreutils"
brew "dbus", restart_service: true
brew "difftastic"
brew "editorconfig"
brew "enchant"
brew "exercism"
brew "exiftool"
brew "exiv2"
brew "fd"
brew "fdupes"
brew "fftw"
brew "findutils"
brew "gawk"
brew "git"
brew "git-absorb"
brew "git-lfs"
brew "glslang"
brew "pango"
brew "gnuplot"
brew "librsvg"
brew "graphviz"
brew "imagemagick"
brew "inetutils"
brew "isync"
brew "jq"
brew "latexindent"
brew "libgccjit"
brew "libolm"
brew "libvterm"
brew "mas"
brew "mercurial"
brew "meson"
brew "mp3gain"
brew "xapian"
brew "mu"
brew "ncurses"
brew "opencv"
brew "p7zip"
brew "pandoc"
brew "poppler"
brew "pdf2svg"
brew "pinentry-mac"
brew "pngpaste"
brew "ripgrep"
brew "roswell"
brew "rsync"
brew "shellcheck"
brew "subversion"
brew "tectonic"
brew "terminal-notifier"
brew "tree"
brew "vulkan-headers"
brew "wget"
brew "wordnet"
brew "youtube-dl"
brew "d12frosted/emacs-plus/emacs-plus@28", args: ["with-dbus", "with-elrumo1-icon", "with-native-comp", "with-no-frame-refocus", "with-xwidgets"]
cask "appcleaner"
cask "balenaetcher"
cask "chromium"
cask "cyberduck"
cask "discord"
cask "docker"
cask "element"
cask "firefox"
cask "firefox-developer-edition"
cask "font-alegreya"
cask "font-fira-code-nerd-font"
cask "font-fontawesome"
cask "font-ibm-plex-mono"
cask "font-jetbrains-mono"
cask "font-jetbrains-mono-nerd-font"
cask "font-jsmath-cmbx10"
cask "font-juliamono"
cask "font-overpass"
cask "grandperspective"
cask "hammerspoon"
cask "karabiner-elements"
cask "mactex-no-gui"
cask "mysides"
cask "nextcloud"
cask "openemu"
cask "rar"
cask "stats"
cask "temurin"
cask "tex-live-utility"
cask "transmission"
cask "vlc"
cask "xquartz"
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
