zstyle ':filter-select:highlight' matched fg=yellow,standout
zstyle ':filter-select' max-lines 10 # use 10 lines for filter-select
zstyle ':filter-select' max-lines -10 # use $LINES - 10 for filter-select
zstyle ':filter-select' rotate-list yes # enable rotation for filter-select
zstyle ':filter-select' case-insensitive yes # enable case-insensitive search
zstyle ':filter-select' extended-search yes # see below
zstyle ':filter-select' hist-find-no-dups yes # ignore duplicates in history source
zstyle ':filter-select' escape-descriptions no # display literal newlines, not \n, etc

zstyle ':zaw:history' default zaw-callback-append-to-buffer

if [[ -e ~/.ssh/id_rsa ]]; then
  ssh-add -K &> /dev/null
fi

() {
  local hist
  for hist in ~/.zsh_history.*; do
    fc -RI $hist
  done
}

# Autoload functions.
autoload -Uz zmv

# Set shell options: http://zsh.sourceforge.net/Doc/Release/Options.html.
setopt glob_dots     # no special treatment for file names with a leading dot
setopt no_auto_menu  # require an extra TAB press to open the completion menu
setopt inc_append_history

# activate z
source ~/.cache/zsh-z/zsh-z.plugin.zsh

autoload -Uz compinit
compinit
zstyle ':completion:*' menu select

printf '\n%.0s' {1..100}

source ~/.p10k.zsh
source ~/.zsh/powerlevel10k/powerlevel10k.zsh-theme
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source ~/.zsh/zaw/zaw.zsh

source ~/.zsh/init
source ~/.zsh/exports
source ~/.zsh/functions
source ~/.zsh/aliases
source ~/.zsh/prompt
source ~/.zsh/bindkeys
