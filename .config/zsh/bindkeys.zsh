# https://unix.stackexchange.com/questions/373795/bindkey-to-execute-command-zsh

bindkey '^W' backward-kill-space-word
bindkey "^u" backward-kill-line

bindkey "^L" clear-to-bottom

bindkey "^R" zaw-history
bindkey -M filterselect "^[" send-break
