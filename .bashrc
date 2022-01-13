[[ $- == *i* ]] && source ~/.local/share/blesh/ble.sh --noattach

export _Z_NO_PROMPT_COMMAND=1
for file in bash_exports bash_prompt bash_aliases z.sh; do
  file="$HOME/.$file"
  # shellcheck disable=SC1090
  [ -e "$file" ] && source "$file"
done

# hard-code check for rsync
for file in {/usr/,$HOME/.}local/share/bash-completion/bash_completion $HOME/.bash_functions; do
  # shellcheck disable=SC1090
  [ -e "$file" ] && source "$file"
done

# Only run these if running interactively
if [ -n "$PS1" ]; then

  # load keys from keychain
  if [[ -f /usr/bin/security ]]; then
    ssh-add -K &> /dev/null
  fi

  [[ "${BASH_VERSINFO[0]}" -ge 4 ]] && shopt -s globstar

  # Case-insensitive globbing (used in pathname expansion)
  shopt -s nocaseglob

  # don't put duplicate lines or lines starting with space in the history.
  HISTCONTROL=ignoreboth

  # append to the history file, don't overwrite it
  shopt -s histappend

  # check the window size after each command and, if necessary,
  # update the values of LINES and COLUMNS.
  shopt -s checkwinsize

  # Get ctrl-s to work in bash searching
  stty -ixon

  [[ -f $HOME/.npmrc ]] && NPMRC="$(cat ~/.npmrc)" && export NPMRC

  # apparently this is needed for gpg agent stuff to not get this error:
  # "error sending to agent: Inappropriate ioctl for device"
  GPG_TTY=$(tty)
  export GPG_TTY
fi

export PATH="$HOME/.cargo/bin:$PATH"

[[ ${BLE_VERSION-} ]] && ble-attach
