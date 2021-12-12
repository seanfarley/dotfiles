for file in exports bash_prompt aliases extra docker-bash; do
  file="$HOME/.$file"
  # shellcheck disable=SC1090
  [ -e "$file" ] && source "$file"
done

# hard-code check for rsync
for file in {/usr/,$HOME/.}local/share/bash-completion/bash_completion $HOME/.functions; do
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

  # write history after a command has executed; not at the end of a session
  export PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND ;} history -a"

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
