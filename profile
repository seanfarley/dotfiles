for file in exports bash_prompt aliases extra docker-bash; do
  file="$HOME/.$file"
  [ -e "$file" ] && source "$file"
done

# hard-code check for rsync
for file in {/opt/,$HOME/.}local/{share/bash-completion/completions/rsync,etc/profile.d/{autojump,bash_completion}.sh} $HOME/.functions /usr/share/autojump/autojump.sh; do
  [ -e "$file" ] && source "$file"
done

# Only run these if running interactively
if [ -n "$PS1" ]; then

  # make sure multiplexing directory exists
  [ -d $HOME/.ssh/connections ] || mkdir $HOME/.ssh/connections

  # load keys from keychain
  if [[ -f /usr/bin/security ]]; then
    ssh-add -K &> /dev/null
    export SSH_KEY="$(/usr/bin/security find-generic-password -w -a /Users/sean/.ssh/id_rsa | openssl rsa -in ~/.ssh/id_rsa -passin stdin 2> /dev/null)"
  fi

  [[ BASH_VERSINFO -ge 4 ]] && shopt -s globstar

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

  # bashcompletion for chg
  complete -o bashdefault -o default -o nospace -F _hg chg \
      || complete -o default -o nospace -F _hg chg

  # bashcompletion for lhg
  complete -o bashdefault -o default -o nospace -F _hg lhg \
      || complete -o default -o nospace -F _hg lhg

  [[ -f $HOME/.npmrc ]] && export NPMRC="$(cat ~/.npmrc)"

fi

if [[ -z "$SSH_CLIENT" && -d /cygdrive/c ]]; then
  SSHAGENT=/usr/bin/ssh-agent
  SSHAGENTARGS="-s"
  if [ -z "$SSH_AUTH_SOCK" -a -x "$SSHAGENT" ]; then
    eval `$SSHAGENT $SSHAGENTARGS`
    trap "kill $SSH_AGENT_PID" 0
  fi
fi
