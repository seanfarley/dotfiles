for file in exports bash_prompt aliases extra; do
  file="$HOME/.$file"
  [ -e "$file" ] && source "$file"
done

for file in {/opt/,$HOME/.}local/etc/profile.d/bash_completion.sh $HOME/.functions; do
  [ -e "$file" ] && source "$file"
done

# Only run these if running interactively
if [ -n "$PS1" ]; then

  # make sure multiplexing directory exists
  [ -d $HOME/.ssh/connections ] || mkdir $HOME/.ssh/connections

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
  stty stop undef

  # bashcompletion for chg
  complete -o bashdefault -o default -o nospace -F _hg chg \
      || complete -o default -o nospace -F _hg chg

  # bashcompletion for lhg
  complete -o bashdefault -o default -o nospace -F _hg lhg \
      || complete -o default -o nospace -F _hg lhg

fi
