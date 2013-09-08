
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

  # check the window size after each command and, if necessary,
  # update the values of LINES and COLUMNS.
  shopt -s checkwinsize

  # Get ctrl-s to work in bash searching
  stty stop undef
fi
