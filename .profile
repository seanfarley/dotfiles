
for file in exports bash_prompt aliases extra; do
  file="$HOME/.$file"
  [ -e "$file" ] && source "$file"
done

for file in {/opt/,$HOME/.}local/etc/profile.d/bash_completion.sh $HOME/.{bashrc,functions}; do
  [ -e "$file" ] && source "$file"
done

[[ BASH_VERSINFO -ge 4 ]] && shopt -s globstar

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# Don't save commands that start with a space
HISTCONTROL=ignorespace

# Get ctrl-s to work in bash searching
[ -t 0 ] && stty stop undef
