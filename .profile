
for file in exports bash_prompt aliases extra; do
  file="$HOME/.$file"
  [ -e "$file" ] && source "$file"
done

for file in /opt/local/etc/bash_completion; do
  [ -e "$file" ] && source "$file"
done

for file in local/etc/profile.d/bash_completion.sh bashrc functions; do
  file="$HOME/.$file"
  [ -e "$file" ] && source "$file"
done

[[ BASH_VERSINFO -ge 4 ]] && shopt -s globstar

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# Don't save commands that start with a space
HISTCONTROL=ignorespace

# Get ctrl-s to work in bash searching
stty stop undef
