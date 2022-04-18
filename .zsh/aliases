# Define aliases.
alias tree='tree -a -I .git'
alias -- -="cd -"
alias sudo="sudo "

# IP addresses
alias wanip="curl -s -4 ifconfig.co"
alias wanip6="curl -s -6 ifconfig.co"
alias lanips="ifconfig -a | grep -E '([0-9]{1,3}[\.]){3}[0-9]{1,3}'"
alias lanip="lanips | grep -v 127.0.0.1"

# Shortcuts
alias e="emacsclient -n"
alias s="perl -pi -e"
alias l="less"
alias g='grep -i'

# Add flags to existing aliases.
# alias ls="${aliases[ls]:-ls} -A"

if command -v fdfind >/dev/null 2>&1; then
    alias fd="fdfind"
fi

if (( $+commands[dircolors] )); then  # proxy for GNU coreutils vs BSD
    # Don't define aliases for commands that point to busybox.
    [[ ${${:-diff}:c:A:t} == busybox* ]] || alias diff='diff --color=auto'
    [[ ${${:-ls}:c:A:t}   == busybox* ]] || alias ls='ls --color=auto'
else
    [[ ${${:-ls}:c:A:t}   == busybox* ]] || alias ls='ls -G'
fi
[[ ${${:-grep}:c:A:t}   == busybox* ]] || alias grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
