# Easier navigation
alias -- -="cd -"

# keep aliases for sudo
alias sudo="sudo "

# IP addresses
alias wanip="curl -s -4 ifconfig.co"
alias wanip6="curl -s -6 ifconfig.co"
alias localips="ifconfig -a | grep -E '([0-9]{1,3}[\.]){3}[0-9]{1,3}'"
alias localip="localips | grep -v 127.0.0.1"

# Shortcuts
alias e="emacsclient -n"
alias s="perl -pi -e"
alias l="less"

[ -n "$(ls --version 2>/dev/null)" ] && alias ls='ls --color=auto'

alias g='grep -i'