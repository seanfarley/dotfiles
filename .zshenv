if [ -n "${ZSH_VERSION-}" ]; then
  : ${ZDOTDIR:=~}
  setopt no_global_rcs
  [[ -o no_interactive ]] && return
  setopt no_rcs
  HISTSIZE=1000000000
  SAVEHIST=1000000000
  HISTFILE=$ZDOTDIR/.zsh_history.${(%):-%m}
fi

setopt rcs
