# Create a new directory and enter it
unalias md 2>/dev/null
md() {
  mkdir -p "$@" && cd "$@" || return
}

# Easier navigation: .., ..., etc.
unalias .. 2>/dev/null
..() {
  cd "../$*" || return
}

_..() {
  [[ ${COMP_WORDS[COMP_CWORD]} == ..* ]] || COMP_WORDS[COMP_CWORD]="../${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=( "$(compgen -S '/' -d "${COMP_WORDS[COMP_CWORD]}" -- "${COMP_WORDS[COMP_CWORD]}" | cut -b4- )" )
}
complete -o nospace -F _.. ..

unalias ... 2>/dev/null
...() {
  cd "../../$*" || return
}

_...() {
  [[ ${COMP_WORDS[COMP_CWORD]} == ../..* ]] || COMP_WORDS[COMP_CWORD]="../../${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=( "$(compgen -S '/' -d "${COMP_WORDS[COMP_CWORD]}" -- "${COMP_WORDS[COMP_CWORD]}" | cut -b7- )" )
}
complete -o nospace -F _... ...

unalias .... 2>/dev/null
....() {
  cd "../../../$*" || return
}

_....() {
  [[ ${COMP_WORDS[COMP_CWORD]} == ../../..* ]] || COMP_WORDS[COMP_CWORD]="../../../${COMP_WORDS[COMP_CWORD]}"
  COMPREPLY=( "$(compgen -S '/' -d "${COMP_WORDS[COMP_CWORD]}" -- "${COMP_WORDS[COMP_CWORD]}" | cut -b9- )" )
}
complete -o nospace -F _.... ....

##################
# helper functions
##################

pg() {
  [[ -z "$*" ]] && ps aux && return
  # shellcheck disable=SC2046
  ps wup $(pgrep -a -f -i "$@")
}

x11docker() {
  xhost +

  docker run -e DISPLAY=host.docker.internal:0 "$@"

  xhost -
}
