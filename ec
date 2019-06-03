#!/usr/bin/env bash

set -o errexit
set -o pipefail


_main() {
  if [[ "$1" == '+' ]]; then
    # Workaround iTerm semantic opening.
    _emacs "${@:2}"
  elif [[ "$1" == '-' ]]; then
    # only bring emacs to foreground forcibly when piping
    osascript -e 'tell application "Emacs" to activate' > /dev/null 2>&1 &
    # Support piping text to emacs.
    local tempfile
    tempfile="$(mktemp -t "emacs-stdin-$USER")"
    cat - > "${tempfile}"
    _emacs "${tempfile}"
  else
    _emacs "$@"
  fi
}

_emacs() {
  exec emacsclient --no-wait "$@" > /dev/null 2>&1 &
}

_main "$@"
