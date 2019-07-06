#!/usr/bin/env bash

set -o errexit
set -o pipefail


_main() {
  if [[ -f /usr/bin/osascript ]]; then
    osascript -e 'tell application "Emacs" to activate' > /dev/null 2>&1 &
  fi
  if [[ "$1" == '+' ]]; then
    # Workaround iTerm semantic opening.
    _emacs "${@:2}"
  elif [[ "$1" == '-' ]]; then
    # Support piping text to emacs.
    local tempfile
    tempfile="$(mktemp -t "emacs-stdin-$USER-XXXXXXX")"
    cat - > "${tempfile}"
    _emacs "${tempfile}"
  elif [[ "$1" =~ .*:[0-9]+ ]]; then
    IFS=':' read -ra arrFL <<< "$1"
    _emacs -e "(let ((buf (find-file \"${arrFL[0]}\"))) (goto-line ${arrFL[1]}) (select-frame-set-input-focus (window-frame (get-buffer-window buf))))"
  else
    _emacs "$@"
  fi
}

_emacs() {
  exec emacsclient --no-wait "$@" > /dev/null 2>&1 &
}

_main "$@"
