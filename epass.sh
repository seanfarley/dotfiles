#!/usr/bin/env bash

# if a username is supplied, then add :user to the auth-source call
USERNAME=""
[[ $2 != "" ]] && USERNAME=":user \"$2\""

CMD="
(funcall
  (plist-get
    (nth 0 (auth-source-search :host \"$1\"
                               $USERNAME
                               :max 1)) :secret))"

pass=$(emacsclient --eval "$CMD")

# remove surrounding quotes
pass="${pass%\"}"
pass="${pass#\"}"

if [[ "$pass" == "nil" ]]; then
    exit 155
fi

echo "$pass"
