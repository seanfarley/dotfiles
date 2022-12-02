# Define functions and completions.
function md() { [[ $# == 1 ]] && mkdir -p -- "$1" && cd -- "$1" }
compdef _directories md

zle -N backward-kill-space-word
backward-kill-space-word() {
  zle -f kill
  WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>' zle .backward-kill-word
}

function sync-history () {
  # This is a function because it needs to access HISTFILE.
  # https://github.com/romkatv/dotfiles-public/blob/2d81e586677f49a07a4992fb3199ce1890de6e42/dotfiles/functions/sync-dotfiles

  emulate -L zsh -o no_unset -o no_prompt_subst -o prompt_percent -o pushd_silent

  local GIT_DIR GIT_WORK_TREE
  unset GIT_DIR GIT_WORK_TREE

  local merge=1 OPTIND OPTARG
  while getopts ":hm" opt; do
    case $opt in
      *h)
        print -r -- $'Usage: sync-dotfiles [{+|-}m]\nSynchronize local dotfiles with GitHub.'
        return 0
        ;;
      \?) print -r -- "sync-dotfiles: invalid option: $OPTARG" >&2;            return 1;;
      :)  print -r -- "sync-dotfiles: missing required argument: $OPTARG" >&2; return 1;;
      m)  merge=0;;
      +m) merge=1;;
    esac
  done

  if (( OPTIND <= ARGC )); then
  print -r -- "sync-dotfiles: unexpected positional argument: ${*[OPTIND]}" >&2
  return 1
  fi

  function -sync-dotfiles-repo() {
    local repo=$(basename $ZDOTDIR_PRIVATE) dirty=0 s
    s="$(git status --porcelain --untracked-files=no)" || return
    if [[ -n $s ]]; then
      dirty=1
      git stash || return
    fi

    print -Pr -- "%F{yellow}sync-dotfiles%f: pulling %B$repo%b" >&2
    if ! git pull --rebase && ! git pull --no-edit; then
      print -Pr -- "%F{red}sync-dotfiles%f: failed to pull %B$repo%b" >&2
      git status || return
      return 1
    fi

    if (( merge )) && git remote get-url origin &>/dev/null; then
      print -Pr -- "%F{yellow}sync-dotfiles%f: merging origin %B$repo%b" >&2
      git fetch origin || return
      if ! git merge --no-edit origin/main; then
        print -Pr -- "%F{red}sync-dotfiles%f: failed to merge origin %B$repo%b" >&2
        git status || return
        return 1
      fi
    fi

    print -Pr -- "%F{yellow}sync-dotfiles%f: pushing %B$repo%b" >&2
    git push || return
    if (( dirty )); then
      git stash pop || return
    fi
  }

  {
    pushd -q "$ZDOTDIR_PRIVATE" || return

    local hist="$HISTFILE"
    local -U hist=($hist{,:*}(N))
    [[ -f ${HISTFILE:-} ]] && hist+=($HISTFILE)
    if (( $#hist )); then
      pwd
      git add -- $hist || return
      local s
      s="$(git status --porcelain -- $hist)" || return
      if [[ -n $s ]]; then
        git commit -m "bump ${(%):-%m} history" -- $hist || return
      fi
    fi
    -sync-dotfiles-repo || return
  } always {
    unset -f -- -sync-dotfiles-repo
    popd -q
  }
}

zle -N clear-to-bottom
clear-to-bottom () {
    [[ -w $TTY ]] || return

    builtin echoti civis >&$TTY
    builtin print -rn -- "${(pl:$((2 * LINES - 1))::\n:)}" > $TTY
    builtin zle -I
    builtin zle -R
    builtin echoti cnorm >&$TTY
}

backup-home () {
  # need to change to home so that the exclude paths match
  cd "$HOME"
  echo "Don't forget to grant full-disk access to emacs, terminal, rsync, etc"
  echo "https://apple.stackexchange.com/questions/375383/rsync-in-cron-on-catalina-no-longer-working"
  echo "Backing up to $1"
  rsync -avlPogAX --delete \
    --exclude Library/Caches/ \
    --exclude Library/Containers \
    --exclude Library/Developer \
    --exclude Library/Python \
    --exclude Library/VirtualBox \
    --exclude Library/VoiceTrigger \
    --exclude "Library/Saved Application State" \
    $HOME/ $1
  cd -
}

defaults-read-filter () {
  defaults read | \
    grep -v _DKThrottledActivityLast_ | \
    grep -v NSPreferencesContentSize | \
    grep -v NSPreferencesSelectedIndex | \
    grep -v "NSWindow Frame Preferences" | \
    grep -v SPMessageTracingWindow | \
    grep -v "Last Metrics Upload Date" | \
    grep -v currencyCacheRefreshDate | \
    grep -v CKStartupTime | \
    grep -v HashManager-Last | \
    grep -v "Age = " | \
    grep -v com.apple.geo.analytics | \
    grep -v engagementCount-com.apple.Spotlight | \
    grep -v "startTime = " | \
    grep -v "NSStatusItem .* Item" | \
    grep -v "Last .* Date" | \
    grep -v currencyCacheRefreshDate | \
    grep -v CKStartupTime | \
    grep -v com.apple.geo.analytics | \
    grep -v "NSWindow Frame Main Window Frame SystemPreferencesApp"
}
