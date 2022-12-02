if [ -n "${ZSH_VERSION-}" ]; then
  # first thing to do is set xdg values if they don't exist
  export XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
  export XDG_CACHE_HOME=${XDG_CACHE_HOME:-$HOME/.cache}
  export XDG_DATA_HOME=${XDG_DATA_HOME:-$HOME/.local/share}
  export XDG_STATE_HOME=${XDG_RUNTIME_DIR:-$HOME/.local/state}
  export XDG_RUNTIME_DIR=${XDG_RUNTIME_DIR:-/var/run/$UID}

  if [[ "$OSTYPE" == darwin* ]]; then
    export XDG_DESKTOP_DIR=${XDG_DESKTOP_DIR:-$HOME/Desktop}
    export XDG_DOCUMENTS_DIR=${XDG_DOCUMENTS_DIR:-$HOME/Documents}
    export XDG_DOWNLOAD_DIR=${XDG_DOWNLOAD_DIR:-$HOME/Downloads}
    export XDG_MUSIC_DIR=${XDG_MUSIC_DIR:-$HOME/Music}
    export XDG_PICTURES_DIR=${XDG_PICTURES_DIR:-$HOME/Pictures}
    export XDG_VIDEOS_DIR=${XDG_VIDEOS_DIR:-$HOME/Videos}
    export XDG_PROJECTS_DIR=${XDG_PROJECTS_DIR:-$HOME/Projects}
  fi

  : ${ZDOTDIR:="$XDG_CONFIG_HOME/zsh"}
  export ZDOTDIR_PRIVATE="$XDG_DATA_HOME/dotfiles-private"

  setopt no_global_rcs
  [[ -o no_interactive ]] && return
  setopt no_rcs
  HISTSIZE=1000000000
  SAVEHIST=1000000000
  HISTFILE=$ZDOTDIR_PRIVATE/.zsh_history.${(%):-%m}
fi

setopt rcs
