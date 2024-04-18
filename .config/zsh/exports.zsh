# Export environment variables.
export GPG_TTY="$TTY"
export GNUPGHOME="$XDG_DATA_HOME/gnupg"

# Make emacs the default editor
export EDITOR="emacsclient"
# Don't clear the screen after quitting a manual page
export MANPAGER="less -X"
export LESS="--ignore-case --quit-if-one-screen --quit-on-intr FRXQ"
export LESSHISTFILE="$XDG_CACHE_HOME/less/history"

# Have bash ignore the .DS_Store
export FIGNORE=DS_Store

# Larger bash history (allow 32⁴ entries; default is 500)
export HISTSIZE=1048576
export HISTFILESIZE="$HISTSIZE"

export PROJECTS="$HOME/projects"
export SANDBOX="$HOME/sandbox"
export PHD="$PROJECTS/phd"

export CDPATH=".:$PROJECTS:$HOME"

export GOPATH="$PROJECTS/go"

# sweet, sweet emacs
export EMACS_SERVER_FILE="$XDG_CONFIG_HOME/emacs/server/server"

export ANSIBLE_VAULT_PASSWORD_FILE="$XDG_CONFIG_HOME/ansible-vault-pass"
export ANSIBLE_HOME="$XDG_CACHE_HOME/ansible"
export ANSIBLE_LOCAL_TMP="$XDG_CACHE_HOME/ansible/tmp"
export ANSIBLE_SSH_CONTROL_PATH_DIR="$XDG_CACHE_HOME/ansible/cp"

# if we're not on our home machine, then set tramp
if [[ $HOME != /Users/$USER ]]; then
    export EMACSCLIENT_TRAMP="/ssh:$(hostname -f 2>/dev/null || hostname):"
fi

# system ssh command to use with our custom ssh function
export SSH="$(sh -c 'command -v ssh')"

##
# Paths, environment variables
##

# set path to be an array as well as unique
typeset -aU path

# make clean tarballs
export COPYFILE_DISABLE=true

path=(
    /usr/local/bin
    /usr/local/sbin
    /usr/local/cuda/bin
    /usr/sbin
    /sbin
    /Library/TeX/texbin
    $path
)

# homebrew
[[ -f /usr/local/bin/brew ]] &&  eval "$(/usr/local/bin/brew shellenv)"
[[ -f /opt/homebrew/bin/brew ]] &&  eval "$(/opt/homebrew/bin/brew shellenv)"

path=(
    "$HOMEBREW_PREFIX/opt/man-db/libexec/bin"
    $path
)

# update path to ensure we grab our preferred executables
hash -r

user_python_path=$(python3 -c 'import site; print(site.USER_SITE.replace("lib/python/site-packages", "bin"))' 2>/dev/null)
path=(
    "$XDG_CONFIG_HOME/emacs/bin"
    "$HOME/.local/bin"
    "$XDG_DATA_HOME/cargo/bin"
    "$user_python_path"
    $path
)

# python
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"

# virtualenv
export WORKON_HOME="$XDG_DATA_HOME/virtualenvs"

# completions
fpath+="$XDG_DATA_HOME/zsh-completions/src"

if type brew &>/dev/null; then
  fpath+="$(brew --prefix)/share/zsh-completions"
  fpath+="$(brew --prefix)/share/zsh/site-functions"
fi

# homebrew
export HOMEBREW_NO_AUTO_UPDATE=1

# zshz / auto jump
export _Z_DATA="$XDG_DATA_HOME/z"
export ZSHZ_TILDE=1
export ZSHZ_KEEP_DIRS=1

# npm
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"

# ripgrep
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep"

# vim
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'

# autosuggest
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# leiningen
export LEIN_HOME="$XDG_DATA_HOME/lein"

# vcpkg
export VCPKG_ROOT="$XDG_CACHE_HOME/vcpkg"
export VCPKG_DISABLE_METRICS=1

# cargo
export CARGO_HOME="$XDG_DATA_HOME/cargo"

# ruff
export RUFF_CACHE_DIR="$XDG_CACHE_HOME/ruff"

# lsp
export LSP_USE_PLISTS=true

# X11
# NOTE Unless the server supports PermitUserEnviornments then ssh will alway
# dump .Xauthority into the home directory
# NOTE XQuartz also blindly dumps into the home directory
# XAUTHORITY="$XDG_STATE_HOME"/Xauthority
