# Export environment variables.
export GPG_TTY=$TTY
export GNUPGHOME="$XDG_DATA_HOME"/gnupg

# Make emacs the default editor
export EDITOR="emacsclient"
# Don't clear the screen after quitting a manual page
export MANPAGER="less -X"
export LESS="--ignore-case --quit-if-one-screen --quit-on-intr FRXQ"
export LESSHISTFILE="$XDG_CACHE_HOME"/less/history

# Have bash ignore the .DS_Store
export FIGNORE=DS_Store

# Larger bash history (allow 32⁴ entries; default is 500)
export HISTSIZE=1048576
export HISTFILESIZE=$HISTSIZE

export PROJECTS=$HOME/projects
export SANDBOX=$HOME/sandbox
export PHD=$PROJECTS/phd

export CDPATH=.:$PROJECTS:$HOME

export GOPATH=$PROJECTS/go

# sweet, sweet emacs
export EMACS_SERVER_FILE=$XDG_CONFIG_HOME/emacs/server/server

export ANSIBLE_VAULT_PASSWORD_FILE=$HOME/.config/ansible-vault-pass
export ANSIBLE_HOME=$HOME/.cache/ansible
export ANSIBLE_LOCAL_TMP=$HOME/.cache/ansible/tmp
export ANSIBLE_REMOTE_TMP="~/.cache/ansible/"
export ANSIBLE_SSH_CONTROL_PATH_DIR=$HOME/.cache/ansible/cp

# if we're not on our home machine, then set tramp
if [[ $HOME != /Users/$USER ]]; then
    export EMACSCLIENT_TRAMP="/ssh:$(hostname -f 2>/dev/null || hostname):"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    # Wonky(?) nano vm implementation on darwin, more info:
    # https://stackoverflow.com/questions/64126942/malloc-nano-zone-abandoned-due-to-inability-to-preallocate-reserved-vm-space
    export MallocNanoZone=0
fi

##
# Paths, environment variables
##

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

# update path to ensure we grab our preferred executables
hash -r

user_python_path=$(python3 -c 'import site; print(site.USER_SITE.replace("lib/python/site-packages", "bin"))' 2>/dev/null)
path=(
    $XDG_CONFIG_HOME/emacs/bin
    ~/.local/bin
    $user_python_path
    $path
)

# python
export PYTHONSTARTUP="$HOME/.config/python/pythonrc"

# virtualenv
export WORKON_HOME="$XDG_DATA_HOME/virtualenvs"

# homebrew
[[ -f /opt/homebrew/bin/brew ]] &&  eval "$(/opt/homebrew/bin/brew shellenv)"

# completions
fpath+=$XDG_DATA_HOME/zsh-completions/src

# homebrew
export HOMEBREW_NO_AUTO_UPDATE=1

# zshz / auto jump
export _Z_DATA="$XDG_DATA_HOME/z"
export ZSHZ_TILDE=1
export ZSHZ_KEEP_DIRS=1

# npm
export NPM_CONFIG_USERCONFIG="$HOME/.config/npm/npmrc"

# ripgrep
export RIPGREP_CONFIG_PATH=$XDG_CONFIG_HOME/ripgrep

# wget
export WGETRC="$XDG_CONFIG_HOME/wgetrc"

# vim
export VIMINIT='let $MYVIMRC="$XDG_CONFIG_HOME/vim/vimrc" | source $MYVIMRC'

# autosuggest
export ZSH_AUTOSUGGEST_STRATEGY=(history completion)

# leiningen
export LEIN_HOME="$XDG_DATA_HOME/lein"

# vcpkg
export VCPKG_ROOT="$XDG_CACHE_HOME/vcpkg"
export VCPKG_DISABLE_METRICS=1

# X11
export XAUTHORITY="$XDG_STATE_HOME"/Xauthority
