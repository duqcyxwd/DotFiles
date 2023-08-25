export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local
export XDG_STATE_HOME=$HOME/.local/state
export XDG_CACHE_HOME=$HOME/.cache

export ZDOTDIR=$HOME/.config/zsh

declare -A ZINIT
ZINIT[ZCOMPDUMP_PATH]="$XDG_CACHE_HOME/zsh/.zcompdump"

# Overwrite by /etc/zshrc
export HIST_STAMPS="yyyy-mm-dd" # ZSH History time format
export HISTSIZE=10000002          #The maximum number of events stored in the internal history list.
export SAVEHIST=10000002          #The maximum number of history events to save in the history file.


case `uname` in
  Darwin)
    # commands for OS X go here
    HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
    ;;
  Linux)
    # commands for Linux go here
    HISTFILE=$HOME/.zsh_history
    ;;
esac

export PATH="$ZDOTDIR/bin:$PATH"

# Some env which could be used in fzf_tp
export ZSH_LOADING_LOG=$XDG_CACHE_HOME/.startup.log
export RUNCACHED_CACHE_DIR=$XDG_CACHE_HOME/.runcached

# Put function in FAPTH so it is faster to run as a script
# Add autoload so I don't need to put them into PATH
FPATH=$FPATH:$ZDOTDIR/autoload/
autoload -Uz $ZDOTDIR/autoload/*(:t)

FPATH=$FPATH:$ZDOTDIR/autoload/kube/
autoload -Uz $ZDOTDIR/autoload/kube/*(:t)

# Add completions
FPATH=$FPATH:$ZDOTDIR/completions

export KUBECONFIG=$HOME/.kube/config

export HOMEBREW_NO_AUTO_UPDATE=0
function brew2() {
    HOMEBREW_NO_AUTO_UPDATE=1 brew "$@" && brew update
}
