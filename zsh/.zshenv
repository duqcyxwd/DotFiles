export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local
export XDG_STATE_HOME=$HOME/.local/state
export XDG_CACHE_HOME=$HOME/.cache

export ZDOTDIR=$HOME/.config/zsh

# Overwrite by /etc/zshrc
export HIST_STAMPS="yyyy-mm-dd" # ZSH History time format
export HISTSIZE=10000002          #The maximum number of events stored in the internal history list.
export SAVEHIST=10000002          #The maximum number of history events to save in the history file.

HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history

export PATH=$ZDOTDIR/commands:$PATH
export PATH=$ZDOTDIR/commands/kubectl:$PATH

# Some env which could be used in fzf_tp
export ZSH_LOADING_LOG=$XDG_CACHE_HOME/.startup.log
export RUNCACHED_CACHE_DIR=$XDG_CACHE_HOME/.runcached

export RUNCACHED_MAX_AGE=1800
export RUNCACHED_IGNORE_ENV=1
export RUNCACHED_IGNORE_PWD=1

