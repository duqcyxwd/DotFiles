
export ZDOTDIR=$HOME/.config/zsh
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local
export XDG_STATE_HOME=$HOME/.local/state
export XDG_CACHE_HOME=$HOME/.cache

# Overwrite by /etc/zshrc
export HISTSIZE=10000002          #The maximum number of events stored in the internal history list.
export SAVEHIST=10000002        #The maximum number of history events to save in the history file.

# Not working, Overwrite by /ect/zshrc
# export HISTFILE=/Users/EYONDUU/.zsh_history_bk/.history

# . "$HOME/.cargo/env"
# env
# echo $NVIM_LISTEN_ADDRESS


export ZSH_CONFIG_HOME=$HOME/.config/zsh
export PATH=$ZSH_CONFIG_HOME/commands:$PATH
export PATH=$ZSH_CONFIG_HOME/commands/kubectl:$PATH

# Some env which could be used in fzf_tp
export ZSH_LOADING_LOG=$XDG_CACHE_HOME/.startup.log
export RUNCACHED_CACHE_DIR=$XDG_CACHE_HOME/.runcached
export RUNCACHED_MAX_AGE=500

