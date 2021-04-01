ZSH_TMUX_ITERM2=true
ZSH_TMUX_AUTOCONNECT=true

# Tmuxinator
export EDITOR='nvim'

alias tmuxt='unset ZSH_PLUGIN_LOADED && /usr/local/bin/tmux'
alias tca='tmux -CC attach -t'
alias tcad='tmux -CC attach -d -t' #Detach other client
alias tcs='tmux -CC new-session -s'

# TODO
# source ~/script/tmuxinator/completion/tmuxinator.zsh        #tmuxinator
