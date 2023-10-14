# ZSH_TMUX_ITERM2=true
# ZSH_TMUX_AUTOCONNECT=true

# Tmuxinator
export EDITOR='nvim'

alias tmuxt='unset ZSH_PLUGIN_LOADED && /usr/local/bin/tmux'

alias ta='tmux attach'
alias tat='tmux attach -t'
alias tk='tmux kill-session -t'
alias tki="for s in \$(tmux list-sessions | awk '{print \$1}' | rg ':' -r '' | fzf); do tmux kill-session -t \$s; done;"

# TODO
# source ~/script/tmuxinator/completion/tmuxinator.zsh        #tmuxinator
