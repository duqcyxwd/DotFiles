#   __  ____     __  ______ _____ _    _ _____   _____
#  |  \/  \ \   / / |___  // ____| |  | |  __ \ / ____|
#  | \  / |\ \_/ /     / /| (___ | |__| | |__) | |
#  | |\/| | \   /     / /  \___ \|  __  |  _  /| |
#  | |  | |  | |     / /__ ____) | |  | | | \ \| |____
#  |_|  |_|  |_|    /_____|_____/|_|  |_|_|  \_\\_____|
#
#  My .zshrc to save some coffee time and keep my hair on my head


# SECTION: : SCRIPT LOADING VARIABLE {{{1
# --------------------------------------------------------------------------
export P10K_INSTANT_PROMOT=1
export ZPROF_TRACK=0

export ZSH_CONFIG_HOME=$HOME/.config/zsh

PATH=$ZSH_CONFIG_HOME/commands:$PATH
PATH=$ZSH_CONFIG_HOME/functions:$PATH

FPATH=$FPATH:$ZSH_CONFIG_HOME/functions:$ZSH_CONFIG_HOME/completions
autoload -Uz $ZSH_CONFIG_HOME/functions/*(:t)

mlog "$(date) : zshrc start loading"


# export VS_TERM = 0         # ENV to determine running env
# SECTION: : P10K INSTANT PROMPT {{{1
[[ $P10K_INSTANT_PROMOT -eq "1" ]] && (){ # instant prompt
  if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    [[ $VS_TERM -ne "1" ]] && source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
  fi
}

# SECTION: : PATH & VARIABLE {{{1
# --------------------------------------------------------------------------
{
  # PATH: Global PATH {{{2
  # --------------------------------------------------------------------------
  export PATH="$HOME/bin:/usr/local/sbin:/usr/local/bin:$PATH"
  export PATH="$HOME/.gem/ruby/2.7.0/bin:$PATH"
  export PATH="$HOME/.SpaceVim/bin:$PATH"
  export PATH="$HOME/script:$PATH"
  export PATH="$HOME/my_script:$PATH"
  export PATH="$HOME/my_script/zsh:$PATH"
  export PATH="/usr/local/heroku/bin:$PATH"
  export PATH="/usr/local/opt/ruby/bin:$PATH"
  export PATH="./node_modules/.bin:$PATH"

  export KAFKA_HOME=/usr/local/kafka-2.1.0
  # export KAFKA_HOME=/usr/local/Cellar/kafka/2.7.0/libexec   #Homebrew installed
  export KAFKA_CONFIG=$KAFKA_HOME/config
  export PATH="$KAFKA_HOME/bin:$PATH"

  # /Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home
  export JAVA_HOME=$(/usr/libexec/java_home)
  export PATH=$PATH:$JAVA_HOME/bin:$DSE_BIN

  export GOPATH=$HOME/go
  export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

  export KUBECONFIG=$KUBECONFIG:$HOME/.kube/config

  export NODE_PATH=/usr/lib/node_modules


  # PATH: Global Parameter {{{2
  # --------------------------------------------------------------------------

  # Path to your oh-my-zsh installation.
  export ZSH=$HOME/.oh-my-zsh
  export TERM="xterm-256color"
  export NVIM_LISTEN_ADDRESS=/tmp/spacevim_nvim_server

  [[ $ZPROF_TRACK -eq "1" ]] && zmodload zsh/zprof
# }}}
}

# SECTION: : Zinit {{{1
zinit_load() {
  # https://zdharma.github.io/zinit/wiki/INTRODUCTION/
  source ~/.zinit/bin/zinit.zsh

  autoload -Uz _zinit
  (( ${+_comps} )) && _comps[zinit]=_zinit

  # sudo chmod 777 /private/tmp
  # sudo chmod +t /private/tmp
  # This solves problem in Catalina

  # autoload -Uz compinit; compinit # zinit 用户这里可能是 zpcompinit; zpcdreplay
  # Test if we still need this
  # zpcompinit; zpcdreplay

  zinit ice wait atload'ac_my_colors' silent;
  zinit light shayneholmes/zsh-iterm2colors

  # Test it
  zinit load larkery/zsh-histdb

  # Load p10k
  zinit ice depth=1 ; zinit light romkatv/powerlevel10k
  # To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
  [[ ! -f $ZDOTDIR/.p10k.zsh ]] || source $ZDOTDIR/.p10k.zsh

  # No turbo mode
  # zsh-vi-mode: Fast not need
  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
  zinit light-mode for \
    jeffreytse/zsh-vi-mode

  # NOTE: fzf-tab needs to be loaded after compinit, but before plugins which will wrap widgets,
  # such as zsh-autosuggestions or fast-syntax-highlighting!!

  zinit wait lucid light-mode for \
    zdharma/history-search-multi-word \
    atinit"zicompinit; zicdreplay"  zdharma/fast-syntax-highlighting \
    Aloxaf/fzf-tab \
    atload"_zsh_autosuggest_start"  zsh-users/zsh-autosuggestions \

  zinit wait silent light-mode for \
    OMZ::lib/functions.zsh \
    OMZ::plugins/git/git.plugin.zsh \
    OMZ::plugins/git-extras/git-extras.plugin.zsh \
    OMZ::plugins/systemd/systemd.plugin.zsh \
    OMZ::plugins/iterm2/iterm2.plugin.zsh \
    OMZ::plugins/brew/brew.plugin.zsh


  zinit wait lucid silent light-mode for \
    as'completion' is-snippet 'https://github.com/Valodim/zsh-curl-completion/blob/master/_curl' \
    blockf atpull'zinit creinstall -q .'  zsh-users/zsh-completions \
    vim/vim \
    psprint/zsh-cmd-architect \
    b4b4r07/enhancd \
    agkozak/zsh-z \
    djui/alias-tips \
    zpm-zsh/template

  : '
  for snippet in $ZSH_CONFIG_HOME/snippets/*.zsh; do
    mlog "snippet loading $snippet"
    # zinit update $snippet
    zinit ice wait silent;
    zinit snippet $snippet
  done
  unset snippet
  # '

  # The last plugin to load need overwrite alias and keybinding
  # check loading order by zinit time (-m)
  # bindkey.zsh and dir-completion are lazy loading
  # zinit ice wait atload'bindkey.zsh && dir-completion' silent;
  zinit ice wait="0" atload'source-all-snippets && bindkey.zsh && dir-completion' silent;
  zinit light zpm-zsh/empty


  # Some Special Plugins below
  # 1. Plugins need to be loaded after config
  # 2. Plugins takes with long loading time
  zinit wait lucid silent light-mode for \
    wfxr/forgit

  zinit wait silent light-mode for \
    OMZ::plugins/kubectl/kubectl.plugin.zsh

  # zinit snippet 'https://github.com/Dabz/kafka-zsh-completions/blob/master/kafka.plugin.zsh'

  # zinit update # Update plugin or snippet
  # zinit self-update updates zinit
}

# SECTION: : zsh_plugins_config {{{1
zsh_plugins_config() {

  # vi-mode
  export KEYTIMEOUT=1

  # b4b4r07/enhancd
  ENHANCD_HYPHEN_NUM=100
  ENHANCD_DISABLE_HYPHEN=1;

  # agkozak/zsh-z
  ZSHZ_CASE=ignore

  # zsh-vi-mode
  # The plugin will auto execute this zvm_after_init function
  # ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BLINKING_BLOCK
  ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BLOCK

  function zvm_after_init() {
    # zsh_cust_bindkey
    # ac_my_colors
  }

  # zsh-histdb
  _zsh_autosuggest_strategy_histdb_top_here() {
    local query="select commands.argv from history left join commands on history.command_id = commands.rowid \
      left join places on history.place_id = places.rowid where places.dir LIKE '$(sql_escape $PWD)%' \
      and commands.argv LIKE '$(sql_escape $1)%' group by commands.argv order by count(*) desc limit 1"
    suggestion=$(_histdb_query "$query")
  }

  _zsh_autosuggest_strategy_histdb_top() {
      local query="select commands.argv from
  history left join commands on history.command_id = commands.rowid
  left join places on history.place_id = places.rowid
  where commands.argv LIKE '$(sql_escape $1)%'
  group by commands.argv
  order by places.dir != '$(sql_escape $PWD)', count(*) desc limit 1"
      suggestion=$(_histdb_query "$query")
  }


  # zsh-users/zsh-autosuggestions

  # ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion)
  # ZSH_AUTOSUGGEST_STRATEGY=(histdb_top_here completion)
  # ZSH_AUTOSUGGEST_STRATEGY=(histdb_top)
}

# }}}1

zinit_load
zsh_plugins_config

# SECTION: : ZSH zstyle {{{1
{ # zstyle
  # https://superuser.com/questions/1092033/how-can-i-make-zsh-tab-completion-fix-capitalization-errors-for-directorys-and
  # ZSH case insensitive completion
  zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

  zstyle ':fzf-tab:*' prefix '- '
  zstyle ':fzf-tab:*' switch-group '[' ','
  zstyle ':fzf-tab:*' show-group full
  zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
  zstyle ':completion:*:descriptions' format '[%d]'
  # zstyle ":completion:*:descriptions" format "---- %d ----"

  zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
  zstyle ':fzf-tab:complete:__enhancd::cd:*' fzf-preview 'exa -1 --color=always $realpath'
}

# due to a bug in fzf-tab, bell is always, triggered https://github.com/Aloxaf/fzf-tab/issues/187
# TODO: this should be removed when the aforementioned bug is fixed
# unsetopt BEEP
unsetopt LIST_BEEP

# SECTION: : After Load {{{1
# --------------------------------------------------------------------------

[[ $ZPROF_TRACK -eq "1" ]] && zprof # bottom of .zshrc

# }}}


[ -f ~/.zshrc-local.sh ] && source ~/.zshrc-local.sh

mlog "zshrc loaded"
