#
#
# ███╗   ███╗██╗   ██╗    ███████╗███████╗██╗  ██╗██████╗  ██████╗
# ████╗ ████║╚██╗ ██╔╝    ╚══███╔╝██╔════╝██║  ██║██╔══██╗██╔════╝
# ██╔████╔██║ ╚████╔╝       ███╔╝ ███████╗███████║██████╔╝██║
# ██║╚██╔╝██║  ╚██╔╝       ███╔╝  ╚════██║██╔══██║██╔══██╗██║
# ██║ ╚═╝ ██║   ██║       ███████╗███████║██║  ██║██║  ██║╚██████╗
# ╚═╝     ╚═╝   ╚═╝       ╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝
#
#
#  My .zshrc to save some coffee time and keep my hair on my head


# SECTION: : SCRIPT LOADING VARIABLE {{{1
# --------------------------------------------------------------------------
export P10K_INSTANT_PROMOT=1
export ZPROF_TRACK=0

# PATH=$ZDOTDIR/commands:$PATH
# for d in $ZDOTDIR/commands/*; do PATH="$PATH:$d"; done

# WIP Not sure if I still need to leave this in regular path
# I have some fucntion and I want they to be loaded as soon as possible
PATH=$ZDOTDIR/functions:$PATH

FPATH=$FPATH:$ZDOTDIR/functions:$ZDOTDIR/completions
autoload -Uz $ZDOTDIR/functions/*(:t)

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
  export PATH="$HOME/.cargo/bin:$PATH"
  export PATH="/usr/local/heroku/bin:$PATH"
  export PATH="/usr/local/opt/ruby/bin:$PATH"
  export PATH="./node_modules/.bin:$PATH"
  export PATH=/usr/local/opt/python/libexec/bin:$PATH         # Use brew install python/pip as default
  export PATH=/usr/local/opt/coreutils/bin/:$PATH             # Add gnu fns.

  # export KAFKA_HOME=/usr/local/kafka-2.1.0
  export KAFKA_HOME=/usr/local/kafka_2.12-2.5.1
  # export KAFKA_HOME=/usr/local/Cellar/kafka/2.8.0/libexec   #Homebrew installed

  export KAFKA_CONFIG=$KAFKA_HOME/config
  export PATH="$KAFKA_HOME/bin:$PATH"

  # /Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home
  export JAVA_HOME=$(/usr/libexec/java_home)
  export PATH=$PATH:$JAVA_HOME/bin:$DSE_BIN

  export GOPATH=$HOME/go
  export PATH=$PATH:$GOROOT/bin:$GOPATH/bin

  # export KUBECONFIG=$KUBECONFIG:$HOME/.kube/config
  export KUBECONFIG=$HOME/.kube/config
  # [[ ! -v KUBECONFIG ]] && export KUBECONFIG=$HOME/.kube/config

  export NODE_PATH=/usr/lib/node_modules


  # PATH: Global Parameter {{{2
  # --------------------------------------------------------------------------

  # Path to your oh-my-zsh installation.
  # 20210824 I removed it and not sure if I still need it
  # export ZSH=$HOME/.oh-my-zsh

  # Specified in .zshenv but it is overwrite by /etc/zshrc
  # Sometimes when script goes wrong, it will mess up my history. Add this to prevent issues.
  # All history script should goes to 300-history.zsh
  export HIST_STAMPS="yyyy-mm-dd" # ZSH History time format
  export HISTSIZE=10000003          #The maximum number of events stored in the internal history list.
  export SAVEHIST=10000004          #The maximum number of history events to save in the history file.

  # Never set TERM in your config
  # export TERM="xterm-256color"
  # export NVIM_LISTEN_ADDRESS=$XDG_CACHE_HOME/nvimsocket

  [[ $ZPROF_TRACK -eq "1" ]] && zmodload zsh/zprof
# }}}
}

# SECTION: : Zinit {{{1
zinit_load() {
  # https://zdharma.github.io/zinit/wiki/INTRODUCTION/
  ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local/share}/zinit/zinit.git"
  source "${ZINIT_HOME}/zinit.zsh"

  # This solves problem in Catalina
  # sudo chmod 777 /private/tmp
  # sudo chmod +t /private/tmp

  # Stage 0 Place to Try New Or Random Plugins
  # zinit load larkery/zsh-histdb

  # Stage 1 Must have plugins before prompt {{{2
  {
    # zinit ice wait atload'ac_my_colors' silent;
    # zinit light shayneholmes/zsh-iterm2colors

    # To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
    [[ ! -f $ZDOTDIR/.p10k.zsh ]] || source $ZDOTDIR/.p10k.zsh
    # Load p10k
    zinit ice depth=1 ; zinit light romkatv/powerlevel10k

    # zinit ice wait="0" atload'[[ ! -f $ZDOTDIR/.p10k.zsh ]] || source $ZDOTDIR/.p10k.zsh' silent;
    # zinit light romkatv/powerlevel10k

    ZVM_LAZY_KEYBINDINGS=false

    zinit light-mode for \
      jeffreytse/zsh-vi-mode

  }

  # Stage 2 Lazy load Plugins {{{2
  {
    # 1. Plugins need to be loaded before my config

    # NOTE: fzf-tab needs to be loaded after compinit, but before plugins which will wrap widgets,
    # such as zsh-autosuggestions or fast-syntax-highlighting!!
    # The order to load plug is important below
    zinit wait lucid light-mode for \
      Aloxaf/fzf-tab \
      unixorn/fzf-zsh-plugin \
      duqcyxwd/history-search-multi-word \
      atinit"zicompinit; zicdreplay"  duqcyxwd/fast-syntax-highlighting \
      atload"_zsh_autosuggest_start"  zsh-users/zsh-autosuggestions \


    zinit wait silent light-mode for \
      OMZ::plugins/kubectl/kubectl.plugin.zsh \
      OMZ::plugins/git/git.plugin.zsh \
      OMZ::plugins/autojump/autojump.plugin.zsh

    zinit wait lucid silent light-mode for \
      as'completion' is-snippet 'https://github.com/Valodim/zsh-curl-completion/blob/master/_curl' \
      blockf atpull'zinit creinstall -q .'  zsh-users/zsh-completions \

    zinit wait silent light-mode for \
      ~/duqcyxwd/kube-int
    }

  # Stage 3 Lazy load Personal scripts or config {{{2
  {

    # I prefer source-all-snippets over zinit snippets because it is faster
    # and I don't need to update them manually
    
    : '
    for snippet in $ZDOTDIR/snippets/*.zsh; do
      # mlog "snippet loading $snippet"
      # zinit update $snippet
      zinit ice wait=0 silent;
      zinit snippet $snippet
    done
    unset snippet
    # '

    # The last plugin to load need overwrite alias and keybinding
    # check loading order by zinit time (-m)

    zinit ice wait="0" atload'source-all-snippets && bindkey.zsh' silent;
    zinit light zpm-zsh/empty

  }

  # Stage 4 Lazy load Other scripts  {{{2
  {
    # 1. Plugins with long loading time
    # 2. Plugins need to be loaded after config
    # 3. Plugins
    # Don't use kubectl-fzf, it will disable auto complte
    # bonnefoa/kubectl-fzf \
    zinit wait="0" lucid silent light-mode for \
      agkozak/zsh-z \
      djui/alias-tips \
      wfxr/forgit \
      zpm-zsh/template \
      OMZ::plugins/brew/brew.plugin.zsh \
      OMZ::plugins/git-extras/git-extras.plugin.zsh \
      OMZ::plugins/iterm2/iterm2.plugin.zsh \
      OMZ::plugins/systemd/systemd.plugin.zsh

    # zinit ice wait="0" atload'bindkey.zsh' silent;
    # zinit light zpm-zsh/empty

  }

  #}}}

  zinit ice wait="0" atload'source ~/.zshrc-local.sh; zicompinit; zicdreplay;' silent;
  zinit light zpm-zsh/empty

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
  # ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BLOCK
  ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BEAM


  function zvm_after_init() {
    # [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
    # bindkey.zsh
    # zsh_cust_bindkey
    # ac_my_colors
  }

}

# }}}1

# Add some quick dirty useful alias so I can use them before they are loaded
alias vim='nvim'
alias vi='nvim'
alias gst='git status'
[[ $commands[exa] ]] && alias la="exa -lbFa"

zinit_load
zsh_plugins_config

# SECTION: : ZSH zstyle {{{1
{ # zstyle
  # https://superuser.com/questions/1092033/how-can-i-make-zsh-tab-completion-fix-capitalization-errors-for-directorys-and
  # ZSH case insensitive completion
  zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
  zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
  zstyle ':completion:*:descriptions' format '[%d]'

  zstyle ':fzf-tab:*' prefix '- '
  zstyle ':fzf-tab:*' switch-group '[' ','
  zstyle ':fzf-tab:*' show-group full
  zstyle ':fzf-tab:*' fzf-command ftb-tmux-popup

  zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
  zstyle ':fzf-tab:complete:cd:*' fzf-command

  zstyle ':fzf-tab:complete:__enhancd::cd:*' fzf-preview 'exa -1 --color=always $realpath'
  zstyle ':fzf-tab:complete:__enhancd::cd:*' fzf-command


  # I am trying to disable sort in fzf-tab but not working.
  # When I use zsh-z, the result is already sort by rank
  # disable sort when completing `git checkout`
  zstyle ':completion:*:z:*' sort false
  zstyle ':completion:*:git-checkout:*' sort false
  zstyle ':completion:complete:*:options' sort false

}

# due to a bug in fzf-tab, bell is always, triggered https://github.com/Aloxaf/fzf-tab/issues/187
# TODO: this should be removed when the aforementioned bug is fixed
# unsetopt BEEP
unsetopt LIST_BEEP

# SECTION: : After Load {{{1
# --------------------------------------------------------------------------

[[ $ZPROF_TRACK -eq "1" ]] && zprof # bottom of .zshrc

# }}}

mlog "zshrc loaded"

### End of Zinit's installer chunk
