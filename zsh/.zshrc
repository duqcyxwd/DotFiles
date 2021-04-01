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
export IS_ASYNC=0            # ASYNC only support loading startup message
export START_MESSAGE=0
export LOADING_BAR=0
export ZPROF_TRACK=0

export ZSH_CONFIG_HOME=$HOME/.config/zsh

PATH=$ZSH_CONFIG_HOME/commands:$PATH
FPATH=$ZSH_CONFIG_HOME/functions:$XDG_CONFIG_HOME/zsh/completions:$FPATH
FPATH=$FPATH:$HOME/.zsh

autoload -Uz $ZSH_CONFIG_HOME/functions/*(:t)

# export ZSH_PLUGIN_LOADED=0 # Disable ZSH PLUGIN # unset ZSH_PLUGIN_LOADED
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
  export PATH="$HOME/script:$PATH"
  export PATH="$HOME/my_script:$PATH"
  export PATH="$HOME/my_script/zsh:$PATH"
  # export PATH="/usr/local/opt/maven@3.3/bin:$PATH"
  export PATH="./node_modules/.bin:$PATH"
  export PATH="/usr/local/heroku/bin:$PATH"
  export PATH="/usr/local/opt/ruby/bin:$PATH"
  export PATH="$HOME/.gem/ruby/2.7.0/bin:$PATH"
  export PATH="$PATH:$HOME/.SpaceVim/bin"
  
  export KAFKA_HOME=/usr/local/kafka
  export KAFKA=$KAFKA_HOME/bin
  export KAFKA_CONFIG=$KAFKA_HOME/config
  export PATH=$KAFKA:$PATH
  
  # /Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home
  export JAVA_HOME=$(/usr/libexec/java_home)
  export PATH=$PATH:$JAVA_HOME/bin:$DSE_BIN
  
  export GOPATH=$HOME/go
  export PATH=$PATH:$GOROOT/bin:$GOPATH/bin
  export KUBECONFIG=$KUBECONFIG:$HOME/.kube/config
  
  # export is required for python path
  export NODE_PATH=/usr/lib/node_modules
  
  
  # PATH: Global Parameter {{{2
  # --------------------------------------------------------------------------
  
  # Path to your oh-my-zsh installation.
  export ZSH=$HOME/.oh-my-zsh
  export TERM="xterm-256color"
  export ZSH_LOADING_LOG=~/.startup.log
  export MESSAGE_CACHE_BEFORE_PRINT=~/.startup_all.log
  export WELCOME_MESSAGE=~/.welcome_message.log
  export NVIM_LISTEN_ADDRESS=/tmp/spacevim_nvim_server
  
  [[ $ZPROF_TRACK -eq "1" ]] && zmodload zsh/zprof
  [[ $LOADING_BAR -eq "1" ]] && revolver --style "bouncingBar" start "Loading zsh config"
# }}}
}
# SECTION: : PRE SCRIPT {{{1
# Script Before loading, inlcuding script for prompt
# --------------------------------------------------------------------------
#=============================== pre script  ===========================================

mlog "\n$(date) : zshrc start loading" 

prepare_start_message() {
    if [[ -v ZSH_PLUGIN_LOADED ]]; then
        # Resource zsh.rc
        cat $WELCOME_MESSAGE >$MESSAGE_CACHE_BEFORE_PRINT
        echo "" >$ZSH_LOADING_LOG
    else
        echo "" >$ZSH_LOADING_LOG &&
            echo "" >$WELCOME_MESSAGE &&
            echo "" >$MESSAGE_CACHE_BEFORE_PRINT &&
            (artii "Welcome, Chuan" && neofetch && (fortune | cowsay)) >$WELCOME_MESSAGE &&
            cat $WELCOME_MESSAGE >$MESSAGE_CACHE_BEFORE_PRINT
    fi
}


print_start_message() {
    (cat $WELCOME_MESSAGE && (cat $ZSH_LOADING_LOG | boxes -d stone -p a2t1)) | lolcat
}

alias -g timeElapsed="pv -F 'Elapsed time: %t'"
alias v='py ~/my_script/version.py ./'

power_v() {
    python ~/my_script/version.py ./ powermode
}

# SECTION: : ZSH THEME CONFIG {{{1
# --------------------------------------------------------------------------
# Set name of the theme to load.
# FUNCTION: powerline 9k {{{2
load_POWERLEVEL9K() {
    power-version() {
        local color='%F{yellow}'
        PROJECT_VERSION=$(power_v)
        if [[ $PROJECT_VERSION != 'None' ]]; then
            echo -n "%{$color%}$(print_icon 'VCS_HG_ICON')$PROJECT_VERSION"
        fi
    }

    #POWERLEVEL9K_MODE='nerdfont-complete':
    POWERLEVEL9K_MODE='awesome-fontconfig'
    POWERLEVEL9K_CUSTOM_VERSION="power-version"
    POWERLEVEL9K_COMMAND_EXECUTION_TIME_BACKGROUND='green'
    POWERLEVEL9K_VI_INSERT_MODE_STRING=''

    if [ "$SIMPLE" -eq "1" ]; then
        mlog "THEME: SIMPLE POWERLEVEL9K"
        POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir)
        POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(command_execution_time)
    else
        mlog "THEME: POWERLEVEL9K"
        POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(ram load ip vcs custom_version newline ssh dir)
        POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status vi_mode root_indicator command_execution_time background_jobs time)
    fi

    alias signs="open https://github.com/bhilburn/powerlevel9k#vcs-symbols"
}
# }}}
# FUNCTION: spaceship_power_version_init {{{2
spaceship_power_version_init() {
    SPACESHIP_POWER_VERSION_SHOW="${SPACESHIP_POWER_VERSION_SHOW=true}"
    SPACESHIP_POWER_VERSION_PREFIX="${SPACESHIP_POWER_VERSION_PREFIX="$SPACESHIP_PROMPT_DEFAULT_PREFIX"}"
    SPACESHIP_POWER_VERSION_SUFFIX="${SPACESHIP_POWER_VERSION_SUFFIX="$SPACESHIP_PROMPT_DEFAULT_SUFFIX"}"
    SPACESHIP_POWER_VERSION_SYMBOL="${SPACESHIP_POWER_VERSION_SYMBOL="🍷 "}"
    SPACESHIP_POWER_VERSION_COLOR="${SPACESHIP_POWER_VERSION_COLOR="white"}"

    # ------------------------------------------------------------------------------
    # SECTION:  power_version
    # ------------------------------------------------------------------------------
    spaceship_async_job_load_power_version() {
        [[ $SPACESHIP_POWER_VERSION_SHOW == false ]] && return
        async_job spaceship spaceship_async_job_power_version
    }
    spaceship_async_job_power_version() {
        # echo "spaceship_async_job_power_version" >> ~/temp/temp.log
        local version=$(power_v)
        # echo $version >> ~/temp/temp.log
        [[ -z $version ]] && return
        echo "$version"
    }
    spaceship_power_version() {
        [[ -z ${SPACESHIP_ASYNC_RESULTS[spaceship_async_job_power_version]} ]] && return
        spaceship::section \
            "$SPACESHIP_POWER_VERSION_COLOR" \
            "$SPACESHIP_POWER_VERSION_PREFIX" \
            "${SPACESHIP_POWER_VERSION_SYMBOL}${SPACESHIP_ASYNC_RESULTS[spaceship_async_job_power_version]}" \
            "$SPACESHIP_POWER_VERSION_SUFFIX"
    }

    # Sync way to load power version
    # spaceship_power_version() {
    #     [[ $SPACESHIP_POWER_VERSION_SHOW == false ]] && return
    #     spaceship::exists power_v || return
    #     [[ -f pom.xml || -n *.xml(#qN^/) ]] || return
    #     local version=$(power_v)
    #     spaceship::section \
    #         "$SPACESHIP_POWER_VERSION_COLOR" \
    #         "$SPACESHIP_POWER_VERSION_PREFIX" \
    #         "$SPACESHIP_POWER_VERSION_SYMBOL$version" \
    #         "$SPACESHIP_POWER_VERSION_SUFFIX"
    #     }
}

spaceship_config() {
    # Spaceship
    SPACESHIP_TIME_SHOW=true
    SPACESHIP_EXEC_TIME_ELAPSED=1

    # SPACESHIP_PROMPT_ORDER=(time user host dir git hg package node ruby elixir xcode swift golang php rust haskell julia aws venv conda pyenv dotnet ember kubecontext power_version exec_time line_sep battery vi_mode jobs exit_code char )
    # maximbaz/spaceship-prompt

    SPACESHIP_DIR_PREFIX=""
    SPACESHIP_GIT_STATUS_SHOW=true
    SPACESHIP_GIT_STATUS_PREFIX="["
    SPACESHIP_GIT_STATUS_SUFFIX="] "
    SPACESHIP_GIT_STATUS_COLOR="red"
    SPACESHIP_GIT_STATUS_UNTRACKED="?"
    SPACESHIP_GIT_STATUS_ADDED="+"
    SPACESHIP_GIT_STATUS_MODIFIED="!"
    SPACESHIP_GIT_STATUS_RENAMED="»"
    SPACESHIP_GIT_STATUS_DELETED="✘"
    SPACESHIP_GIT_STATUS_STASHED="$"
    SPACESHIP_GIT_STATUS_UNMERGED="="
    SPACESHIP_GIT_STATUS_AHEAD="⇡"
    SPACESHIP_GIT_STATUS_BEHIND="⇣"
    SPACESHIP_GIT_STATUS_DIVERGED="⇕"

    SPACESHIP_PROMPT_ORDER=(time user host git_branch git_status power_version hg package node ruby elixir xcode swift golang php haskell julia aws venv conda pyenv dotnet ember docker kubecontext exec_time line_sep dir battery vi_mode jobs exit_code char)
}

# }}}2
# FUNCTION: P10K: kube-toggle {{{2
function kube-toggle() {
  if (( ${+POWERLEVEL9K_KUBECONTEXT_SHOW_ON_COMMAND} )); then
    unset POWERLEVEL9K_KUBECONTEXT_SHOW_ON_COMMAND
  else
    POWERLEVEL9K_KUBECONTEXT_SHOW_ON_COMMAND='kubectl|helm2|helm|kubens|kubectx|oc|istioctl|kogito'
  fi
  p10k reload
  if zle; then
    zle push-input
    zle accept-line
  fi
}
# }}}1
# SECTION: : ZSH CUST ASYNC LOAD {{{1
# --------------------------------------------------------------------------
# Async_load {{{2
# {{{3
async_load() {
}
# }}}
# Deprecated
async_load0() {
    # Quick Antigen plugins loading {{{3
    # Something very interesting. callback is working
    # Async works with alias but not tab_complete

    # Make sure this doesn't break anything, if break clean this out
    # source /Users/chuan.du/.antigen/bundles/robbyrussell/oh-my-zsh/oh-my-zsh.sh

    # Manully async load bundles that installed by antigen
    export ANTIGEN_BUNDLES=~/.antigen/bundles
    source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/git/git.plugin.zsh
    source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/tmux/tmux.plugin.zsh
    source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/tmuxinator/tmuxinator.plugin.zsh
    # export GIT_AUTO_FETCH_INTERVAL=1200
    # source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/git-auto-fetch/git-auto-fetch.plugin.zsh
    # brew install git-extras
    source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/git-extras
    source $ANTIGEN_BUNDLES/zsh-users/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh

    # antigen bundle zsh-users/zsh-syntax-highlighting # Async load
    # source $ANTIGEN_BUNDLES/zsh-users/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
    # antigen bundle zdharma/fast-syntax-highlighting
    source $ANTIGEN_BUNDLES/zdharma/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
    source $ANTIGEN_BUNDLES/zdharma/history-search-multi-word/history-search-multi-word.plugin.zsh
    # source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/kubectl/kubectl.plugin.zsh
    source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/mvn/mvn.plugin.zsh
    source $ANTIGEN_BUNDLES/psprint/zsh-cmd-architect/zsh-cmd-architect.plugin.zsh

    # source $ANTIGEN_BUNDLES/Aloxaf/fzf-tab/fzf-tab.plugin.zsh        # tab, c-spc multi select
    source $ANTIGEN_BUNDLES/rupa/z/z.sh                                # z jump around
    source $ANTIGEN_BUNDLES/changyuheng/fz/fz.plugin.zsh               # z zz  with fzf search
    source $ANTIGEN_BUNDLES/wfxr/forgit/forgit.plugin.zsh

    apply_my_config

    autoload -Uz compinit
    if [ $(date +'%j') != $(/usr/bin/stat -f '%Sm' -t '%j' ${ZDOTDIR:-$HOME}/.zcompdump) ]; then
        compinit
    else
        compinit -C
    fi

    ## Disable async loader to test theme
    [[ $ZSH_PLUGIN_LOADED -eq "1" ]] && async_stop_worker lazyloader
}
# }}}2
# async_cust_init {{{2
[[ $IS_ASYNC -eq "1" ]] && source ~/script/zsh-async/async.zsh
async_cust_init() {

    async_init

    _my_worker_stop() {
        # print "STOP MY WORKER"
        async_stop_worker my_worker
    }
    _async_start_message() {
        prepare_start_message
    }

    async_start_worker my_worker -n
    async_register_callback my_worker _my_worker_stop

    async_start_worker lazyloader -n
    async_register_callback lazyloader async_load
    async_worker_eval lazyloader sleep 1 # Start async load after 1s

    if [ "$START_MESSAGE" -eq "1" ]; then
        async_worker_eval my_worker sleep 5
        async_worker_eval my_worker _async_start_message
    fi
}
# }}}2

### }}}1
# SECTION: : ZSH PLUGIN MANAGEMENT {{{1
# --------------------------------------------------------------------------
# METHOD: : Antigen {{{2
# FUNCTION: antigen-1{{{3
__load_antigen0() {
    echo "theme: spaceship"
    if ! type "antigen" >/dev/null; then
        source ~/script/antigen.zsh
        # load the oh-my-zsh's library. it will conflict with other theme
        antigen use oh-my-zsh
    fi
    # antigen theme denysdovhan/spaceship-prompt
    # antigen bundle paulmelnikow/zsh-startup-timer
    antigen apply
}
#}}}

load_Antigen() {
    echo "Load antigen"
    mlog "PlUGIN ENGINE: Antigen"
    if ! type "antigen" >/dev/null; then
        source ~/script/antigen.zsh
        # Load the oh-my-zsh's library. It will conflict with other theme
        # echo "load oh-my-zsh"
        antigen use oh-my-zsh
    fi
    # Antigen loading package {{{3

    antigen bundle vi-mode
    antigen bundle iterm2 # Iterm2 profile, color

    antigen bundle shayneholmes/zsh-iterm2colors # Iterm2 Color "0.01"
    # antigen bundle paulmelnikow/zsh-startup-timer
    antigen bundle djui/alias-tips # Alias helper
    # antigen bundle sei40kr/zsh-fast-alias-tips

    # Somehow this affect spaceship
    # antigen bundle sei40kr/zsh-fzf-docker

    # antigen bundle johanhaleby/kubetail              # Lazy load

    # antigen bundle zsh-users/zsh-autosuggestions     # 0.02  Async load
    # antigen bundle zdharma/history-search-multi-word # 0.02s Async load

    # antigen bundle psprint/zsh-cmd-architect

    # echo "THEME: spaceship"
    # antigen theme denysdovhan/spaceship-prompt
    # Async prompt
    # antigen theme maximbaz/spaceship-prompt

    # Max suggest to switch to powerlevel10k, I will give it a try
    # I like it, it is fast
    # antigen theme romkatv/powerlevel10k

    # antigen bundle git-extras

    # fzf-tab need to work here
    antigen bundle Aloxaf/fzf-tab 
    # give a preview of directory by exa when completing cd
 
    # echo "THEME: pure"
    # antigen bundle mafredri/zsh-async
    # antigen bundle sindresorhus/pure
    # }}}
    # Lazy load bundles {{{3
    if ! type "kubetail" >/dev/null; then
        kubetail() {
            unfunction "$0"
            antigen bundle johanhaleby/kubetail
            $0 "$@"
        }
    fi

    npm() {
        unfunction "$0" && antigen bundle npm && $0 "$@"
    }
    brew() {
        unfunction "$0" && antigen bundle brew && $0 "$@"
    }
    docker() {
        unfunction "$0" && antigen bundle docker && $0 "$@"
    }
    lein() {
        unfunction "$0" && antigen bundle lein && $0 "$@"
    }

    nvm() {
        unfunction "$0"
        export NVM_DIR="$HOME/.nvm"
        # lazy load for nvm. Take 1.5 second to load
        [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"                   # This loads nvm
        [ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion" # This loads nvm bash_completion
        $0 "$@"
    }

    # tmuxinator() {
    #     unfunction "$0" && antigen bundle tmuxinator && $0 "$@"
    # }
    # }}}
    antigen apply
}
# METHOD: : Antibody {{{2
# --------------------------------------------------------------------------
__load_Antibody() {
    mlog "Use antibody"
    # No matter what I try, this is super slow
    autoload -Uz compinit && compinit
    source ~/.zsh_plugins.sh

    # source <(antibody init)

    # # antibody bundle bhilburn/powerlevel9k
    # antibody bundle <~/.zsh_plugins.txt

    # echo "POWERLINE THEME: spaceship"
    # antibody bundle denysdovhan/spaceship-prompt
    # antibody bundle maximbaz/spaceship-prompt
}

# }}}
# METHOD: : Lazy Plugin Load {{{2
# --------------------------------------------------------------------------

# Helm completion
# source <(helm completion zsh | sed -E 's/\["(.+)"\]/\[\1\]/g')

# alias helm3="/usr/local/Cellar/helm/3.2.1/bin/helm"
alias helm2="/usr/local/Cellar/helm@2/2.16.7/bin/helm"

if [ $commands[helm] ]; then
    helm() {
        unfunction "$0"
        source <(helm completion zsh | sed -E 's/\["(.+)"\]/\[\1\]/g')
        $0 "$@"
    }
fi

# Example of lazy load
# Check if 'kubectl' is a command in $PATH
if [ $commands[kubectl] ]; then
    kubectl() {
        unfunction "$0"
        source <(kubectl completion zsh)
        $0 "$@"
    }
fi


nvm() {
  unfunction "$0"
  export NVM_DIR="$HOME/.nvm"
  # lazy load for nvm. Take 1.5 second to load
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"                   # This loads nvm
  [ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion" # This loads nvm bash_completion
  $0 "$@"
}

# Lazy load example {{{3
# if [ $commands[kubectl] ]; then
#
#   # Placeholder 'kubectl' shell function:
#   # Will only be executed on the first call to 'kubectl'
#   kubectl() {
#
#     # Remove this function, subsequent calls will execute 'kubectl' directly
#     unfunction "$0"
#
#     # Load auto-completion
#     source <(kubectl completion zsh)
#
#     # Execute 'kubectl' binary
#     $0 "$@"
#   }
# fi
# }}}

# }}}1
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

  zinit light shayneholmes/zsh-iterm2colors

  # No turbo mode
  # fzf-tab: unknow problem
  # zsh-vi-mode: Fast not need
  # zsh-iterm2colors: random color when load complete
  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
  zinit light-mode for \
    jeffreytse/zsh-vi-mode \
    Aloxaf/fzf-tab

  # NOTE: fzf-tab needs to be loaded after compinit, but before plugins which will wrap widgets, 
  # such as zsh-autosuggestions or fast-syntax-highlighting!!
  # zinit load Aloxaf/fzf-tab

  # Load theme
  zinit ice depth=1; zinit light romkatv/powerlevel10k
  [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

  zinit wait lucid light-mode for \
    zdharma/history-search-multi-word \
    atinit"zicompinit; zicdreplay"  zdharma/fast-syntax-highlighting \
    atload"_zsh_autosuggest_start"  zsh-users/zsh-autosuggestions

  zinit wait silent load for \
    OMZ::lib/git.zsh \
    OMZ::lib/functions.zsh \
    OMZ::plugins/git/git.plugin.zsh \
    OMZ::plugins/git-extras/git-extras.plugin.zsh \
    OMZ::plugins/systemd/systemd.plugin.zsh \
    OMZ::plugins/iterm2/iterm2.plugin.zsh \
    OMZ::plugins/kubectl/kubectl.plugin.zsh 

  zinit wait="1" lucid light-mode silent for \
    blockf atpull'zinit creinstall -q .'  zsh-users/zsh-completions \
    wfxr/forgit \
    vim/vim \
    Dabz/kafka-zsh-completions \
    psprint/zsh-cmd-architect \
    b4b4r07/enhancd \
    agkozak/zsh-z \
    djui/alias-tips \
    zpm-zsh/template

  # zinit load agkozak/zsh-z

  # The last plugin to load need overwrite alias and keybinding
  # check loading order by zinit time (-m)
  # zinit light zpm-zsh/empty


  for snippet in $ZSH_CONFIG_HOME/snippets/*.zsh; do
    # source $snippet
    # NOTES: zinit in for is buggy
    mlog "snippet loading $snippet "

    zinit ice wait="1" silent;
    zinit snippet $snippet
  done
  unset snippet

  # Need a trigger to overwrite some setting
  # bindkey.zsh and dir-completion are lazy loading
  zinit ice wait="1" atload'__git_alias && bindkey.zsh && dir-completion' silent;
  zinit snippet $ZSH_CONFIG_HOME/snippets/history.zsh

  # zinit update # Update plugin or snippet
  # zinit self-update updates zinit
}
# }}}2

zinit_load

# SECTION: : SCRIPT TOOLS {{{1
# --------------------------------------------------------------------------
#   TOOL: FUNCTION {{{2
# --------------------------------------------------------------------------
# FUNCTION: color-test {{{3
color-test() {
    clear
    cat ${HOME}/script-tool/iterm-syntax-test.txt
}

# FUNCTION: sample_function {{{3
sample_function() {
    if [ $# -eq 0 ]; then
        echo "Sample function"
        return
    fi
    echo $@
    echo $1
    echo $2
}



# FUNCTION: _weekly_upgrade {{{3
_weekly_upgrade() {
  # git -C  ~/github/powerlevel10k pull #moved to zinit
  zinit self-update
  zinit delete --clean
  zinit update --all
  zinit cclear
  zinit compinit

  brew update            # update brew
  brew upgrade           # update formula
}


# }}}2
#   TOOL: WIDGET {{{2
# --------------------------------------------------------------------------
zle -N _expand_stuff
function _expand_stuff() { zle _expand_alias || zle .expand-word || true }

#   TOOL: FZF RELATED FUNCTIONS {{{2
# FZF with Brew {{{3
# Install (one or multiple) selected application(s)
# using "brew search" as source input
# mnemonic [B]rew [I]nstall [P]lugin
bip() {
  local inst=$(brew search | fzf -m)

  if [[ $inst ]]; then
    for prog in $(echo $inst);
    do; brew install $prog; done;
  fi
}
# Update (one or multiple) selected application(s)
# mnemonic [B]rew [U]pdate [P]lugin
bup() {
  local upd=$(brew leaves | fzf -m)

  if [[ $upd ]]; then
    for prog in $(echo $upd);
    do; brew upgrade $prog; done;
  fi
}
# Delete (one or multiple) selected application(s)
# mnemonic [B]rew [C]lean [P]lugin (e.g. uninstall)
bcp() {
  local uninst=$(brew leaves | fzf -m)

  if [[ $uninst ]]; then
    for prog in $(echo $uninst);
    do; brew uninstall $prog; done;
  fi
}

# FZF with File {{{3
# alternative using ripgrep-all (rga) combined with fzf-tmux preview
# implementation below makes use of "open" on macOS, which can be replaced by other commands if needed.
# allows to search in PDFs, E-Books, Office documents, zip, tar.gz, etc. (see https://github.com/phiresky/ripgrep-all)
# find-in-file - usage: fif <searchTerm> or fif "string with spaces" or fif "regex"
fif() {
    if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
    local file
    file="$(rga --max-count=1 --ignore-case --files-with-matches --no-messages "$@" | fzf-tmux +m --preview="rga --ignore-case --pretty --context 10 '"$@"' {}")" && open "$file"
}

# FZF with kill {{{3
# fkill - kill processes - list only the ones you can kill. Modified the earlier script.
fkilll() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID  | fzf -m --header-lines=1 | awk '{print $2}')
    else
        pid=$(ps -ef | fzf -m --header-lines=1 | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}


#}}}
# SECTION: : DEV TOOL {{{1
# --------------------------------------------------------------------------
#   DEVTOOL: : bat {{{2
{
    if [ $commands[bat] ]; then
      export MANPAGER="sh -c 'col -bx | bat -l man -p --color=always'"
    fi
}
#   DEVTOOL: : exa {{{2
{
    if [ $commands[exa] ]; then
      alias exa='/usr/local/bin/exa --time-style=long-iso --group-directories-first -F --icons --color=always'
      alias e=exa
      alias ea='exa -a'
      alias eaa='exa .?* -d'

      alias ls='e'

      # g git, a, all
      alias l='exa -lhbF'                                                # list, size, type
      alias ld='exa -lhbFD'                                              # list, size, type
      alias lg='l --git'                                                 # list, size, type, git
      alias lss='l -s ext'                                               # list, size, type

      alias la='exa -lbFa'                     
      alias laa='la .?* -d'                                              # Show hidden files only
      alias lag=la --git

      alias ll='l -G'                                                    # long list
      alias lls='ll -s ext'                                              # long list sort
      alias llg='ll --git'                                               # long list

      alias lla='ll -a'                                                  # long list
      alias llag='lla --git'                                             # long list
      alias llaa='lla .?* -d'                                            # long list

      alias lx='exa -lbhHigUmuSa --time-style=long-iso --color-scale'    # all list
      alias lxaa='lx .?* -d'                                            


      alias lta='exa --group-directories-first -lT'
      alias lt1='exa --group-directories-first -lT -L 1'
      alias lt2='exa --group-directories-first -lT -L 2'
      alias lt3='exa --group-directories-first -lT -L 3'
      alias lt4='exa --group-directories-first -lT -L 4'
      alias lt=lt2
    fi
}

# }}}1 DEV TOOL
# SECTION: : CONFIG (FZF, PLUGIN) {{{1
# --------------------------------------------------------------------------
#   FUNCTION: zsh_plugins_config {{{2
zsh_plugins_config() {

    alias fzfc='fzf | tr -d "\n" | pbcopy && pbpaste'


    # vi-mode
    export KEYTIMEOUT=1

    ## Fun
    alias test-passed='if [ "$?" -eq "0" ]; then lolcat ~/.tp -a -s 40 -d 2; fi;'

    # b4b4r07/enhancd
    ENHANCD_HYPHEN_NUM=100
    ENHANCD_DISABLE_HYPHEN=1;

    # agkozak/zsh-z
    ZSHZ_CASE=ignore


  # zsh-vi-mode
  # The plugin will auto execute this zvm_after_init function
  # ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BLINKING_BLOCK
  function zvm_after_init() {
    # zsh_cust_bindkey
    ac_my_colors
  }

}
#   FUNCTION: apply_my_config {{{2
apply_my_config() {
  zsh_plugins_config
}
# }}}2
# }}}1
# SECTION: : ZSH zstyle {{{1
{ # zstyle
  # https://superuser.com/questions/1092033/how-can-i-make-zsh-tab-completion-fix-capitalization-errors-for-directorys-and
  # ZSH case insensitive completion
  zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
  
  # zstyle ':fzf-tab:*' prefix '.'
  # zstyle ':fzf-tab:*' switch-group ',' '.'
  zstyle ':fzf-tab:*' show-group full
  zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
  zstyle ':completion:*:descriptions' format '[%d]'
  # zstyle ":completion:*:descriptions" format "---- %d ----"
  
  zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
  # zstyle ':fzf-tab:complete:__enhancd::cd:*' fzf-preview 'exa -1 --color=always $realpath'
}


# }}}
# SECTION: : After Load {{{1
# --------------------------------------------------------------------------
{ # SECTION: : After Load
  if [ "$IS_ASYNC" -ne "1" ]; then
      # [[ $ZSH_PLUGIN_LOADED -ne "1" ]] && async_load
      if [ "$START_MESSAGE" -eq "1" ]; then
          prepare_start_message
      fi
  fi
  
  [[ $LOADING_BAR -eq "1" ]] && revolver stop # Stop loading Bar
  [[ $START_MESSAGE -eq "1" ]] && print_start_message
  
  # Only load omz and theme once
  # export ZSH_PLUGIN_LOADED=1
  export IS_ASYNC=0
  # unset ZSH_PLUGIN_LOADED && unset IS_ASYNC
  
  [[ $ZPROF_TRACK -eq "1" ]] && zprof # bottom of .zshrc
}
# }}}

apply_my_config

[ -f ~/.zshrc-local.sh ] && source ~/.zshrc-local.sh
mlog "zshrc loaded"
