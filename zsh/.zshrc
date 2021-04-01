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
# METHOD: : Zinit {{{2
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

  # zinit ice wait="1" atload'__git_alias && zsh_cust_bindkey && my_comp' silent;
  zinit ice wait="1" atload'__git_alias && bindkey.zsh && my_comp' silent;
  # zinit snippet $ZSH_CONFIG_HOME/snippets/directory_auto_complete.zsh
  zinit snippet $ZSH_CONFIG_HOME/snippets/history.zsh

  for i in $ZSH_CONFIG_HOME/snippets/*.zsh; do
    source $i
    # zinit snippet $i
  done

  # zinit update # Update plugin or snippet
  # zinit self-update updates zinit
}
# }}}2
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

# FUNCTION: c-bash {{{3
c-bash() {
    if [ $# -eq 0 ]; then
        echo "Create Bash Script"
        return
    fi
    touch $1
    chmod +x $1
    echo "#!/bin/sh\n" >>$1

}
alias cbash=c-bash

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
#   DEVTOOL: : K8s {{{2
#     K8S: NAMESPACE CONTEXT {{{3
{
  # Set k8s ns
  set-ns() {
      echo $1 >$HOME/.kube/KUBE_NS
      export KUBE_NS=$1
      # echo "kubectl config set-context --current --namespace=$KUBE_NS"
      kubectl config set-context --current --namespace=$KUBE_NS
      echo "Current Namespace: " $KUBE_NS
      kube_env_update&
  }
  
  set-nsi() {
      set-ns $(kubectl get namespaces | awk 'NR>1' | fzf -0 | awk '{print $1}')
  }
  
  set-context() {
      if [ $# -eq 0 ]; then
          echo "Require cluster"
          return
      fi
  
      echo "Will change context to $@"
      local context=$@
      kubectl config use-context $context &&
        local ns=$(kubectl config view --minify | grep namespace | awk '{print $2}') &&
        set-ns $ns
      # kube_env_update&
  }
  
  set-contexti() {
    # kubectl config use-context $(kubectl config get-contexts  | awk 'NR>1' | fzf | awk '{print $2}')
    set-context $(kubectl config get-contexts  | awk 'NR>1' | fzf -0 | awk '{print $2}')
  }
  
}
#     K8S: scale deployment {{{3
# Useage: 
#   kgdi | ksd0
#   kubectl scale deployment $(kgdi) --replicas=1
{
  ksd0() { 
    # Support pipe
    while read data; 
    do; 
      kubectl scale deployment $data --replicas=0
    done; 
  }
  
  ksd1() { 
    # Support pipe
    while read data; 
    do; 
      kubectl scale deployment $data --replicas=1
    done; 
  }
  
}

#     K8S: NAMESPACE CLEAN {{{3
{
  # helm ls | grep $KUBE_NS | cut -f1 | hpurge && kdns $KUBE_NS
  hpurge() { 
    # Support pipe
    while read data; 
    do; 
      helm del --purge $data; 
    done; 
  }
  
  alias hdelp='helm del --purge'
}
#     K8S: SETUP ALIAS {{{3
{
  if [ -f $HOME/.kube/KUBE_NS ]; then
      export KUBE_NS=$(cat $HOME/.kube/KUBE_NS)
  fi
  
  alias kexecit='kubectl exec -it'
  unalias -m kgns

  alias ns=set-ns
  alias nsi=set-nsi
  
  alias context=set-context
  alias contexti=set-contexti

  alias ksd='kubectrl scale deployment'

  alias kgns='kubectl get namespaces --sort-by=.metadata.creationTimestamp'
  alias kgdi="kubectl get deployment | fzf --header-lines=1 | awk '{print \$1}'"
}



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

#   DEVTOOL: __fzf_config {{{2
# -------------------------------------------------------
__fzf_config() {
   # require fzf junegunn/fzf
   # brew install fzf

  # FZF KEYBINDING {{{3
    export FZF_MY_KEYBINDING="
    --bind '?:toggle-preview' 

    --bind 'left:up' 
    --bind 'left:+toggle' 
    --bind 'right:toggle+down' 
    --bind 'tab:down' 
    --bind 'btab:up' 

    --bind='ctrl-k:preview-half-page-up'
    --bind='ctrl-j:preview-half-page-down'

    --bind 'ctrl-h:backward-char'
    --bind 'ctrl-l:forward-char'

    --bind='ctrl-b:half-page-up'
    --bind='ctrl-f:half-page-down'

    --bind='ctrl-s:toggle-sort' 
    --bind='ctrl-w:toggle-preview-wrap'

    "
    # Why not tab:toggle+down
    # {{{
    # Need a way to move cursor
    # --bind 'left:up' 

    # --bind='ctrl-u:half-page-up'
    # --bind='ctrl-d:half-page-down'
    # }}}
    
    # Other Default keybinding
    # ctrl-n
    # ctrl-p
    #
    # ctrl-e
    # ctrl-a
    # ctrl-u
    # ctrl-w
    
    #### My fzf shared general keybinding
    # ctrl-r vim open remote
    # ctrl-v vim open
    # ctrl-y copy
    #
    # ctrl-o fzf-exec
    # ctrl-space  bat preview
    #
    
    #### Fuzzy preview
    # enter echo input
    #
    # shift-right depth increase
    # shift-left depth decrease
    #
    # alt-up/down/left/right
    #
    #### Fag fzf ag
    # }}}3
  # FZF Theme {{{3

    # export FZF_COLOR_SCHEMA_BORDER=""
    export FZF_COLOR_SCHEMA_BORDER="--color 'fg:#bbccdd,fg+:#ddeeff,bg:#334455,preview-bg:#223344,border:#778899' --border"

    # Dracula Theme
    local __FZF_COLOR_SCHEMA_DRACULA='
    --color=dark
    --color=fg:-1,bg:-1,hl:#5fff87,fg+:-1,bg+:-1,hl+:#ffaf5f
    --color=info:#af87ff,prompt:#5fff87,pointer:#ff87d7,marker:#ff87d7,spinner:#ff87d7
    '
    
    # Theme Ayu Mirage
    local __FZF_COLOR_SCHEMA_AYU='
     --color=fg:#cbccc6,bg:#1f2430,hl:#707a8c
     --color=fg+:#707a8c,bg+:#191e2a,hl+:#ffcc66
     --color=info:#73d0ff,prompt:#707a8c,pointer:#cbccc6
     --color=marker:#73d0ff,spinner:#73d0ff,header:#d4bfff'

    # Theme NORD
    local __FZF_COLOR_SCHEMA_NORD='
    --color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C,fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C
    --color pointer:#BF616A,info:#4C566A,spinner:#4C566A,header:#4C566A,prompt:#81A1C1,marker:#EBCB8B
    '

    # FZF Theme }}}
  # FZF Default Config {{{3
    export FZF_MY_OPTS="--reverse --ansi --multi --exit-0 --cycle"

    # FZF Default options
    # export FZF_DEFAULT_OPTS="$FZF_MY_OPTS $FZF_MY_KEYBINDING $__FZF_COLOR_SCHEMA_DRACULA"
    export FZF_DEFAULT_OPTS="$FZF_MY_OPTS $FZF_MY_KEYBINDING $__FZF_COLOR_SCHEMA_AYU"
    # export FZF_DEFAULT_OPTS="$FZF_MY_OPTS $FZF_MY_KEYBINDING $__FZF_COLOR_SCHEMA_NORD"
    

    export FZF_TMUX_HEIGHT=80%        #Aslo been used by fzf-tab

    # FZF CTRL_T
    local FZF_PREVIEW_FILE='bat --style="numbers,changes" --color=always {} -r 0:200| head -200'
    export FZF_CTRL_T_OPTS="--preview \"${FZF_PREVIEW_FILE}\" $FZF_COLOR_SCHEMA_BORDER"                          #fzf file

    # FZF CTRL_G
    local FZF_PREVIEW_DIR='exa --group-directories-first -F --icons --group-directories-first -T -lh -L 2 --color=always {}'
    export FZF_ALT_C_OPTS="--preview \"${FZF_PREVIEW_DIR}\""                                                      #fzf cd Folder

    # Options to fzf command
    # export FZF_COMPLETION_OPTS='+c -x'
    
  # FZF fd support {{{3
    # Setting fd as the default source for fzf
    if [ $commands[fd] ]; then
      # Use fd (https://github.com/sharkdp/fd) instead of the default find
      export FZF_DEFAULT_COMMAND="fd --type file --color=always"

      # https://github.com/Aloxaf/fzf-tab/issues/65
      # export FZF_DEFAULT_COMMAND='fd --hidden --follow --type=f'
      # _fzf_compgen_path() { fd --hidden --follow --type=f }

      export FZF_ALT_C_COMMAND="fd --type d --color=always"
      export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

      # Use fd (https://github.com/sharkdp/fd) instead of the default find
      # command for listing path candidates.
      # - The first argument to the function ($1) is the base path to start traversal
      # - See the source code (completion.{bash,zsh}) for the details.
      _fzf_compgen_path() { fd --hidden --follow --exclude ".git" . "$1" }

      # Use fd to generate the list for directory completion
      _fzf_compgen_dir() { fd --type d --hidden --follow --exclude ".git" . "$1" }

    fi
    # }}}3

} 
#   DEVTOOL: __ls_fuzzy_preview {{{2
# -------------------------------------------------------
__ls_fuzzy_preview() {

  # fd_search_cur_dir{{{4
    export FD_SEARCH_CUR_DIR_DEPTH=1
    __fd_search_cur_dir_dir() {
      fd -d $FD_SEARCH_CUR_DIR_DEPTH --hidden --no-ignore-vcs --color=always --type file
    }

    __fd_search_cur_dir_file() {
      fd -d $FD_SEARCH_CUR_DIR_DEPTH --hidden --no-ignore-vcs --color=always --type directory
    }

    fd_search_cur_dir() {
      # fd -d $FD_SEARCH_CUR_DIR_DEPTH --hidden --no-ignore-vcs --color=always
      { __fd_search_cur_dir_file && __fd_search_cur_dir_dir}
    }
  # }}}4
  # short_pwd{{{4
  # Short pwd for ls_fuzzy_preview
    short_pwd() {
      local pwd_str="$(pwd)"
      local SHORT_PWD_LENGTH=38
      if [[ ${#pwd_str} -gt $SHORT_PWD_LENGTH ]]; then
        echo $pwd_str | awk -v len=${SHORT_PWD_LENGTH} '{print "..."substr($0,length($0)-len,len+1)"/"}'
      else
        echo "${pwd_str}/"
      fi
    }
  # ls_fuzzy_preview {{{4
  # Notes: we can mix use of bind and while loop key 
  # Can't used execute and while loop key together
    ls_fuzzy_preview() {

      local searchTerm=""
      local FZF_FUZZY_BIND_OPTS=" 
        --bind=\"ctrl-r:execute-silent(echo {} | agnvim_remote_open )\"
        --bind=\"ctrl-y:execute-silent(echo {} | pbcopy )\"
      "
      while out=$( fd_search_cur_dir | FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS $FZF_COLOR_SCHEMA_BORDER $FZF_FUZZY_BIND_OPTS" fzf-tmux -0 \
        --preview "quick-preview {}" --exit-0 \
        --expect=ctrl-v,ctrl-e,ctrl-space,enter,alt-left,alt-right,alt-up,alt-down,shift-left,shift-right \
        --print-query --header "[${FD_SEARCH_CUR_DIR_DEPTH}]:$(short_pwd)" \
        --preview-window right:50% --height ${FZF_TMUX_HEIGHT:-100%} \
        -q "$searchTerm" \
        );
      do
        searchTerm=$(head -1 <<< "$out"| tail -1)
        key=$(head -2 <<< "$out"| tail -1)
        input=$(echo "$out" | tail +3 | tr '\n' ' ')

        #echo $out
        #echo "input: " $input
        #echo "key: " $key
        #echo "searchT: " $searchTerm

        if [[ "$key" == 'alt-left' ]]; then
          pushd +0 1>/dev/null;
        elif [[ "$key" == 'alt-right' ]] || [[ "$key" == 'alt-down' ]]; then
          pushd -1 1>/dev/null;
        elif [[ "$key" == 'alt-up' ]]; then
          cd ../
        elif [[ "$key" == 'shift-right' ]]; then
          FD_SEARCH_CUR_DIR_DEPTH=$(expr $FD_SEARCH_CUR_DIR_DEPTH + 1)
        elif [[ "$key" == 'shift-left' ]]; then
          local new_dept=$(expr $FD_SEARCH_CUR_DIR_DEPTH - 1)
          if [[ "$new_dept" -ge "1" ]];then
            FD_SEARCH_CUR_DIR_DEPTH=$new_dept
          fi

        elif [[ "$key" == 'ctrl-v' ]]; then
          nvim $input
        elif [[ "$key" == 'ctrl-o' ]]; then
          fzf-exec $input
          break;

        elif [[ "$key" == 'ctrl-space' ]] || ; then
          if [[ -d "${input}" ]] ; then
            cd "${input}"
          elif [[ -f "${input}" ]]; then
            bat --color always --paging always --style full $input
            # bat --color always --pager always $input | LESS='-R' less
          fi
        elif [[ "$key" == 'enter' ]]; then
          echo $input
          break;
        fi
      done
    }
  # }}}4

    alias lf=ls_fuzzy_preview
}
#   DEVTOOL: __fzf_ag_preview fag {{{2
# -------------------------------------------------------
__fzf_ag_preview() {
    fag() {
      if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
    
      local FZF_FAG_BIND_OPTS=" \
        --bind=\"ctrl-space:execute(bat --style=numbers --color=always --paging always --highlight-line {2} {1} | LESS='-R +{2}' less)\"
        --bind=\"ctrl-e:execute(echo {} | cut -d ':' -f1 | xargs fzf-exec )\"
        --bind=\"ctrl-v:execute(echo {} | agnvim_open )\"
        --bind=\"ctrl-r:execute-silent(echo {} | agnvim_remote_open )\"
        --bind=\"ctrl-y:execute-silent(echo {} | cut -d ':' -f1 | xargs | tr -d '\\\n' | pbcopy )\"
        --header \"ctrl-o:fzfexec, ctrl-y:pbcopy, ctrl-r:nvim_remote, ctrl-v:nvim\"
      "
      # -0 exit when no match
      # -1 Automatically select the only match 
      # ag --nobreak --noheading --color $@ | FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS $FZF_COLOR_SCHEMA_BORDER $FZF_FAG_BIND_OPTS"  fzf-tmux -0 --preview "agbat {}"
      #
      # Use new fzf build feature to replace agbat
      ag --nobreak --noheading --color $@ | FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS $FZF_COLOR_SCHEMA_BORDER $FZF_FAG_BIND_OPTS"  \
        fzf-tmux -0 --delimiter : \
        --preview 'bat --style=numbers --color=always --highlight-line {2} {1}' \
        --preview-window +{2} 
        # Not working with :: in zinit --delimiter
        # | cut -d ':' -f1 | xargs fzf-exec   # This is not working with ctrl-v, anything command launches other script
    }


    # We can't simplely launch vim with fzf keybinding that is why we need a wrapper fn
    # Same as fag except enter will open in vim and keep the search
    vag() {
      while out=$(fag $@)
      do
        echo $out
        file=$(echo $out | cut -d ":" -f1)
        line=$(echo $out | cut -d ":" -f2)
  
        if [[ "$file" != "" ]]; then
          vim $file +$line
        fi
  
      done
    }
}
# }}}2
# }}}1 DEV TOOL
# SECTION: : Autocompletion {{{1
# --------------------------------------------------------------------------
# Autocompletion for teamocil
compctl -g '~/.teamocil/*(:t:r)' itermocil

local _repodir="${HOME}/code/"
c() {cd $_repodir$1}

local _gh_dir="${HOME}/github/"
gh() {cd $_gh_dir$1}

local _duqcyxwd_dir="${HOME}/duqcyxwd/"
d() {cd $_duqcyxwd_dir$1}


my_comp() {
  # https://github.com/zsh-users/zsh-completions/blob/master/zsh-completions-howto.org
  _code_directory_completion() {
    _alternative \
      "dirs:user directory:($(/bin/ls $_repodir/))" 
  }

  _gh_directory_completion() {
    _alternative \
      "dirs:user directory:($(/bin/ls $_gh_dir/))" 
  }

  _duqcyxwd_directory_completion() {
    _alternative \
      "dirs:user directory:($(/bin/ls $_duqcyxwd_dir/))" 
  }

  compdef _code_directory_completion c
  compdef _gh_directory_completion gh
  compdef _duqcyxwd_directory_completion d
}
# }}}
# SECTION: : CONFIG (FZF, PLUGIN) {{{1
# --------------------------------------------------------------------------
#   FUNCTION: __fzf_git_config {{{2
__fzf_git_config(){
    # Interactive git and rewrite some helper function
    # antigen bundle 'wfxr/forgit'
    
    # load fzf default options
    FORGIT_FZF_DEFAULT_OPTS="
    $FZF_DEFAULT_OPTS
    --height='80%'
    +1
    "

    __git_pager=$(git config core.pager || echo 'cat')                           # to use diff-so-fancy

    forgit::stash::show_cust() {
      # ctrl-d for drop stash
      forgit::inside_work_tree || return 1
      local cmd opts
      cmd="echo {} |cut -d: -f1 |xargs -I% fzf_preview_git_stash % |$__git_pager"
      opts="
          $FORGIT_FZF_DEFAULT_OPTS
          +s +m -0 --tiebreak=index --bind=\"enter:execute($cmd)\"
          $FORGIT_STASH_FZF_OPTS
      "
      git stash list | FZF_DEFAULT_OPTS="$opts" fzf --preview="$cmd"
    }

    export FORGIT_NO_ALIASES=true
    # forgit_log=glo
    # forgit_diff=gd
    # forgit_add=ga
    # forgit_reset_head=grh
    # forgit_ignore=gi
    # forgit_checkout_file=gcf
    # forgit_clean=gclean
    # forgit_stash_show=gss
    # forgit_cherry_pick=gcp
    # forgit_rebase=grb
    # forgit_fixup=gfu
    # forgit_checkout_branch=gcb

  # FZF ALIAS {{{
  # --------------------------------------------------------------------------
  alias fga='forgit::add'
  alias fgrs='forgit::restore'
  alias fgclean='forgit::clean'
  alias fgd='forgit::diff'
  # alias fgl='forgit::log'
  # alias fgss='forgit::stash::show'
  alias fgrh='forgit::reset::head'
  alias fgi='forgit::ignore'

  alias gai=forgit::add
  alias gdi=forgit::diff
  alias gstsi0=forgit::stash::show
  alias gstsi=forgit::stash::show_cust
  alias grhi=forgit::reset::head

    # }}}
    # FZF: GIT COMMIT: log, git rebase/reset/revert {{{3
  # --------------------------------------------------------------------------
    
    # glo oneline for fzf select
    _glo_one_line() {
      git log --graph --color=always --format='%C(auto)%h%d %s %C(black)%C(bold)%cr' $@
    }

    __git_commit_preview_cmd="echo {} |grep -Eo '[a-f0-9]+' | head -1 |xargs -I% fzf_preview_git_commit % |$__git_pager | LESS='-R' less"
    # __git_commit_fzf_opts=" -0" # $FZF_DEFAULT_OPTS is used by default 
    __git_commit_fzf_opts="$FZF_DEFAULT_OPTS -m -0
    --bind=\"ctrl-space:execute($__git_commit_preview_cmd)\"
    --bind=\"ctrl-y:execute-silent(echo {} |grep -Eo '[a-f0-9]+' | head -1 | tr -d '\\\n' | pbcopy)\"
    " # $FZF_DEFAULT_OPTS is used by default 

    git_log_interactive_preview(){
      _glo_one_line $@  \
        | FZF_DEFAULT_OPTS="$__git_commit_fzf_opts" fzf --preview="$__git_commit_preview_cmd"
    }
    # get git commit SHA1 short
    git_log_interactive_select(){
      _glo_one_line $@  \
        | FZF_DEFAULT_OPTS="$__git_commit_fzf_opts" fzf --preview="$__git_commit_preview_cmd" \
        | /usr/bin/grep -Eo '[a-f0-9]{7,41}' \
        | tr -d '\n' \
        | pbcopy && pbpaste

    }

    git_revert_interactive() {
      git_log_interactive_select | xargs git revert $@
    }  

    git_reset_interactive(){
      git_log_interactive_select | xargs git reset $@
    }

    git_rebase_interactive(){
      git_log_interactive_select | xargs git rebase $@
    }

    # alias glop=glopi
    alias gloi=git_log_interactive_select
    alias glo=gloi
    # get git commit SHA1 long
    alias glois='git_log_interactive_select |/usr/bin/grep -Eo "[a-f0-9]{7,41}" | xargs git show | head -1 | cut -d" " -f2'

    alias grevi=git_revert_interactive
    alias greseti=git_reset_interactive
    alias grebi=git_rebase_interactive

    # }}}3
  # FZF: GIT FILES {{{3
  # --------------------------------------------------------------------------

    # Restore modified file
    alias grsi=forgit::checkout::file
    alias gcoi=forgit::checkout::file

    # Cleanup just untracked file 
    # git clean selector
    # alias git_clean_dry_run=
    git_clean_interactive() {

      local FZF_GIT_FILE_BIND_OPTS=" \
        --bind=\"ctrl-space:execute(bat --style=numbers --color=always --paging always {} )\"
        --bind=\"ctrl-e:execute(echo {} | cut -d ':' -f1 | xargs fzf-exec )\"
        --bind=\"ctrl-v:execute(echo {} | agnvim_open )\"
        --bind=\"ctrl-r:execute-silent(echo {} | agnvim_remote_open )\"
        --bind=\"ctrl-y:execute-silent(echo {} | cut -d ':' -f1 | xargs | tr -d '\\\n' | pbcopy )\"
        --header \"ctrl-o:fzfexec, ctrl-y:pbcopy, ctrl-r:nvim_remote, ctrl-v:nvim\"
      "

      forgit::inside_work_tree || return 1
      local files opts
      opts="
        $FORGIT_FZF_DEFAULT_OPTS
        -m -0
        $FORGIT_CLEAN_FZF_OPTS
        "
        # Note: Postfix '/' in directory path should be removed. Otherwise the directory itself will not be removed.
        files=$(git clean -dfn "$@"| sed 's/^Would remove //' | FZF_DEFAULT_OPTS="$FORGIT_FZF_DEFAULT_OPTS $FZF_COLOR_SCHEMA_BORDER $FZF_GIT_FILE_BIND_OPTS" \
          fzf --preview 'quick-preview {}' | sed 's#/$##')
        [[ -n "$files" ]] && echo "$files" | tr '\n' '\0' | xargs -0 -I% git clean -xdf '%' && return
        echo 'Nothing to clean.'

    
      # Use new fzf build feature to replace agbat
      # ag --nobreak --noheading --color $@ | FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS $FZF_COLOR_SCHEMA_BORDER $FZF_GIT_FILE_BIND_OPTS"  \
      #   fzf-tmux -0 --delimiter : \
      #   --preview 'bat --style=numbers --color=always --highlight-line {2} {1}' \
      #   --preview-window +{2} 
      #   # | cut -d ':' -f1 | xargs fzf-exec   # This is not working with ctrl-v, anything command launches other script
    }

    # Cleanup untracked file include ignore file
    alias gcleani0=forgit::clean

    # Cleanup all untracked file
    # gcleani -x
    alias gcleani=git_clean_interactive

    # }}}3
  # FZF: GIT BRANCH {{{3
  # --------------------------------------------------------------------------
  
    # Git loves FZF
    # My personal git preivew scripts
    
    # A, input, B parse selection and preview, C, output
    #

    __git_branch_fzf_opts="$FORGIT_FZF_DEFAULT_OPTS -m -0
    --bind=\"ctrl-y:execute-silent(echo {} | $__gb_clean_cmd_str | tr -d '\\\n' | pbcopy)\"
    --preview-window hidden
    --preview-window right:70%
    " # $FZF_DEFAULT_OPTS is used by default 
    # "echo {} |grep -Eo '[a-f0-9]+' | head -1 |xargs -I% fzf_preview_git_commit % |$__git_pager | LESS='-R' less"

    # git branch fzf: show last commit preview (take commit)
    __git_branch_commit_preview_cmd="xargs -I%% fzf_preview_git_commit %% |$__git_pager | LESS='-R' less"
    # git branch fzf: history preview (take branch name)
    __git_branch_history_preview_cmd="xargs -I$$  git log -100 --graph --color=always --format='%C(auto)%d %s %C(bold blue)<%an>%Creset %C(black)%C(bold)%cr%Creset' $$ -- | $__git_pager | LESS='-R' less"

  # fzf: git branch old way {{{4
    # Old way, can't take parameter
    # git branch fzf: show last commit preview
    # alias fzf_gb_commit_after_pipe='FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | cut -c3-1000 | $__git_branch_commit_preview_cmd" '
    # alias git_branch_interactive_preview_commit="git branch --sort=-committerdate --color=always | fzf_gb_commit_after_pipe"

    # git branch fzf: history preview
    # alias fzf_gb_history_after_pipe='FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | cut -c3-1000 | $__git_branch_history_preview_cmd" '
    # alias git_branch_interactive_preview_history="git branch --sort=-committerdate --color=always | fzf_gb_history_after_pipe"
    
  # fzf: git branch  }}}4
  # fzf: git branch functions {{{4

    __gb_response_clean_pipe() { 
      #  Can't use alias
      # alias -g __gb_response_clean_pipe="cut -c3-1000 | cut -f1 -d' ' | tr -d '\\\n' | pbcopy && pbpaste"
      while read data; 
      # Support pipe
      do; 
        # echo $data | cut -c3-1000 | cut -f1 -d' ' | tr -d '\n' | pbcopy && pbpaste
        # echo $data | eval $__gb_clean_cmd_str | tr -d '\n' | pbcopy && pbpaste 
        # Keep \n will merges all lines
        echo $data | eval $__gb_clean_cmd_str | pbcopy && pbpaste
      done; 
    }

    git_branch_interactive(){
      gb $@ \
        | FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | $__gb_clean_cmd_str | $__git_branch_history_preview_cmd" \
        | __gb_response_clean_pipe
      # This script is depending on format from gb
    }

    git_branch_interactive_preview_commit(){
      gb $@ \
        | FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | $__gb_clean_cmd_str | $__git_branch_commit_preview_cmd" \
        | __gb_response_clean_pipe
      # This script is depending on format from gb
    }

    # git branch remote + delete + by me
    git_branch_remote_interactive_select(){
      gbr $@ \
        | FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | $__gb_clean_cmd_str | $__git_branch_history_preview_cmd" \
        | __gb_response_clean_pipe
      # This script is depending on format from gbr
    }

    # ctrl-y not working since it is using different format
    git_branch_remote_interactive_select_name(){
      local __gbr2_clean_cmd_str="cut -d ' ' -f6";             # get commit SHA-1
      local __gbr2_clean_branch_name_cmd_str="cut -d ' ' -f4"; # get branch name

      gbr2 $@ \
        | FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | $__gbr2_clean_cmd_str | $__git_branch_history_preview_cmd" \
        | eval $__gbr2_clean_branch_name_cmd_str \
        | sed 's/origin\///' \
        | tr -d '\n' \
        | pbcopy && pbpaste
      # This script is depending on format from gbr2
    }

  # fzf: git branch  }}}4

    # Sam as function but function can take parameter
    alias gbi=git_branch_interactive
    alias gbic=git_branch_interactive_preview_commit    

    # If gbri is working, will deprecate dbri2 later
    alias gbri=git_branch_remote_interactive_select
    alias gbri2=git_branch_remote_interactive_select_name


    #======================== DELETE BRANCH ===============================
    # Delete branches local
    git_branch_delete_interactive(){ git_branch_interactive $@ | xargs -n 1 git branch -d }
    git_branch_delete_interactive_D(){ git_branch_interactive $@ | xargs -n 1 git branch -D }

    alias gbdi=git_branch_delete_interactive
    alias gbDi=git_branch_delete_interactive_D


    # Delete branches remote
    git_branch_remote_delete_interactive(){ git_branch_remote_interactive_select $@ | xargs -n 1 git push -d origin }
    alias gbrdi=git_branch_remote_delete_interactive

    # duplicate with gbrdi, choose one
    # alias gbd_remote='ee "git push -d origin"'
    # alias gbrdi='gbd_remote $(gbri)'

    #===== Branch clean up (House Clean) ======
    #
    # Removed remote merged branch 
    # alias gb_merged_remote="git for-each-ref --merged HEAD --sort=-committerdate refs/remotes/ --format='(%(color:green)%(committerdate:relative)%(color:reset)) %(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(authorname)' --color=always"
    # alias gb_merged_remote_me='ee "gb_merged_remote |grep chuan"'
    # alias gb-merged='git branch --merged'
    # Replaced by
    # gbdri --merged
    #
    #
    # Clean merged Branch (Delete local branches which are already merged)
    # alias gbd-merged-branch-local='git branch --merged | grep -v "\*" | xargs -n 1 git branch -d'
    # alias git-house-clean="echo gbd-merged-branch-local \ngb_merged_remote_me\n"
    # gbdi --merged


    #======================== SWITCH BRANCH ===============================
    alias gcobi='git_branch_interactive | xargs git checkout'
    # Git branch checkout remote
    alias gcobri="git_branch_remote_interactive_select | sed 's/origin\///' | xargs git checkout"
    # Same as gcobri
    alias gcobri2="git_branch_remote_interactive_select_name | xargs git checkout"


    # gbr show merged branched
    # Cleanup
    # Can be replaced with gbi --merged
    # alias gbri_merged="gb_merged_remote | fzf | cut -d ' ' -f6 | cut -c8-1000"
    # alias gbri_me="gbr | grep chuan | fzf | cut -d ' ' -f6 | cut -c8-1000"

  # fzf: git branch? }}}3

  # Git stash
    FORGIT_STASH_FZF_OPTS='
    --bind="ctrl-d:reload(git stash drop $(cut -d: -f1 <<<{}) 1>/dev/null && git stash list)"
    '
    __git_stash_preview() {
      git stash show --color=always --ext-diff $@
    }


}
# }}}2
#   FUNCTION: zsh_plugins_config {{{2
zsh_plugins_config() {
    
  __iterm2colors_functions_config() {
    ac_my_colors() {
        # Light Theme "ayu_light"
        local MYCOLORS=("Darkside" "OceanicMaterial" "Solarized Darcula" "Broadcast" "Desert" "DotGov" "Misterioso" "Galaxy" "AdventureTime" "AtelierSulphurpool" "Dracula")
        local RANDOM=$$$(gdate +%N)
        local NEW_COLOR=${MYCOLORS[RANDOM % ${#MYCOLORS[@]} + 1]}
        _iterm2colors_apply $NEW_COLOR
        mlog "COLOR THEME: " $_iterm2colors_current
    }

    # zsh-iterm2colors
    alias ic=_iterm2colors_apply
    alias icc='echo $_iterm2colors_current'
    alias icr=_iterm2colors_apply_random
    alias ic-light='ac "ayu_light"'
    alias ic-darkside='ac "Darkside"'
    alias ic-ocean='ac "OceanicMaterial"'
    alias ic-sd='ac "Solarized Darcula"'
    alias rc='ac_my_colors &&  echo "COLOR THEME: " $_iterm2colors_current'
  }

    alias fzfc='fzf | tr -d "\n" | pbcopy && pbpaste'


    # vi-mode
    export KEYTIMEOUT=1

    ## Fun
    alias test-passed='if [ "$?" -eq "0" ]; then lolcat ~/.tp -a -s 40 -d 2; fi;'

    # b4b4r07/enhancd
    ENHANCD_HYPHEN_NUM=100
    ENHANCD_DISABLE_HYPHEN=1;


  # zsh-vi-mode
  # The plugin will auto execute this zvm_after_init function
  # ZVM_INSERT_MODE_CURSOR=$ZVM_CURSOR_BLINKING_BLOCK
  function zvm_after_init() {
    # zsh_cust_bindkey
    ! (typeset -f ac_my_colors > /dev/null) &&  __iterm2colors_functions_config
    ac_my_colors
  }

}
#   FUNCTION: apply_my_config {{{2
apply_my_config() {
  zsh_plugins_config
  __fzf_config
  __ls_fuzzy_preview
  __fzf_ag_preview
  __fzf_git_config
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

# for i in $ZSH_CONFIG_HOME/snippets/*.zsh; do
#     source $i
# done

# SECTION: : ZSH PLAYGROUND {{{1
# --------------------------------------------------------------------------
_clean_my_cache() {
  # fd -d 1 --type d --changed-before=500d
  fd -d 1 . ~/work_credential/temp/cache --changed-before=20d | xargs trash
}

# _clean_my_cache
# SECTION: : Random after {{{1
# --------------------------------------------------------------------------

alias signs="open https://github.com/romkatv/powerlevel10k#what-do-different-symbols-in-git-status-mean"


# Sample Function
function zsh_sample_function {
  local clear list
  zparseopts -E c=clear l=list

  if [[ -n "$clear" ]]; then
    echo >&2 file deleted.
    echo "clear"
  elif [[ -n "$list" ]]; then
    echo "list"
  else
    echo "else"
  fi
}


# WIP
fzf_ls_widget() {

  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
  setopt localoptions pipefail no_aliases 2> /dev/null

  # --bind=ctrl-z:ignore
  LBUFFER="${LBUFFER}$(ls_fuzzy_preview)"
  local ret=$?

  zle reset-prompt
  return $ret
}
zle -N fzf_ls_widget

# WIP fh - repeat history
# FZF preview
function fh() {
  # print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')
  
  # Method 1
  # print -z $( ([ -n "$ZSH_NAME" ] && fc -li 1 || history) | \
  #   sed 's/^[ ]*//' | \
  #   fzf --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}' | \
  #   sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //' \
  #            )

  # Method 2
  # print -z $( ([ -n "$ZSH_NAME" ] && fc -li 1 || history) | \
  #   fzf --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}' | \
  #   sed 's/^[ ]*//' | sed 's/* /  /' | \
  #   sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //' \
  #            )
  
  # Method 3
  # fc -rli 0 > ~/.zsh_history_cache
  print -z $( cat ~/.zsh_history_cache | \
    sed 's/^[ ]*//' | \
    fzf --reverse --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}' | \
    sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //' \
             )


  # --reverse
}

# TODO a pip fzf preview, can search and view

# fc -li 1
#  cat temp| sed 's/^[ ]*//' |sed 's/* /  /' | fzf --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}'
#  cat temp| sed 's/^[ ]*//' |sed 's/* /  /' | fzf --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}' | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //'
# cat ~/.zsh_history | sed 's/;/  /'| fzf --with-nth 3..

# crontab crontab-config

# }}}


