#   __  ____     __  ______ _____ _    _ _____   _____
#  |  \/  \ \   / / |___  // ____| |  | |  __ \ / ____|
#  | \  / |\ \_/ /     / /| (___ | |__| | |__) | |
#  | |\/| | \   /     / /  \___ \|  __  |  _  /| |
#  | |  | |  | |     / /__ ____) | |  | | | \ \| |____
#  |_|  |_|  |_|    /_____|_____/|_|  |_|_|  \_\\_____|
#
#  My .zshrc to save some coffee time and keep my hair on my head

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  [[ $VS_TERM -ne "1" ]] && source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Section: Pre Init {{{1
# --------------------------------------------------------------------------
# Display a loading sign for zshrc

# ASYNC only support loading startup message
export IS_ASYNC=0
export START_MESSAGE=0
export LOADING_BAR=0
export ZPROF_TRACK=0
# export ZSH_PLUGIN_LOADED=0 # Disable ZSH PLUGIN # unset ZSH_PLUGIN_LOADED
# export VS_TERM = 0         # ENV to determine running env
#
# [[ $IS_ASYNC -ne "1" ]] && echo "synchronous load "

# Section: PATH {{{1
# --------------------------------------------------------------------------
  # PATH: Global PATH {{{2
  # --------------------------------------------------------------------------
export PATH=$HOME/bin:/usr/local/sbin:/usr/local/bin:$PATH
export PATH="$HOME/script:$PATH"
export PATH="$HOME/my_script:$PATH"
export PATH="$HOME/my_script/zsh:$PATH"
# export PATH="/usr/local/opt/maven@3.3/bin:$PATH"
export PATH="./node_modules/.bin:$PATH"
export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="/usr/local/opt/ruby/bin:$PATH"
export PATH="$HOME/.gem/ruby/2.7.0/bin:$PATH"
export PATH=$PATH:$HOME/.SpaceVim/bin

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
# export KUBECONFIG=$HOME/.kube/config2
# export KUBECONFIG=$HOME/.kube/config3

# export is required for python path
export NODE_PATH=/usr/lib/node_modules

[[ $ZPROF_TRACK -eq "1" ]] && zmodload zsh/zprof
[[ $LOADING_BAR -eq "1" ]] && revolver --style "bouncingBar" start "Loading zsh config"

# bat
export MANPAGER="sh -c 'col -bx | bat -l man -p'"

  # PATH: Global Parameter {{{2
  # --------------------------------------------------------------------------

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export TERM="xterm-256color"
export ZSH_LOADING_LOG=~/.startup.log
export MESSAGE_CACHE_BEFORE_PRINT=~/.startup_all.log
export WELCOME_MESSAGE=~/.welcome_message.log
export NVIM_LISTEN_ADDRESS=/tmp/spacevim_nvim_server

# }}}
# Section: pre script {{{1
# Script Before loading
# --------------------------------------------------------------------------
#=============================== pre script  ===========================================
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

mlog() {
    echo $@ >>$ZSH_LOADING_LOG
}

echoAndEval() { echo $@ && eval $@; }

alias -g ee=echoAndEval
alias -g timeElapsed="pv -F 'Elapsed time: %t'"
alias v='py ~/my_script/version.py ./'
power_v() {
    python ~/my_script/version.py ./ powermode
}

noti() {
    # terminal-notifier
    # Can use terminal-notifier if we want icon modification. Can't do noti confirm
    # Buildin noti
    osascript -e "display notification \"$1\" with title \"$2\""
}

# Section: ZSH ASYNC PATH {{{1
# --------------------------------------------------------------------------
# Async_load {{{2
# {{{3
async_load() {
}
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

    plugin_config

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
# Section: Theme Config {{{1
# --------------------------------------------------------------------------
# Set name of the theme to load.

# ZSH_THEME="agnoster-cus"
# ZSH_THEME="agnoster"
# ZSH_THEME="powerlevel9k/powerlevel9k"

# powerline 9k {{{2
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
# spaceship_power_version_init {{{2

spaceship_power_version_init() {
    SPACESHIP_POWER_VERSION_SHOW="${SPACESHIP_POWER_VERSION_SHOW=true}"
    SPACESHIP_POWER_VERSION_PREFIX="${SPACESHIP_POWER_VERSION_PREFIX="$SPACESHIP_PROMPT_DEFAULT_PREFIX"}"
    SPACESHIP_POWER_VERSION_SUFFIX="${SPACESHIP_POWER_VERSION_SUFFIX="$SPACESHIP_PROMPT_DEFAULT_SUFFIX"}"
    SPACESHIP_POWER_VERSION_SYMBOL="${SPACESHIP_POWER_VERSION_SYMBOL="🍷 "}"
    SPACESHIP_POWER_VERSION_COLOR="${SPACESHIP_POWER_VERSION_COLOR="white"}"

    # ------------------------------------------------------------------------------
    # Section power_version
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
# }}}1
# Section: ZSH plugin {{{1
# --------------------------------------------------------------------------
# Load very basic plugins
# Section: Antigen {{{2
# antigen-1{{{3
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

# source ~/script/antigen.zsh
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
# Section: Antibody {{{2
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
# Section: Zinit {{{2
zinit_load() {
  source ~/.zinit/bin/zinit.zsh

  autoload -Uz _zinit
  (( ${+_comps} )) && _comps[zinit]=_zinit

  zinit for \
    OMZ::lib/git.zsh \
    OMZ::lib/functions.zsh \
    OMZ::lib/history.zsh \
    OMZ::lib/directories.zsh \
    OMZ::plugins/git/git.plugin.zsh \
    OMZ::plugins/git-extras/git-extras.plugin.zsh \
    OMZ::plugins/systemd/systemd.plugin.zsh \
    OMZ::plugins/iterm2/iterm2.plugin.zsh \
    OMZ::plugins/kubectl/kubectl.plugin.zsh \
    OMZ::plugins/vi-mode/vi-mode.plugin.zsh

  # sudo chmod 777 /private/tmp
  # sudo chmod +t /private/tmp
  # This solves problem in Catalina

  # Disabled because of permission error in Catalina
  #  OMZ::plugins/kubectl/kubectl.plugin.zsh

  zinit light zdharma/history-search-multi-word
  zinit light shayneholmes/zsh-iterm2colors
  zinit light psprint/zsh-cmd-architect
  zinit light rupa/z
  zinit light changyuheng/fz
  zinit light wfxr/forgit
  zinit light vim/vim
  zinit light Dabz/kafka-zsh-completions
  zinit light djui/alias-tips

  zinit light AdrieanKhisbe/diractions
  zinit light zpm-zsh/template

  # zinit ice wait "0" ver"dev"
  # zinit light zsh-vi-more/evil-registers
  # ZSH_EVIL_COPY_HANDLERS[+]="pbcopy"
  # ZSH_EVIL_PASTE_HANDLERS[+]="pbpaste"
  # "+p

  zinit light b4b4r07/enhancd
  
  # autoload -Uz compinit; compinit # zinit 用户这里可能是 zpcompinit; zpcdreplay
  zpcompinit; zpcdreplay

  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

  zinit load Aloxaf/fzf-tab
  # zinit snippet https://raw.githubusercontent.com/lincheney/fzf-tab-completion/master/zsh/fzf-zsh-completion.sh
  
  zinit light zsh-users/zsh-autosuggestions
  zinit light zdharma/fast-syntax-highlighting

  zinit ice depth=1; zinit light romkatv/powerlevel10k

  # zinit update # Update plugin or snippet
  # zinit self-update updates zinit
}
# }}}2
# }}}1

# [[ $IS_ASYNC -eq "1" ]] && async_cust_init
# async_load0
# load_Antigen
zinit_load

# Section: Script Tools {{{1
# --------------------------------------------------------------------------
# Tool: Random {{{2
# --------------------------------------------------------------------------
color-test() {
    clear
    cat ${HOME}/script-tool/iterm-syntax-test.txt
}

c-bash() {
    if [ $# -eq 0 ]; then
        echo "Create Bash Script"
        return
    fi
    touch $1
    chmod +x $1
    echo "#!/bin/bash\n" >>$1

}
alias cbash=c-bash


sample_function() {
    if [ $# -eq 0 ]; then
        echo "Sample function"
        return
    fi
    echo $@
    echo $1
    echo $2
}

# FileSearch
f() {echo 'find . -iname "*'$1'*" '${@:2} && find . -iname "*$1*" ${@:2} }
r-old() {echo ' grep "'$1'" '${@:2}' -R .' && grep "$1" ${@:2} -R . }
r() {echo 'Replaced with ag'}

# Watch function, replaced by watch
mywatch() {
    while :; do
        a=$($@)
        clear
        echo "$(date)\n\n$a"
        sleep 2
    done
}

mywatch2() {
    while :; do
        a=$($@)
        clear
        echo "$(date)\n\n$a"
        sleep 4
    done
}

mywatch-no-clean() {
    while :; do
        a=$($@)
        echo "$a"
        sleep 2
    done
}

#watch-zookeeper {while :; do clear; echo "$(date)\n\n$(echo stat |nc localhost 2181)"; sleep 1;  done}
#watch-zookeeper2 {while :; a=$@; do clear; echo "$(date)\n\n$(echo stat |nc $a 2181)"; sleep 1;  done}
#watch-zookeeper-cnumber {while :; do clear; echo "$(date)\n\n$(echo stat | nc localhost 2181 |grep 127.0.0.1 |wc -l)"; sleep 1;  done}


# fzf with Brew {{{
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

#}}}

# alternative using ripgrep-all (rga) combined with fzf-tmux preview
# implementation below makes use of "open" on macOS, which can be replaced by other commands if needed.
# allows to search in PDFs, E-Books, Office documents, zip, tar.gz, etc. (see https://github.com/phiresky/ripgrep-all)
# find-in-file - usage: fif <searchTerm> or fif "string with spaces" or fif "regex"
fif() {
    if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
    local file
    file="$(rga --max-count=1 --ignore-case --files-with-matches --no-messages "$@" | fzf-tmux +m --preview="rga --ignore-case --pretty --context 10 '"$@"' {}")" && open "$file"
}

# using ripgrep combined with preview
# find-in-file - usage: fif <searchTerm>
fif0() {
  if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
  rg --files-with-matches --no-messages "$1" | fzf --preview "highlight -O ansi -l {} 2> /dev/null | rg --colors 'match:bg:yellow' --ignore-case --pretty --context 10 '$1' || rg --ignore-case --pretty --context 10 '$1' {}"
}

# fkill - kill processes - list only the ones you can kill. Modified the earlier script.
fkilll() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
    else
        pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}

# Tool: B Deprecated {{{2
# --------------------------------------------------------------------------
#========================= Other helper script ================================
#
# Media
# Convert m4a/wma to mp3
# mkdir newfiles
# for f in *.m4a; do ffmpeg -i "$f" -codec:v copy -codec:a libmp3lame -q:a 2 newfiles/"${f%.m4a}.mp3"; done
# for f in *.wma; do ffmpeg -i "$f" -codec:v copy -codec:a libmp3lame -q:a 2 newfiles/"${f%.wma}.mp3"; done

#
## Hacky, this might help with jenkin login
#if [ $(which docker-credential-osxkeychain) ]; then
#        unlink $(which docker-credential-osxkeychain)
#fi

# test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
#}}}

# Section: Dev functions/tools {{{1
# --------------------------------------------------------------------------
# Section: Docker functions {{{2
# --------------------------------------------------------------------------
# Docker ps pretty
docker-ps() {
    docker ps $@ --format 'table{{ .ID }}\t{{ .Names }}\t{{ .Status }}\t'
}

docker-ps-more() {
    # docker ps $@ --format 'table{{ .ID }}\t{{ .Names }}\t{{ .Status }}\t{{ .Image }}\\t{{.Command}}\\t{{.RunningFor}}'
    docker ps $@ --format 'table {{ .Names }}\t {{ .ID }}\t {{ .Command }}\t {{ .RunningFor }}\t {{ .Status }}\t {{ .Size }}\t {{ .Mounts }}\t {{.Image}}\t'

}

# Docker ps pretty with port
docker-ps-port() {
    docker ps $@ --format 'table{{ .Image }}\t{{ .Names }}\t{{ .Status }}\t{{ .Ports }}' | awk '
    NR % 2 == 0 {
      printf "\033[0m";
    }
    NR % 2 == 1 {
      printf "\033[1m";
    }
    NR == 1 {
      PORTSPOS = index($0, "PORTS");
      PORTS = "PORTS";
      PORTSPADDING = "\n";
      for(n = 1; n < PORTSPOS; n++)
        PORTSPADDING = PORTSPADDING " ";
    }
    NR > 1 {
      PORTS = substr($0, PORTSPOS);
      gsub(/, /, PORTSPADDING, PORTS);
    }
    {
      printf "%s%s\n", substr($0, 0, PORTSPOS - 1), PORTS;
    }
    END {
      printf "\033[0m";
    }
    '
}

# Print in vertical
docker-ps-vertical-old() {
    local FORMAT="ID\t{{.ID}}\nNAME\t{{.Names}}\nIMAGE\t{{.Image}}\nSTATUS\t{{.Status}}\nRunningFor\t{{.RunningFor}}\nCOMMAND\t{{.Command}}\nPORTS\t{{.Ports}}\nLABEL\t{{.Labels}}\nMounts\t{{.Mounts}}\nNetworks\t{{.Networks}}\nSize\t{{.Size}}\n\n"
    docker ps --filter name=$@ -a --format="$FORMAT" --no-trunc
}

docker-ps-vertical() {
    local FORMAT="ID\t{{.ID}}\nNAME\t{{.Names}}\nIMAGE\t{{.Image}}\nSTATUS\t{{.Status}}\nLIVE\t{{.RunningFor}}\nCOMMAND\t{{.Command}}\nPORTS\t{{.Ports}}\nLABEL\t{{.Labels}}\nMOUNTS\t{{.Mounts}}\nNETS\t{{.Networks}}\nSIZE\t{{.Size}}\n\n"
    docker ps --filter name=$@ -a --format="$FORMAT" --no-trunc | awk '
    NR == 1 {
      PORTSPADDING = "\n";
      for(n = 1; n < 9; n++)
          PORTSPADDING = PORTSPADDING " ";
    }
    NR > 1 {
    }
    {
        if ($1 ~ "PORT") {
            POS = index($0, $2);
            DATA = substr($0, POS);
            gsub(/, /, PORTSPADDING, DATA);
            printf "\033[1m%s\033[0m%s\n", substr($0, 0, POS - 1), DATA;
        } else if ($1 ~ "LABEL" || $1 ~ "MOUNTS") {
            POS = index($0, $2);
            DATA = substr($0, POS);
            gsub(/,/, PORTSPADDING, DATA);
            printf "\033[1m%s\033[0m%s\n", substr($0, 0, POS - 1), DATA;
        } else if ($1 ~ "ID" || $1 + "IMAGE") {
            POS = index($0, $2);
            printf "\033[1m%s\033[0m\033[4m%s\033[0m\n", substr($0, 0, POS - 1), $2;
        } else {
            POS = index($0, $2);
            printf "\033[1m%s\033[0m%s\n", substr($0, 0, POS - 1), $2;
        }
    }
    END {
      printf "\033[0m";
    }
    '
}

# Docker exec
docker-exec-name() {
    echo "Docker exec -it by name"
    firstmatch=$(docker ps -q --filter name=$@ | head -1)
    dpsv -f id=$firstmatch
    docker exec -it $(docker ps -q --filter name=$@ | head -1) bash
}

# Docker stats
docker-stats() { docker stats $@ --format "table {{.Name}}\t{{.Container}}\t{{.CPUPerc}}\t{{.MemPerc}}\t{{.MemUsage}}\t{{.NetIO}}\t{{.BlockIO}}"; }

docker-stats-peek() {
    if [ $# -eq 0 ]; then
        docker stats --no-stream
    else
        docker stats --no-stream | grep $1
    fi
}

### }}}
# Section: Docker Alias {{{2
# --------------------------------------------------------------------------

alias mvimr='mvim --remote'
alias vi='nvim'
alias vim='nvim'
alias vimdiff="nvim -d"
alias dps=docker-ps
alias dpsa='dps -a'
alias dpss=docker-ps-more
alias dpsv=docker-ps-vertical
alias dpsp=docker-ps-port
alias dexec=docker-exec-name
alias dki='docker images'
alias dkin='docker inspect'
alias dstats=docker-stats
alias dks=docker-stats
alias dksp=docker-stats-peek

# Docker clean
alias docker-stop='echo "Docker stop all containers" && docker stop $(docker ps -q)'
alias dkstop=docker-stop
alias dkprune='docker system prune -af'
alias docker-clean='ee "docker system prune"'
alias docker-clean-unused='ee "docker system prune --all --force --volumes"'
alias docker-clean-stop-all='docker stop $(docker container ls -a -q) && docker system prune -a -f --volumes'

# More alias from https://hackernoon.com/handy-docker-aliases-4bd85089a3b8
alias dk='docker'
alias dks='docker service'
alias dkrm='docker rm'
alias dkl='docker logs'
alias dklf='docker logs -f'
alias dkflush='docker rm `docker ps --no-trunc -aq`'
alias dkflush2='docker rmi $(docker images --filter "dangling=true" -q --no-trunc)'

# More
dkln() {
    docker logs -f $(docker ps | grep $1 | awk '{print $1}')
}

dke() {
    docker exec -it $1 /bin/sh
}

dkexe() {
    docker exec -it $1 $2
}

dkreboot() {
    osascript -e 'quit app "Docker"'
    countdown 2
    open -a Docker
    echo "Restarting Docker engine"
    countdown 120
}

# }}}
# Section: Docker old {{{2
# --------------------------------------------------------------------------
#============================== Docker old ===============================
#export DOCKER_HOST=tcp://192.168.59.103:2375
#unset DOCKER_HOST
#unset DOCKER_CERT_PATH
#unset DOCKER_TLS_VERIFY
# }}}
# Section: Local wildfly {{{2
# --------------------------------------------------------------------------
# Unfinished
## export JBOSS_HOME=/usr/local/opt/wildfly-as/libexec
## export PATH=${PATH}:${JBOSS_HOME}/bin

pre-wildfly() {
    echo "export JBOSS_HOME"
    export JBOSS_HOME=${HOME}/Downloads/wildfly-10.1.0.Final
    ## export PATH=${PATH}:${JBOSS_HOME}/bin
    export WILDFLY_DEPLOY=${JBOSS_HOME}/standalone/deployments
}

# export WILDFLY_HOME=${HOME}/CENX/wildfly-10.1.0.Final
# Wildfly created by Eclipse
# export WILDFLY_HOME=~/Applications/wildfly-10.1.0.Final
export WILDFLY_HOME=~/Applications/wildfly-12.0.0.Final
export WILDFLY_DEPLOY=${WILDFLY_HOME}/standalone/deployments
export WILDFLY_BIN=${WILDFLY_HOME}/bin

export ZOOKEEPER_INSTANCES=LOCAL
export ZOOKEEPER_LOCAL_HOST=LOCALHOST
export ZOOKEEPER_LOCAL_CLIENT_PORT=2181

alias wildfly-standalone='$WILDFLY_BIN/standalone.sh -b=172.17.0.1'
alias wildfly-standalone-deploy-clean='echo "Clean wildfly deploy directory" && rm $WILDFLY_DEPLOY/*.war*'
alias wildfly-standalone-deploy-war="echo 'deploy war' && cp ./*.war $WILDFLY_DEPLOY"
alias wildfly-standalone-deploy-target-war="echo 'deploy war from target' && cp target/*.war $WILDFLY_DEPLOY"
alias wildfly-standalone-deploy-all="echo 'deploy war' && cp *.war $WILDFLY_DEPLOY"
alias wildfly-standalone-deploy-war-enable="echo 'enable app from deployment' && rm $WILDFLY_DEPLOY/*.undeployed && rm $WILDFLY_DEPLOY/*.failed"
alias wildfly-standalone-deploy-war-disable-all="echo 'disable app from ' $JBOSS_HOME && rm $WILDFLY_DEPLOY/*.deployed"
alias wildfly-standalone-restart='$WILDFLY_BIN/jboss-cli.sh -c --command=":shutdown(restart=true)"'

alias wfsstart=wildfly-standalone

# }}}1
# Section: Alias {{{1
# --------------------------------------------------------------------------

# Alias
# alias vi='/usr/local/bin/vim'
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
# alias rs="ee 'source ~/.zshrc'"
alias rs='source ~/.zshrc'
alias rsl='source ~/.zshrc-local.sh'
alias rst='unset ZSH_PLUGIN_LOADED && zsh'
alias mz='nvim ~/.zshrc && shfmt -i 4 -s -w -ci ~/.zshrc'
alias ca="less ~/.zshrc"

alias mvimdiff="mvim -d"

alias mysql="/Applications/XAMPP/xamppfiles/bin/mysql --use=root"
alias notes="mvim ~/repo/Notes/CLojure.md"

alias kafka21="cd /usr/local/kafka_2.12-2.1.0"
alias kafka08="cd /usr/local/kafka_2.9.1-0.8.2.2"

alias kafka21="cd /usr/local && ln -s kafka_2.12-2.1.0 kafka"
alias kafka08="cd /usr/local && ln -s kafka_2.9.1-0.8.2.2 kafka"

#============= Global alias is danger =============
# alias -g Gc=' --color=always | grep -i'
# alias -g G='| grep -i'
# alias -g WC='| wc -l'
# alias -g TF='| tail -f'
# alias -g F='| fzf | tr -d "\n" | pbcopy && pbpaste'
# alias -g C='| pbcopy && pbpaste'

#============= Applications =============
alias copen='open -a Google\ Chrome'
alias ems='open -a /Applications/Emacs.app $@'
alias py='python'
alias rmt='/bin/rm'
alias rm="trash"

alias em='ems'
alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
# Cheatsheet
alias cidea='cat ~/duqcyxwd/DotFiles/vim/ideavim-cheatsheet | grep $@'

#alias sql="echo 'psql to localhost' && psql -h 'localhost' -U 'postgres'"
alias sql="echo 'psql to localhost' && ee \"export PAGER='less -SF' && psql -h 'localhost' -U 'postgres'\""

#============= Services =============
alias zkcli0=/usr/local/Cellar/zookeeper/3.4.13/bin/zkCli


# Alias for echo done
alias tf='echo "Task finished"'

#============= Dir alias =============
# CD to any directory with auto complete
local _repodir="${HOME}/code/"
c() {cd $_repodir$1}
compctl -/ -W $_repodir c

local _gh_dir="${HOME}/github/"
gh() {cd $_gh_dir$1}
compctl -/ -W $_gh_dir gh

#============= Powerful and Common alias =============
alias cpwd='echo "copy currenty directory" && pwd |pbcopy'
#Copy file path to clipboard
cpath() {greadlink -f $1 | tr -d '\n' | pbcopy }
alias cf='pbpaste | pbcopy' # clean format of clipboard
alias dir='dirs -v'

#============= Kubernetes alias =============
alias kexecit='kubectl exec -it'
if [ -f $HOME/.kube/KUBE_NS ]; then
    export KUBE_NS=$(cat $HOME/.kube/KUBE_NS)
fi

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

# ns_clean dev

# helm ls | grep $KUBE_NS | cut -f1 | hpurge && kdns $KUBE_NS
hpurge() { 
  # Support pipe
  while read data; 
  do; 
    helm del --purge $data; 
  done; 
}

ns-clean() {
    if [ $# -eq 0 ]; then
        echo "Require namespace"
        return
    fi
    echo "Will clean namespace $1"
    # helm ls --namespace $1 | /usr/bin/grep $1 | cut -f1 | hpurge && kubectl delete namespace $1 && kubectl get pods --namespace $1
    kubectl delete namespace $1 && kubectl get pods --namespace $1
}

ns-cleani() {
  local KUBE_NS_TO_DELTE="$(kubectl get namespaces | awk 'NR>1' | fzf -0 | awk '{print $1}')"
  [[ ! -z "$KUBE_NS_TO_DELTE" ]] && ns-clean $KUBE_NS_TO_DELTE
}


# Or
# KUBE_NS=dev
# hpurge() { while read data; do; helm del --purge $data; done; }
# helm ls --namespace $KUBE_NS | /usr/bin/grep $KUBE_NS | cut -f1 | hpurge && kubectl delete namespace $KUBE_NS 

alias hdelp='helm del --purge'

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

agbat_pip() {
    while read data; do
        fn=$(echo $data | cut -f 1 -d ":")
        line=$(echo $data | cut -f 2 -d ":")
        echo "$fn"
        echo "$line"
        bat $fn -r $line:$(($line+200))
    done
}

# Section: Git {{{1
# --------------------------------------------------------------------------
# Git config {{{2
git config --global color.ui true
git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.ci commit
git config --global alias.st status
git config --global alias.a add
git config --global alias.root "rev-parse --show-toplevel"
# Configure git as my personal repo
alias config-git-local="ee \"git config --local user.name 'Yongqinchuan Du' && git config --local user.email 'duqcyxwd@gmail.com'\""


# Git functions {{{2
# --------------------------------------------------------------------------
#=== Special Git Tool  ====
get_git_current_branch() { git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'; }

# gb --show-current
get_current_branch() { git rev-parse --abbrev-ref HEAD }

git_create_branch() {
    if [ -z "$1" ]; then
        echo "*******************************************"
        echo "*   !!! WARNING !!!  Branch not created   *"
        echo "*******************************************"
        echo ""
        echo "Silly me!!! I need to specify a parameter, the branch.."
    else
        current_branch=$(get_git_current_branch)
        set -x
        git checkout -b $1
        if [ $? -eq 0 ]; then
            git push --set-upstream origin $1
            if [ $? -ne 0 ]; then
                git checkout ${current_branch}
                git branch -D $1
            fi
        fi
        set +x
    fi
}

gitopen-current-branch() {
    ee "gitopen -b $(get_current_branch)"
}

git-blame() {
    ruby ~/repo/DotFiles/otherTool/git-blame-colored $1 | less -R
}


# Delete local and remote branch
git-branch-delete-remote-current-branch() {
  if [ $# -eq 0 ]; then
    echo "Require branch"
    return
  fi
  local branch="$(get_current_branch)"
  git checkout -
  ee "git branch -d $branch"
  ee "git push -d $branch"
}



# Git Alias {{{2
# --------------------------------------------------------------------------

# git pull --rebase is shorthand for a fetch and a rebase!
# Use git rebase instead of git merge
# https://learngitbranching.js.org/
# alias gl="git pull rebase" Not working for develop branch
alias gl="echo 'use gupv for feature branch update' && git pull --ff-only"

alias git-tag-tips="echo ' git tag v1.0.0 \n git tag -a v1.2 9fceb02 \n git push origin v1.5 \n git push origin --tags'"
alias git-hidden="git ls-files -v | grep '^[a-z]' | cut -c3-"
alias git-hide='ee "git update-index --assume-unchanged"'
alias git-unhide-all='ee "git update-index --really-refresh"'
alias git-update-all='find . -type d -depth 1 -exec git --git-dir={}/.git --work-tree=$PWD/{} pull origin master \;'


alias gbcopy="echo 'Copy current branch name' && git rev-parse --abbrev-ref HEAD |pbcopy && git branch"

alias gbc="git create-branch"
gbcd() { git fetch origin develop:$@ }

# Create cust gco for cust completion
git_checkout_branch_cust() { git checkout $@ }
alias gcob=git_checkout_branch_cust


gb_format="%(HEAD) %(align:60,left)%(color:yellow)%(refname:short)%(color:reset)%(end) - %(align:15,left)%(authorname)%(end) %(align:19,right)%(color:black)%(committerdate:relative)%(color:reset)%(end) %(color:red)%(objectname:short)%(color:reset)"
alias gb="git branch --format=\"$gb_format\" --sort=-committerdate --color=always"

# alias gbr='git branch --remote'
alias gbr="git for-each-ref --sort=-committerdate refs/remotes/ --format='(%(color:green)%(committerdate:relative)%(color:reset)) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(authorname)' --color=always"
alias gbrr="ee 'gbr |grep r/'"                                      # ggsup or gpsup  git create-branch -r development


#===== Branch clean up ======
# Check branch
alias gb_merged_remote="git for-each-ref --merged HEAD --sort=-committerdate refs/remotes/ --format='(%(color:green)%(committerdate:relative)%(color:reset)) %(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(authorname)' --color=always"
alias gb_merged_remote_me='ee "gb_merged_remote |grep chuan"'
alias gb-merged='git branch --merged'

# Clean merged Branch
alias gbd-merged-branch-local='git branch --merged | grep -v "\*" | xargs -n 1 git branch -d'
alias gbd_remote='ee "git push -d origin"'
alias git-house-clean="echo gbd-merged-branch-local \ngb_merged_remote_me\n"


# Others
alias gcobr='echo "Create branch and remote branch| Stop using this one, use push remote instead" & git_create_branch'
alias gcobr2='git create-branch -r development'

alias gcam='git commit --amend'

alias gre="ee 'git recent | head'"
alias grec="ee 'git recent | grep -i chuan | grep -v gone'"
alias gstau='git stash -u'
alias gstaa='echo "Tip: use gstp instead\n git stash apply" && git stash apply'
alias gru="ee 'git remote update origin --prune'"

#======================= Git Alias for work  =========================================

# alias gbui="echo 'git branch update with integration' && git fetch -p && git merge origin/integration"
alias gbud="echo 'git branch update with develop' && git pull origin develop"

# alias gcoi="git checkout integration && git pull"
alias gcod="git checkout develop && git merge origin/develop --ff-only"
# The following way will remove my unstashed change
# alias gcod="git checkout develop && git reset origin/develop --hard"

alias gf='git fetch --prune'
alias gfco='git fetch -p && git checkout'
alias gitf='open -a GitFiend --args $(git rev-parse --show-toplevel)'

# run gitk
alias gk="ee 'gitk --all&'"

# git diff
git_pager=$(git config core.pager || echo 'cat')
alias gdd="{git diff --stat --color origin/develop.. && git diff --color origin/develop.. } | ${git_pager}"
alias gitxdd="git diff origin/develop.. | gitx"
alias gds="ee 'git diff -w --stat'"


# Delete all branchs excep current branch
# alias gbdelete-all='gb | grep "f/CD" | grep -v "\*" |xargs -n 1 git branch -D'

# }}}}


# git clean up
#clean all but the stuff the stuff that we would like preserved like .ccache, xmls catalog etc
#clean -dxf will wipe everything requiring user to source gitenv again
#alias gclean='pushd $MY_GIT_TOP > /dev/null && git submodule foreach --recursive 'git clean -xdf' && git clean -xdf -e .ccache -e .flex_dbg -e remap_catalog.xml && popd > /dev/null'

# Section: Some Autocompletion {{{1
# --------------------------------------------------------------------------
# Autocompletion for teamocil
compctl -g '~/.teamocil/*(:t:r)' itermocil

# }}}
# Section: Lazy Plugin Load {{{1
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

# Lazy load example {{{2
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

# Section: ZSH plugin config {{{1
# --------------------------------------------------------------------------
# tmux config{{{2
__tmux_config() {
    ZSH_TMUX_ITERM2=true
    ZSH_TMUX_AUTOCONNECT=true

    # Tmuxinator
    export EDITOR='nvim'

    alias tmuxt='unset ZSH_PLUGIN_LOADED && /usr/local/bin/tmux'
    alias tca='tmux -CC attach -t'
    alias tcad='tmux -CC attach -d -t' #Detach other client
    alias tcs='tmux -CC new-session -s'

    source ~/script/tmuxinator/completion/tmuxinator.zsh        #tmuxinator
}
# fzf config {{{2
# -------------------------------------------------------
__fzf_config() {
    # require fzf junegunn/fzf

    local FZF_COLOR_SCHEMA_BORDER="--color 'fg:#bbccdd,fg+:#ddeeff,bg:#334455,preview-bg:#223344,border:#778899' --border"
    local FZF_COLOR_SCHEMA_CLEAN="--color=dark --color=fg:-1,bg:-1,hl:#5fff87,fg+:-1,bg+:-1,hl+:#ffaf5f --color=info:#af87ff,prompt:#5fff87,pointer:#ff87d7,marker:#ff87d7,spinner:#ff87d7"

    local FZF_PREVIEW_DIR='exa --group-directories-first -F --icons --group-directories-first -T -lh -L 2 --color=always {}'
    local FZF_PREVIEW_FILE='bat --style="numbers,changes" --color=always {} -r 0:200| head -200'
    export FZF_AG_BAT_PREVIEW="echo {} | cut -d ":" -f1 | head -1| xargs -I% bat --color always --pager never %"

    export FZF_TMUX_HEIGHT=80%        #Aslo been used by fzf-tab
    export FZF_DEFAULT_OPTS_WITHOUT_COLOR="
    --reverse 
    --ansi 
    -m 
    --cycle 
    --bind '?:toggle-preview' 
    --bind 'right:toggle+down' 
    --bind 'tab:down' 
    --bind 'btab:up' 
    --bind='ctrl-k:preview-up'
    --bind='ctrl-j:preview-down'
    --bind='ctrl-s:toggle-sort' 
    --bind='ctrl-w:toggle-preview-wrap'"

    export FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS_WITHOUT_COLOR $FZF_COLOR_SCHEMA_CLEAN"

    export FZF_CTRL_T_OPTS="--preview \"${FZF_PREVIEW_FILE}\" $FZF_COLOR_SCHEMA_BORDER "                          #fzf file
    export FZF_ALT_C_OPTS="--preview \"${FZF_PREVIEW_DIR}\""                                                      #fzf cd Folder

    # Setting fd as the default source for fzf
    if [ $commands[fd] ]; then
      # Use fd (https://github.com/sharkdp/fd) instead of the default find
      export FZF_DEFAULT_COMMAND="fd --type file --color=always"
      export FZF_ALT_C_COMMAND="fd --type d --color=always"
      export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    fi

    # Options to fzf command
    # export FZF_COMPLETION_OPTS='+c -x'

    # Use fd (https://github.com/sharkdp/fd) instead of the default find
    # command for listing path candidates.
    # - The first argument to the function ($1) is the base path to start traversal
    # - See the source code (completion.{bash,zsh}) for the details.
    _fzf_compgen_path() {
      fd --hidden --follow --exclude ".git" . "$1"
    }

    # Use fd to generate the list for directory completion
    _fzf_compgen_dir() {
      fd --type d --hidden --follow --exclude ".git" . "$1"
    }

  # fzf with ls fzfpreview  {{{
  # fd_search_cur_dir{{{
    export FD_SEARCH_CUR_DIR_DEPTH=1
    fd_search_cur_dir_dir() {
      fd -d $FD_SEARCH_CUR_DIR_DEPTH --hidden --no-ignore-vcs --color=always --type file
    }

    fd_search_cur_dir_file() {
      fd -d $FD_SEARCH_CUR_DIR_DEPTH --hidden --no-ignore-vcs --color=always --type directory
    }

    fd_search_cur_dir() {
      # fd -d $FD_SEARCH_CUR_DIR_DEPTH --hidden --no-ignore-vcs --color=always
      { fd_search_cur_dir_file && fd_search_cur_dir_dir}
    }
  # }}}

  # short_pwd{{{
    SHORT_PWD_LENGTH=38
    short_pwd() {
      local pwd_str="$(pwd)"
      if [[ ${#pwd_str} -gt $SHORT_PWD_LENGTH ]]; then
        echo $pwd_str | awk -v len=${SHORT_PWD_LENGTH} '{print "..."substr($0,length($0)-len,len+1)"/"}'
      else
        echo "${pwd_str}/"
      fi
    }
  # }}}
  # ls-fuzzy-preview {{{
    ls-fuzzy-preview() {
      while out=$( fd_search_cur_dir | FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS_WITHOUT_COLOR $FZF_COLOR_SCHEMA_BORDER" fzf-tmux -0 --preview "quick-preview {}" --exit-0 --expect=ctrl-i,ctrl-o,ctrl-space,enter,alt-left,alt-right,alt-up,alt-down --print-query --header "[${FD_SEARCH_CUR_DIR_DEPTH}]:$(short_pwd)" --preview-window right:50% --height ${FZF_TMUX_HEIGHT:-100%});
      do
        echo $out
        searchTerm=$(head -1 <<< "$out"| tail -1)
        key=$(head -2 <<< "$out"| tail -1)
        input=$(head -3 <<< "$out"| tail -1)

        if [[ "$key" == 'alt-left' ]]; then
          cd ../
        elif [[ "$key" == 'alt-right' ]] || [[ "$key" == 'alt-down' ]]; then
          pushd -1 1>/dev/null;
        elif [[ "$key" == 'alt-up' ]]; then
          pushd +0 1>/dev/null;
        elif [[ "$key" == 'ctrl-i' ]]; then
          FD_SEARCH_CUR_DIR_DEPTH=$(expr $FD_SEARCH_CUR_DIR_DEPTH + 1)
        elif [[ "$key" == 'ctrl-o' ]]; then
          local new_dept=$(expr $FD_SEARCH_CUR_DIR_DEPTH - 1)
          if [[ "$new_dept" -ge "1" ]];then
            FD_SEARCH_CUR_DIR_DEPTH=$new_dept
          fi
        elif [[ "$key" == 'ctrl-space' ]] || ; then
          if [[ -d "${input}" ]] ; then
            cd "${input}"
          elif [[ -f "${input}" ]]; then
            nvim $input
          fi
        elif [[ "$key" == 'enter' ]]; then
          fzf-exec $input
          exit
        fi
      done
      FD_SEARCH_CUR_DIR_DEPTH=1
    }
  # }}}

    export FZF_PREVIEW_ALL_PREVIEW_OPTION="--preview \"quick-preview {}\" $FZF_COLOR_SCHEMA_BORDER "
    alias fzfpp='eval "fzf $FZF_DEFAULT_OPTS $FZF_PREVIEW_ALL_PREVIEW_OPTION"'
    alias lf0="fd_search_cur_dir | fzfpp"

    alias lf=ls-fuzzy-preview
    alias fp=lf
    # }}}

  # fzf with ag {{{
    fag() {
      if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
      # --bind=\"ctrl-o:execute-silent(echo {} | agcode_open )\"
        # --bind=\"ctrl-l:execute-silent(echo {} | pbcopy )\"
    
      local FZF_BIND_OPTS=" --bind=\"ctrl-space:execute($FZF_AG_BAT_PREVIEW | LESS='-R' less)\"
        --bind=\"ctrl-v:execute(echo {} | agnvim_open )\"
        --bind=\"ctrl-o:execute( fzf-exec {} )\"
        --bind=\"ctrl-y:execute-silent(echo {} | cut -d ':' -f1,2 | xargs | tr -d '\\\n' | pbcopy )\"
        --header \"ctrl-o:fzfexec, ctrl-v:nvim, ctrl-y:pbcopy, ctrl-l:copy whole line\"
      "

      # -0 exit when no match
      # -1 Automatically select the only match 
      ag --nobreak --noheading $@ | FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS $FZF_COLOR_SCHEMA_BORDER $FZF_BIND_OPTS"  fzf-tmux -0 --preview "agbat {}"

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

    #}}}

} 
# }}}2
# fzf_git_config {{{2
__fzf_git_config(){
    # Unalias {{{
    # --------------------------------------------------------------------------
    unalias -m glg
    unalias -m glgp
    unalias -m glgg
    unalias -m glgga
    unalias -m glgm
    unalias -m glo
    unalias -m glol
    unalias -m glols
    unalias -m glod
    unalias -m glods
    unalias -m glola
    unalias -m glog
    unalias -m gloga
    unalias -m glp
    # }}}
    # antigen bundle 'wfxr/forgit'


    __git_pager=$(git config core.pager || echo 'cat')                           # to use diff-so-fancy

    forgit::stash::show_cust() {
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

  # Fzf alias {{{
    alias fga='forgit::add'
    alias fgrs='forgit::restore'
    alias fgclean='forgit::clean'
    alias fgd='forgit::diff'
    # alias fgl='forgit::log'
    # alias fgss='forgit::stash::show'
    alias fgrh='forgit::reset::head'
    alias fgi='forgit::ignore'

    alias gai=forgit::add
    alias gcleani=fgclean
    alias gdi=forgit::diff
    # alias glopi=forgit::log           # replaced with my git log
    # alias gstsi=forgit::stash::show
    alias gstsi=forgit::stash::show_cust
    alias grhi=forgit::reset::head

    alias fa=fga
    alias fdd=fgd
    alias frh=fgrh
    alias fl=fgl
    alias fclean=fgclean
    alias fss=fgss
    # }}}
    
    alias glos='git log --stat'
    alias glog='git log --graph'
    # git log with author
    alias gloo="git log --graph --pretty='%Cred%h%Creset -%C(auto)%d%Creset %s %C(bold blue)<%an>%Creset %C(black)%C(bold)%cr%Creset'"
    alias glogg='git log --oneline --decorate --graph'

    alias gke='\gitk --all $(git log -g --pretty=%h)'
    alias glof='git log --follow -p --'

    # glo oneline for fzf select
    _glo_one_line() {
      git log --graph --color=always --format='%C(auto)%h%d %s %C(black)%C(bold)%cr' $@
    }

    __git_commit_preview_cmd="echo {} |grep -Eo '[a-f0-9]+' | head -1 |xargs -I% fzf_preview_git_commit % |$__git_pager | LESS='-R' less"
    __git_fzf_opts=" -0" # $FZF_DEFAULT_OPTS is used by default 
    __git_fzf_opts="$FZF_DEFAULT_OPTS -m -0
    --bind=\"ctrl-space:execute($__git_commit_preview_cmd)\"
    --bind=\"ctrl-y:execute-silent(echo {} |grep -Eo '[a-f0-9]+' | head -1 | tr -d '\n' | pbcopy)\"
    " # $FZF_DEFAULT_OPTS is used by default 

    git_log_interactive_preview(){
      _glo_one_line $@  \
        | FZF_DEFAULT_OPTS="$__git_fzf_opts" fzf --preview="$__git_commit_preview_cmd"
    }
    # get git commit SHA1 short
    git_log_interactive_select(){
      _glo_one_line $@  \
        | FZF_DEFAULT_OPTS="$__git_fzf_opts" fzf --preview="$__git_commit_preview_cmd" \
        | /usr/bin/grep -Eo '[a-f0-9]{7,41}'
    }

    git_revert_interactive() {
      git_log_interactive_select | xargs git revert $@
    }  

    git_reset_interactive(){
      git_log_interactive_select | xargs git reset $@
    }

    # alias glop=glopi
    alias gloi=git_log_interactive_select
    alias glo=git_log_interactive_select
    # get git commit SHA1 long
    alias glois='git_log_interactive_select |/usr/bin/grep -Eo "[a-f0-9]{7,41}" | xargs git show | head -1 | cut -d" " -f2'

    # TODO Add git rebase interactive
    alias grevi=git_revert_interactive
    alias greseti=git_reset_interactive
    alias grsi=forgit::restore
    alias gcoi=forgit::restore


    # Git loves FZF
    # My bersonal git preivew scripts

    # TODO update preview 
    # --bind='ctrl-space:execute($__git_commit_preview_cmd)'
    __git_branch_fzf_opts="$FZF_DEFAULT_OPTS -m -0
    --bind=\"ctrl-y:execute-silent(echo {} | cut -c3-1000 | head -1 | tr -d '\n' | pbcopy)\"
    --preview-window right:70%
    " # $FZF_DEFAULT_OPTS is used by default 

    __git_branch_commit_preview_cmd="xargs -I% fzf_preview_git_commit % |$__git_pager | LESS='-R' less"
    __git_branch_history_preview_cmd="xargs -I$$  git log -100 --graph --color=always --format='%C(auto)%d %s %C(bold blue)<%an>%Creset %C(black)%C(bold)%cr%Creset' $$  |$__git_pager | LESS='-R' less"

    # git branch fzf: last commit preview
    alias fzf_gb_commit='FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | $__git_branch_commit_preview_cmd" '
    # git branch fzf: history preview
    alias fzf_gb_history='FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | cut -c3-1000 | $__git_branch_history_preview_cmd" '


    git_branch_interactive(){
      gb $@ \
        | FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | cut -c3-1000 | cut -f1 -d' ' | $__git_branch_history_preview_cmd" | cut -c3-1000 | cut -f1 -d' '
      # This script is depending on format from gb
    }

    # git branch remote + delete + by me
    git_branch_remote_interactive(){
      gbr $@ \
        | FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | cut -d ' ' -f6 | $__git_branch_history_preview_cmd" \
        | cut -d ' ' -f4 | cut -c8-1000
      # This script is depending on format from gbr
    }

    # A, input, B parse selection and preview, C, output

    # Sam as function but function can take parameter
    # alias gbi="git branch --sort=-committerdate --color=always | fzf_gb_history"
    alias gbi=git_branch_interactive
    alias gbic="git branch --sort=-committerdate --color=always | fzf_gb_commit"
    

    alias gbri=git_branch_remote_interactive
    alias gbrdi='gbd_remote $(gbri)'
    alias gcobri='git checkout $(gbri)'

    # Delete branches
    alias gbdi="gbi | xargs -n 1 git branch -d "
    alias gbDi="gbi | xargs -n 1 git branch -D "

    #gbr merged
    alias gbri_merged="gb_merged_remote | fzf | cut -d ' ' -f6 | cut -c8-1000"
    alias gbri_merged_me="gb_merged_remote_me | fzf | cut -d ' ' -f6 | cut -c8-1000"
    alias gbri_me="gbr | grep chuan | fzf | cut -d ' ' -f6 | cut -c8-1000"

    # This might not working
    alias gbrmmdi='gbd_remote $(gbri_merged_me)'

    # alias gcobi='git checkout $(gbi)'                   #Buggy
    alias gcobi='git_branch_interactive | xargs git checkout'


  # Git stash
    FORGIT_STASH_FZF_OPTS='
    --bind="ctrl-d:reload(git stash drop $(cut -d: -f1 <<<{}) 1>/dev/null && git stash list)"
    '
    __git_stash_preview() {
      git stash show --color=always --ext-diff $@
    }


}
# }}}2
# plugin_config {{{2
plugin_config() {
    # Some good keybind is overwrite by plugins or oh-my-zsh
    # Also includes plugin variables
    
    # Others {{{3
    __fzf_config
    __fzf_git_config
    __tmux_config


    ac_my_colors() {
        # Light Theme "ayu_light"
        local MYCOLORS=("Darkside" "OceanicMaterial" "Solarized Darcula" "Broadcast" "Desert" "DotGov" "Misterioso" "Galaxy")
        local RANDOM=$$$(gdate +%N)
        local NEW_COLOR=${MYCOLORS[RANDOM % ${#MYCOLORS[@]} + 1]}
        _iterm2colors_apply $NEW_COLOR
        mlog "COLOR THEME: " $_iterm2colors_current
    }

    # zsh-iterm2colors
    alias ac=_iterm2colors_apply
    alias acc='echo $_iterm2colors_current'
    alias acr=_iterm2colors_apply_random
    alias ac-light='ac "ayu_light"'
    alias ac-darkside='ac "Darkside"'
    alias ac-ocean='ac "OceanicMaterial"'
    alias ac-sd='ac "Solarized Darcula"'
    alias rc='ac_my_colors &&  echo "COLOR THEME: " $_iterm2colors_current'


    alias exa='/usr/local/bin/exa --time-style=long-iso --group-directories-first -F --icons'
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


    alias fzfc='fzf | tr -d "\n" | pbcopy && pbpaste'

    bindkey '^k'      autosuggest-accept
    bindkey '^\n'     autosuggest-execute
    bindkey "^R"      history-search-multi-word # Use multi word. fzf is too aggressive

    # Fzf related
    bindkey "^F"      fzf-file-widget           # fzf files
    bindkey '^G'      fzf-cd-widget             # Search and goto fzf

    bindkey '^T'      toggle-fzf-tab
    bindkey "^O"      zca-widget                # Zsh Command Architect zsh-cmd-architect

    function my-expand() { zle _expand_alias || zle .expand-word || true }

    zle -N my-expand
    bindkey '^ '      my-expand                           # ctrl+space expand alias/glob/parameter
    # Notes "^I" is reserved for suggestion complete
    # ^M for enter

    export GREP_COLOR='1;33'
    alias grepc='grep --color=always'
    alias cgrep='grep --color=always'

    # Vi-mode
    export KEYTIMEOUT=1

    ## Fun
    alias test-passed='if [ "$?" -eq "0" ]; then lolcat ~/.tp -a -s 40 -d 2; fi;'


    # b4b4r07/enhancd
    ENHANCD_HYPHEN_NUM=100
}
# }}}2
# }}}1
# Section: iTerm2 {{{1
# source ~/.iterm2_shell_integration.zsh

# Overwrite iterm2 setting for tab color and brightness
echo -e "\033]6;1;bg;red;brightness;40\a" 1>/dev/null
echo -e "\033]6;1;bg;green;brightness;44\a" 1>/dev/null
echo -e "\033]6;1;bg;blue;brightness;52\a" 1>/dev/null

# Change iTerm2 Profile
# this might work as well: iterm2_profile Performance
alias performance='echo -e "\033]50;SetProfile=Performance\x7"'
alias noperformance='echo -e "\033]50;SetProfile=Empty Default\x7"'

function title {
  local name=$*
  source ~/.iterm2_shell_integration.zsh
  # Change iterm2 tab title
  echo -ne "\033]0;"$name"\007"
  
  iterm2_set_user_var badge $name
}

# Change iterm2 panel badge
function badge {
  local name=$*
  source ~/.iterm2_shell_integration.zsh
  iterm2_set_user_var badge $name
}


# Section: ZSH History {{{1
# --------------------------------------------------------------------------
# ZSH History
# https://unix.stackexchange.com/questions/273861/unlimited-history-in-zsh
# http://zsh.sourceforge.net/Doc/Release/Parameters.html#index-HISTSIZE
export HIST_STAMPS="yyyy-mm-dd" # ZSH History time format
export HISTSIZE=100000          #The maximum number of events stored in the internal history list.
export SAVEHIST=10000000        #The maximum number of history events to save in the history file.

setopt EXTENDED_HISTORY       # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY     # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY          # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS       # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS   # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS      # Do not display a line previously found.
setopt HIST_IGNORE_SPACE      # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS      # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS     # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY            # Don't execute immediately upon history expansion.

zsh_history_bk() {
    mkdir -p ~/.zsh_history_bk
    cp ~/.zsh_history ~/.zsh_history_bk/.zsh_history-$(date +%Y-%m-%d-%H)
}

alias history='zsh_history_bk && omz_history -i'
alias hist-c='zsh_history_bk && vi ~/.zsh_history'
uuu() {
    echo "Clean last command from history"
    ZSH_HISTORY_TEMP="zsh_history_temp"
    ghead -n -2 $HISTFILE >$ZSH_HISTORY_TEMP
    cp $ZSH_HISTORY_TEMP $HISTFILE
    rm $ZSH_HISTORY_TEMP
}

# ZSH AUTO CD into directories
setopt AUTO_CD


# Section: After Load {{{1
# --------------------------------------------------------------------------

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
# }}}

# source /Users/EYONDUU/github/enhancd/init.sh
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
# source ~/github/powerlevel10k/powerlevel10k.zsh-theme
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
plugin_config

ac_my_colors

[ -f ~/.zshrc-local.sh ] && source ~/.zshrc-local.sh

echo "zshrc loaded" >> ~/temp/zsh/log

# https://superuser.com/questions/1092033/how-can-i-make-zsh-tab-completion-fix-capitalization-errors-for-directorys-and
# ZSH case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

_weekly_upgrade() {
  # git -C  ~/github/powerlevel10k pull #moved to zinit
  zinit self-update
  zinit delete --clean
  zinit update --all
  brew update            # update brew
  brew upgrade           # update formula
}

# Other Tool: Proxy {{{1
# --------------------------------------------------------------------------

set-proxy() {
  export ALL_PROXY=127.0.0.1:1087;
  export http_proxy=http://127.0.0.1:1087;
  export https_proxy=http://127.0.0.1:1087;
}
unset-proxy() {
  unset ALL_PROXY
  unset http_proxy
  unset https_proxy
}

# Section: Random after {{{1
# --------------------------------------------------------------------------

alias signs="open https://github.com/romkatv/powerlevel10k#what-do-different-symbols-in-git-status-mean"

# hello completion example
hello() {
    printf 'Hello world.\n'
}

hello2() {
    printf 'Hello world 2.\n'
}


hello3() {
    printf 'Hello world 3.\n'
}


git2() {
    printf 'Git 2\n'
}
 
export fpath=($fpath ~/.zsh)
zstyle ":completion:*:descriptions" format "---- %d ----"

_fzf_complete_gcob2() {
  _fzf_complete --multi --reverse --prompt="doge> " -- "$@ xxx" < <(git branch)
}

# crontab crontab-config

