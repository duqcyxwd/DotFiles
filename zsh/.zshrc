#   __  ____     __  ______ _____ _    _ _____   _____
#  |  \/  \ \   / / |___  // ____| |  | |  __ \ / ____|
#  | \  / |\ \_/ /     / /| (___ | |__| | |__) | |
#  | |\/| | \   /     / /  \___ \|  __  |  _  /| |
#  | |  | |  | |     / /__ ____) | |  | | | \ \| |____
#  |_|  |_|  |_|    /_____|_____/|_|  |_|_|  \_\\_____|
#
#  My .zshrc to save some coffee time and keep my hair on my head

# Section: Init {{{1
# --------------------------------------------------------------------------
# Display a loading sign for zshrc

# ASYNC only support loading startup message
export IS_ASYNC=1
export START_MESSAGE=1
export LOADING_BAR=1
export OH_MY_ZSH_LOADED=1
export ZPROF_TRACK=0
# export ZSH_PLUGIN_LOADED=1 # Disable ZSH PLUGIN # unset ZSH_PLUGIN_LOADED

# Section: ENV PATH {{{1
# --------------------------------------------------------------------------
#=============================== PATH ===========================================

export PATH=$HOME/bin:/usr/local/sbin:$HOME/script-tool:/usr/local/bin:$PATH
export PATH="/Users/chuan.du/repo/dev-env/bin:$PATH"
export PATH="/usr/local/opt/maven@3.3/bin:$PATH"
export PATH="./node_modules/.bin:$PATH"
### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# export is required for python path
# export PYTHONPATH="/Users/chuan.du/repo/autotest/tests/component/cenx-rest-api:${PYTHONPATH}"
export NODE_PATH=/usr/lib/node_modules

# Tmuxinator
export EDITOR='vim'

if [ "$ZPROF_TRACK" -eq "1" ]; then
    zmodload zsh/zprof
fi

if [ "$LOADING_BAR" -eq "1" ]; then
    revolver --style "bouncingBar" start "Loading zsh config"
fi

# Section: Default PATH Parameter {{{1
# --------------------------------------------------------------------------

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export TERM="xterm-256color"
export ZSH_LOADING_LOG=~/.startup.log
export MESSAGE_CACHE_BEFORE_PRINT=~/.startup_all.log
export WELCOME_MESSAGE=~/.welcome_message.log
export HIST_STAMPS="yyyy-mm-dd"

# Section: pre script {{{1
# --------------------------------------------------------------------------
#=============================== pre script  ===========================================
PREPARE_START_MESSAGE() {
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

PRINT_START_MESSAGE() {
    (cat $WELCOME_MESSAGE && (cat $ZSH_LOADING_LOG | boxes -d stone -p a2t1)) | lolcat
    return 0
}

mlog() {
    echo $@ >>$ZSH_LOADING_LOG
}

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

echoAndEval() { echo $@ && eval $@; }

alias -g ee=echoAndEval
alias -g timeElapsed="pv -F 'Elapsed time: %t'"
alias v='py /Users/chuan.du/script-tool/version.py ./'
power_v() {
    python /Users/chuan.du/script-tool/version.py ./ powermode
}

noti() {
    # terminal-notifier
    # Can use terminal-notifier if we want icon modification. Can't do noti confirm
    # Buildin noti
    osascript -e "display notification \"$1\" with title \"$2\""
}

# Section: ZSH ASYNC PATH {{{1
# --------------------------------------------------------------------------
#=============================== ZSH ASYNC PATH ===========================================

lazy_load() {
    # print "LAZY LOAD completed"
    # source /Users/chuan.du/github/anishathalye-dotfiles/zsh/prompt.zsh
    # Manully load theme to spped them up.
    # fpath=( "$HOME/.zfunctions" $fpath )
    # autoload -U promptinit; promptinit
    # prompt spaceship
    
    # Something very interesting. callback is working
    # Async works with alias but not tab_complete
    source /Users/chuan.du/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/git/git.plugin.zsh
    source /Users/chuan.du/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/git/git.plugin.zsh
    
    source /Users/chuan.du/.antigen/bundles/zsh-users/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
    source /Users/chuan.du/.antigen/bundles/zsh-users/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
    source /Users/chuan.du/.antigen/bundles/zdharma/history-search-multi-word/history-search-multi-word.plugin.zsh
    source /Users/chuan.du/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/kubectl/kubectl.plugin.zsh
    source /Users/chuan.du/.antigen/bundles/robbyrussell/oh-my-zsh/plugins/mvn/mvn.plugin.zsh
    source /Users/chuan.du/github/kafka-zsh-completions/kafka.zsh
    
    # Antibody load
    # source ~/.zsh_plugins.sh
    
    if [ $commands[autojump] ]; then
        [ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh
    fi
    # plugin_config
    ac_my_colors
    
    autoload -Uz compinit
    if [ $(date +'%j') != $(/usr/bin/stat -f '%Sm' -t '%j' ${ZDOTDIR:-$HOME}/.zcompdump) ]; then
        compinit
    else
        compinit -C
    fi
    ## Disable async loader to test theme
    async_stop_worker lazyloader
}

pre-async() {
    source /Users/chuan.du/github/zsh-async/async.zsh
    async_init
    
    _my_worker_stop() {
        # print "STOP MY WORKER"
        async_stop_worker my_worker
    }
    _async_start_message() {
        PREPARE_START_MESSAGE
    }
    
    async_start_worker my_worker -n
    async_register_callback my_worker _my_worker_stop
    
    async_start_worker lazyloader -n
    async_register_callback lazyloader lazy_load
    
    if [ "$START_MESSAGE" -eq "1" ]; then
        async_worker_eval lazyloader sleep 1
        async_worker_eval my_worker sleep 5
        async_worker_eval my_worker _async_start_message
    fi
}

if [ "$IS_ASYNC" -eq "1" ]; then
    pre-async
fi

# Section: oh-my-zsh {{{1
# --------------------------------------------------------------------------
#=============================== oh-my-zsh  ======================================
# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/

# ZSH_THEME="agnoster-cus"
# ZSH_THEME="agnoster"
# ZSH_THEME="powerlevel9k/powerlevel9k"

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

# Section: Antigen {{{1
# --------------------------------------------------------------------------
#=========================== Antigen ==================================
# Load very basic plugins
load_Antigen0() {
    echo "THEME: spaceship"
    if ! type "antigen" >/dev/null; then
        source ~/antigen.zsh
        # Load the oh-my-zsh's library. It will conflict with other theme
        antigen use oh-my-zsh
    fi
    antigen theme denysdovhan/spaceship-prompt
    antigen bundle paulmelnikow/zsh-startup-timer
    antigen apply
}

load_Antigen() {
    mlog "PlUGIN ENGINE: Antigen"
    if ! type "antigen" >/dev/null; then
        echo "init antigen"
        source ~/antigen.zsh
        # Load the oh-my-zsh's library. It will conflict with other theme
        antigen use oh-my-zsh
    fi
    source ~/.antigenrc
    # Lazy load bundles
    if ! type "kubetail" >/dev/null; then
        kubetail() {
            unfunction "$0"
            antigen bundle johanhaleby/kubetail
            $0 "$@"
        }
    fi
    gst() {
        unfunction "$0"
        antigen bundle git
        antigen bundle git-extras
        $0 "$@"
    }
    # mvn() { unfunction "$0" && antigen bundle mvn && $0 "$@" }
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
    tmuxinator() {
        unfunction "$0" && antigen bundle tmuxinator && $0 "$@"
    }
    
}

load_Antigen_init() {
    echo "PlUGIN ENGINE: Antigen Init"
    if ! type "antigen" >/dev/null; then
        source ~/antigen.zsh
        # Load the oh-my-zsh's library. It will conflict with other theme
        antigen use oh-my-zsh
    fi
    antigen init ~/.antigenrc
}

plugin_config() {
    # Some good keybind is overwrite by plugins or oh-my-zsh
    # Also includes plugin variables
    
    # Spaceship
    SPACESHIP_TIME_SHOW=true
    SPACESHIP_EXEC_TIME_ELAPSED=1
    spaceship_power_version_init
    
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
    
    SPACESHIP_PROMPT_ORDER=(time user host git_branch git_status power_version hg package node ruby elixir xcode swift golang php haskell julia aws venv conda pyenv dotnet ember kubecontext exec_time line_sep dir battery vi_mode jobs exit_code char)
    
    # zsh-iterm2colors
    alias ac=_iterm2colors_apply
    alias acl='echo $_iterm2colors_current'
    alias acr=_iterm2colors_apply_random
    alias ac-light='ac "ayu_light"'
    alias ac-darkside='ac "Darkside"'
    alias ac-ocean='ac "OceanicMaterial"'
    alias ac-sd='ac "Solarized Darcula"'
    
    ac_my_colors() {
        local MYCOLORS=("ayu_light" "Darkside" "OceanicMaterial" "Solarized Darcula" "Broadcast" "Desert" "DotGov" "Misterioso" "Galaxy")
        local RANDOM=$$$(gdate +%N)
        local NEW_COLOR=${MYCOLORS[RANDOM % ${#MYCOLORS[@]} + 1]}
        _iterm2colors_apply $NEW_COLOR
        mlog "COLOR THEME: " $_iterm2colors_current
    }
    
    alias rc='ac_my_colors &&  echo "COLOR THEME: " $_iterm2colors_current'
    
    alias exa='/usr/local/bin/exa --time-style=long-iso'
    alias ea='exa --git --group-directories-first -a'
    alias el='exa -l --git --group-directories-first'
    alias e='exa --git --group-directories-first'
    
    alias l='colorls -A --sd --report'
    alias lg='colorls -A --sd --report --gs'
    alias lc='colorls -l --sd --gs'
    alias ll='colorls -l --sd --gs'
    alias lca='colorls -lA --sd --gs'
    source $(dirname $(gem which colorls))/tab_complete.sh
    
    bindkey '^k' autosuggest-accept
    bindkey '^\n' autosuggest-execute
    # bindkey "^R" history-incremental-search-backward
    # bindkey "^S" history-incremental-search-forward
    
    # Vi-mode
    export KEYTIMEOUT=1
}

# Section: Antibody {{{1
# --------------------------------------------------------------------------
#============================== antibody ======================================

load_Antibody() {
    mlog "Use antibody"
    # No matter what I try, this is super slow
    source ~/.zsh_plugins.sh
    
    # source <(antibody init)
    
    # # antibody bundle bhilburn/powerlevel9k
    # antibody bundle <~/.zsh_plugins.txt
    
    # echo "POWERLINE THEME: spaceship"
    # # antibody bundle denysdovhan/spaceship-prompt
    # if [ "$OH_MY_ZSH_LOADED" -ne "1" ]; then
    #     # antibody bundle maximbaz/spaceship-prompt
    # fi
}

# Section: Load plugins {{{1
# --------------------------------------------------------------------------

#========================= oh-my-zsh DEFAULT ==================================
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# vi-mode
# plugins=(git git-extras osx mvn npm brew docker)
# plugins=()

#========================= load ZSH plugins ===================================
if [ "$OH_MY_ZSH_LOADED" -ne "1" ]; then
    mlog "INIT: oh-my-zsh"
    echo "INIT: oh-my-zsh"
    source $ZSH/oh-my-zsh.sh
fi

# source /Users/chuan.du/github/anishathalye-dotfiles/zsh/prompt.zsh

if [[ ! -v ZSH_PLUGIN_LOADED ]]; then
    # load_Antigen0>>$ZSH_LOADING_LOG
    load_Antigen >>$ZSH_LOADING_LOG
    # load_Antigen_init>>$ZSH_LOADING_LOG
    # load_Antibody>>$ZSH_LOADING_LOG
fi

# Section: Script tools {{{1
# --------------------------------------------------------------------------
#============================= Script tool: Most useful tool ever ===============================

cc() {
    res=$(pbpaste | sed -e :a -e '$!N; s/\n//; ta' | sed 's/\(CD-[0-9]*\)/[[\1]](https:\/\/cenx-cf.atlassian.net\/browse\/\1) /')
    echo $res
}

color-test() {
    clear
    cat /Users/chuan.du/temp/iterm-syntax-test.txt
}

# Change iTerm2 Profile

# this might work as well: iterm2_profile Performance
alias performance='echo -e "\033]50;SetProfile=Performance\x7"'

#============================= system-clean-up ===============================
alias clean-m2-cenx="rm /Users/chuan.du/.m2/repository/cenx"

# Section: Dev small and Docker {{{1
# --------------------------------------------------------------------------

#============================= Dev Small stuff ===============================
# alias doppelganger="docker run -it --rm -v `pwd`:/transport docker.cenx.localnet:5000/doppelganger:0.1.3-SNAPSHOT-b6"
alias doppelganger="docker run -it --rm -v $(pwd):/transport ship.cenx.com:5000/doppelganger"
export CORTX_IP=192.168.59.103
export ZK_PORT=2181
export KAFKA_PORT=9092
export SOLR_PORT=8983
export PARKER_REPL_PORT=4081
export TERMINUS_REPL_PORT=4083
export NARANATHU_REPL_PORT=4015

#============================= Dokcer ===============================
docker-stats() { docker stats --format "table {{.Name}}\t{{.Container}}\t{{.CPUPerc}}\t{{.MemPerc}}\t{{.MemUsage}}\t{{.NetIO}}\t{{.BlockIO}}"; }
docker-stats-peek() { docker stats --no-stream --format "table {{.Name}}\t{{.Container}}\t{{.CPUPerc}}\t{{.MemPerc}}\t{{.MemUsage}}\t{{.NetIO}}\t{{.BlockIO}}"; }
alias=docker-stop='docker stop $(docker ps -q)'
alias docker-rm-container-stopped="docker rm $(docker ps -aq -f status=exited)"
alias docker-rm-images-stopped="docker rm $(docker ps -aq -f status=exited)"
alias docker-rmi-clean='docker rmi $(docker images --filter "dangling=true" -q --no-trunc)'
docker-cleanup-deep() {
    docker rm -v $(docker ps --filter status=exited -q 2>/dev/null) 2>/dev/null
    docker rmi $(docker images --filter dangling=true -q 2>/dev/null) 2>/dev/null
}

docker-ps() {
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
alias dps=docker-ps

dpsc() {
    docker ps $@ --format 'table{{ .ID }}\t{{ .Names }}\t{{ .Status }}\t'
}

dpsv() {
    export FORMAT="ID\t{{.ID}}\nNAME\t{{.Names}}\nIMAGE\t{{.Image}}\nPORTS\t{{.Ports}}\nCOMMAND\t{{.Command}}\nCREATED\t{{.CreatedAt}}\nSTATUS\t{{.Status}}\n"
    docker ps $@ --format="$FORMAT" --no-trunc
}

dpsvf() {
    dpsv --filter name=$@
}

dexec() {
    echo "Docker exec -it by name"
    firstmatch=$(docker ps -q --filter name=$@ | head -1)
    dpsv -f id=$firstmatch
    docker exec -it $(docker ps -q --filter name=$@ | head -1) bash
}

dpsa() { echo "docker list all containers" && docker-ps -a $@; }

dps-old() { echo "Docker ps old" && docker ps -a --format "table {{.Names}}\t{{.Image}}\t{{.Status}}" | awk 'NR == 1; NR > 1 { print $0 | "sort" }'; }
dps-old-p() { echo "Docker ps old with port" && docker ps -a --format "table {{.Names}}\t{{.Image}}\t{{.Status}}\t{{.Ports}}" | awk 'NR == 1; NR > 1 { print $0 | "sort" }'; }

pre-docker() {
    export DOCKER_MACHINE_IP=$(docker-machine ip)
    eval $(docker-machine env)
}

#============================== Docker old ===============================
#export DOCKER_HOST=tcp://192.168.59.103:2375
#unset DOCKER_HOST
#unset DOCKER_CERT_PATH
#unset DOCKER_TLS_VERIFY

# Section: Local wildfly {{{1
# --------------------------------------------------------------------------
#============================ local wildfly ==============================
# Unfinished
## export JBOSS_HOME=/usr/local/opt/wildfly-as/libexec
## export PATH=${PATH}:${JBOSS_HOME}/bin
export PATH=${PATH}:/usr/local/kafka/bin
export PATH=${PATH}:/usr/local/Cellar/kafka/0.8.2.1/bin

pre-wildfly() {
    echo "export JBOSS_HOME"
    export JBOSS_HOME=/Users/chuan.du/Downloads/wildfly-10.1.0.Final
    ## export PATH=${PATH}:${JBOSS_HOME}/bin
    export WILDFLY_DEPLOY=${JBOSS_HOME}/standalone/deployments
}

# export WILDFLY_HOME=/Users/chuan.du/CENX/wildfly-10.1.0.Final
# Wildfly created by Eclipse
export WILDFLY_HOME=/Users/chuan.du/wildfly-10.0.0.Final
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

# Section: Something {{{1
# --------------------------------------------------------------------------

#========== Clojure Repl ===========
alias lr="lein do clean, repl"
alias lcr="echo 'lein do clean, repl' && lein do clean, repl"
alias lci="echo 'lein do clean, install' && lein do clean, install"

# ================================
#
# Configuration only with local computer
# /Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home
export JAVA_HOME=$(/usr/libexec/java_home)
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_79.jdk/Contents/Home/
export DSE_BIN=/Users/chuan.du/CENX/dse/bin
export PATH=$PATH:$JAVA_HOME/bin:$DSE_BIN

alias later="nohup /Users/chuan.du/repo/DotFiles/otherTool/later.pl"

# FileSearch
f() {echo 'find . -iname "*'$1'*" '${@:2} && find . -iname "*$1*" ${@:2} }
r-old() {echo ' grep "'$1'" '${@:2}' -R .' && grep "$1" ${@:2} -R . }
r() {echo ' Replaced with ag'}

# Watch function
mywatch() {
    while :; do
        a=$($@)
        clear
        echo "$(date)\n\n$a"
        sleep 2
    done
}
#watch-zookeeper {while :; do clear; echo "$(date)\n\n$(echo stat |nc localhost 2181)"; sleep 1;  done}
#watch-zookeeper2 {while :; a=$@; do clear; echo "$(date)\n\n$(echo stat |nc $a 2181)"; sleep 1;  done}
#watch-zookeeper-cnumber {while :; do clear; echo "$(date)\n\n$(echo stat | nc localhost 2181 |grep 127.0.0.1 |wc -l)"; sleep 1;  done}

# t(){ timerToStartApplication "$@" &}
timerToStartApplication() {
    # sleep 1 && open -a /Applications/Time\ Out\ Free.app
    ti=$@
    print "sleep to start Timeout Free\nSleep $ti"
    pkill "Time Out Free"
    sleep $@
    print 'start Timeout Free'
    open -a /Applications/Time\ Out\ Free.app
    open -a /Applications/Font\ Book.app
}

# Section: Alias {{{1
# --------------------------------------------------------------------------

# Alias
alias vi='/usr/local/bin/vim'
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
# alias rs="ee 'source ~/.zshrc'"
alias rs='source ~/.zshrc'
alias rst='unset ZSH_PLUGIN_LOADED && antigen-reset && source ~/.zshrc'
alias mz='vim ~/.zshrc && shfmt -l -s -w -ci ~/.zshrc'
alias ca="less ~/.zshrc"
alias sch="qlmanage -p /Users/SuperiMan/Documents/2014\ Fall\ Time\ table.png"
alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
alias mysql="/Applications/XAMPP/xamppfiles/bin/mysql --use=root"

alias notes="mvim ~/repo/Notes/CLojure.md"

alias py='python'
alias rmt='/bin/rm'
alias rm="trash"
alias ems='open -a /Applications/Emacs.app $@'
alias em='ems'
# alias emc="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

alias sourcetree='open -a SourceTree'
alias st='sourcetree'

# Alias for echo done
alias tf='echo "Task finished"'

#============= Database =============
#alias sql="echo 'psql to localhost' && psql -h 'localhost' -U 'postgres'"
alias sql="echo 'psql to localhost' && ee \"export PAGER='less -SF' && psql -h 'localhost' -U 'postgres'\""

#============= Dir alias =============
# CD to any directory with auto complete
export repodir="/Users/chuan.du/repo/"
c() {cd $repodir$1}
compctl -g $repodir'*(:t:r)' c

export repodir2="/opt/cenx/application/"
opt() {cd $repodir2$1}
compctl -g $repodir2'*(:t:r)' opt

export repodir3="/Users/chuan.du/github/"
gh() {cd $repodir3$1}
compctl -g $repodir3'*(:t:r)' gh

#============= Powerful and Common alias =============
# alias copy="echo 'Copy file path' && tr -d '\n' | pbcopy"
# Don't know
# alias copy="tr -d '\n' | pbcopy"
alias cpwd='echo "copy currenty directory" && pwd |pbcopy'

#Copy file path to clipboard
#greadlink -f development.pem
cpath() {greadlink -f $1 | tr -d '\n' | pbcopy }
alias cf='pbpaste | pbcopy' # clean format of clipboard
alias dir='dirs -v'

# Section: Git {{{1
# --------------------------------------------------------------------------
#============================= Git alias =================================
git config --global color.ui true
alias git-tag-tips="echo ' git tag v1.0.0 \n git tag -a v1.2 9fceb02 \n git push origin v1.5 \n git push origin --tags'"
alias git-hidden="git ls-files -v | grep '^[a-z]' | cut -c3-"
alias git-hide='ee "git update-index --assume-unchanged"'
alias git-unhide-all='ee "git update-index --really-refresh"'
alias git-update-all='find . -type d -depth 1 -exec git --git-dir={}/.git --work-tree=$PWD/{} pull origin master \;'

#======================= Git Alias for work  =========================================

alias gu="git add project.clj && git commit -m 'Upversion'"
alias gbc="echo 'Copy current branch name' && git rev-parse --abbrev-ref HEAD |pbcopy && git branch"
alias gb-update-five-one="git fetch -p && git merge origin/r/5.1.x"
alias gbu51=gb-update-five-one
alias gbui="echo 'git branch update with integration' && git fetch -p && git merge origin/integration"
alias gbu60x="git fetch -p && git merge origin/r/6.0.x"
alias gbu800="git fetch -p && git merge origin/r/8.0.0.x"
alias gbu810="git fetch -p && git merge origin/r/8.1.0.x"

alias gcoi="git checkout integration && git pull"
alias gcoip="git checkout integration && git pull"
alias gco51="git fetch -p && git checkout r/5.1.x && git pull"
alias gco61="git fetch -p && git checkout r/6.1.0.x && git pull"

alias gfco='git fetch -p && git checkout'

#=== Special Git Tool  ====
get_git_current_branch() { git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'; }

get_current_branch() { git branch 2>/dev/null | sed -e "/^[^*]/d" -e "s/* \(.*\)/\1/"; }

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
    current_branch=$(get_current_branch)
    ee "gitopen -b ${current_branch}"
}

alias gcob="ee 'gco -b'"
alias gbrr="ee 'gbr |grep r/'"
# alias gcobr='echo "Create branch and remote branch| Stop using this one, use push remote instead. Try ggsup"'
alias gcobr='echo "Create branch and remote branch| Stop using this one, use push remote instead" & git_create_branch'

# git recent branch
alias gre="ee 'git recent | head'"
alias grec="ee 'git recent | grep -i chuan | grep -v gone'"

#===== Branch clean up ======
# Check branch
alias gb-merged-remote="git for-each-ref --merged HEAD --sort=-committerdate refs/remotes/ --format='(%(color:green)%(committerdate:relative)%(color:reset)) %(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(authorname)'"
alias gb-merged-remote-by-me='ee "gb-merged-remote |grep Chuan"'
alias gb-merged='git branch --merged'

# Clean merged Branch
alias gbd-merged-branch-local='git branch --merged | grep -v "\*" | xargs -n 1 git branch -d'
alias gbd-remote='ee "git push -d origin"'

# Delete all branchs excep current branch
# alias gbdelete-all='gb | grep "f/CD" | grep -v "\*" |xargs -n 1 git branch -D'

# Configure git as my personal repo
alias config-git-local="ee \"git config --local user.name 'Yongqinchuan Du' && git config --local user.email 'duqcyxwd@gmail.com'\""

#===== Git alist =====

# run gitk
alias gk="ee 'gitk --all&'"

# git diff
alias gdi='echo "git diff current branch to integration;" && ee "git diff origin/integration..$get_git_current_branch"'
alias gitxdi="ee 'git diff origin/integration..$get_git_current_branch | gitx'"
alias gdi-gitx=gitxdi
alias gds="ee 'git diff -w --stat'"

#pretty git one line git log with time and author(should use glog)
alias glogt="ee 'git log --pretty=tformat:\"%h %ad | %s%d [%an]\" --graph --date=short'"

#show all git aliases
alias galias='alias|grep git'

# git clean up
#clean all but the stuff the stuff that we would like preserved like .ccache, xmls catalog etc
#clean -dxf will wipe everything requiring user to source gitenv again
#alias gclean='pushd $MY_GIT_TOP > /dev/null && git submodule foreach --recursive 'git clean -xdf' && git clean -xdf -e .ccache -e .flex_dbg -e remap_catalog.xml && popd > /dev/null'

# Section: Something {{{1
# --------------------------------------------------------------------------
# Autocompletion for teamocil
compctl -g '~/.teamocil/*(:t:r)' itermocil

plugin_config

# Section: Something {{{1
# --------------------------------------------------------------------------
#======================== Terminal Config ==================================
source ~/.zshrc-local.sh

#========================= Other helper script ================================
#
# Media
# Convert m4a/wma to mp3
# mkdir newfiles
# for f in *.m4a; do ffmpeg -i "$f" -codec:v copy -codec:a libmp3lame -q:a 2 newfiles/"${f%.m4a}.mp3"; done
# for f in *.wma; do ffmpeg -i "$f" -codec:v copy -codec:a libmp3lame -q:a 2 newfiles/"${f%.wma}.mp3"; done

#
## Hacky
#if [ $(which docker-credential-osxkeychain) ]; then
#        unlink $(which docker-credential-osxkeychain)
#fi

# test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# Section: Something {{{1
# --------------------------------------------------------------------------

# unset ZSH_PLUGIN_LOADED && unset OH_MY_ZSH_LOADED && unset IS_ASYNC

# Helm completion
# source <(helm completion zsh | sed -E 's/\["(.+)"\]/\[\1\]/g')
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

# Lazy load example
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

#========================= Startup message  ===================================

if [ "$IS_ASYNC" -eq "1" ]; then
    # No need stop worker
    # async_stop_worker my_worker
else
    if [ "$START_MESSAGE" -eq "1" ]; then
        PREPARE_START_MESSAGE
    fi
fi

if [ "$LOADING_BAR" -eq "1" ]; then
    revolver stop # Stop loading Bar
fi

if [ "$START_MESSAGE" -eq "1" ]; then
    PRINT_START_MESSAGE
fi

# Only load omz and theme once
export OH_MY_ZSH_LOADED=1
export ZSH_PLUGIN_LOADED=1
export IS_ASYNC=0

if [ "$ZPROF_TRACK" -eq "1" ]; then
    zprof # bottom of .zshrc
fi
# }}}
