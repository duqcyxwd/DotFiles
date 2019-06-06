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
export START_MESSAGE=0
export LOADING_BAR=0
export ZPROF_TRACK=0
# export ZSH_PLUGIN_LOADED=1 # Disable ZSH PLUGIN # unset ZSH_PLUGIN_LOADED

# Section: PATH {{{1
# --------------------------------------------------------------------------
# PATH: Global PATH {{{2
# --------------------------------------------------------------------------
export PATH=$HOME/bin:/usr/local/sbin:$HOME/script-tool:/usr/local/bin:$PATH
export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"
# export PATH="/usr/local/opt/maven@3.3/bin:$PATH"
export PATH="./node_modules/.bin:$PATH"
### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

export KAFKA_HOME=/usr/local/kafka
export KAFKA=$KAFKA_HOME/bin
export KAFKA_CONFIG=$KAFKA_HOME/config
export PATH=$KAFKA:$PATH

# export is required for python path
export NODE_PATH=/usr/lib/node_modules
# Tmuxinator
export EDITOR='vim'

[[ $ZPROF_TRACK -eq "1" ]] && zmodload zsh/zprof
[[ $LOADING_BAR -eq "1" ]] && revolver --style "bouncingBar" start "Loading zsh config"

# PATH: Default PATH Parameter {{{2
# --------------------------------------------------------------------------

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export TERM="xterm-256color"
export ZSH_LOADING_LOG=~/.startup.log
export MESSAGE_CACHE_BEFORE_PRINT=~/.startup_all.log
export WELCOME_MESSAGE=~/.welcome_message.log
export HIST_STAMPS="yyyy-mm-dd" # ZSH History time format
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
# Async_load {{{2
async_load() {
    # Something very interesting. callback is working
    # Async works with alias but not tab_complete

    # Make sure this doesn't break anything, if break clean this out
    # source /Users/chuan.du/.antigen/bundles/robbyrussell/oh-my-zsh/oh-my-zsh.sh

    # Manully async load bundles that installed by antigen
    local ANTIGEN_BUNDLES=~/.antigen/bundles
    source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/git/git.plugin.zsh
    source $ANTIGEN_BUNDLES/zsh-users/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
    source $ANTIGEN_BUNDLES/zsh-users/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
    source $ANTIGEN_BUNDLES/zdharma/history-search-multi-word/history-search-multi-word.plugin.zsh
    source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/kubectl/kubectl.plugin.zsh
    source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/mvn/mvn.plugin.zsh
    source /Users/chuan.du/github/kafka-zsh-completions/kafka.zsh

    # tmux
    source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/tmux/tmux.plugin.zsh
    ZSH_TMUX_ITERM2=true
    ZSH_TMUX_AUTOCONNECT=true

    alias tmuxt='unset ZSH_PLUGIN_LOADED && /usr/local/bin/tmux'
    alias tca='tmux -CC attach -t'
    alias tcad='tmux -CC attach -d -t' #Detach other client
    alias tcs='tmux -CC new-session -s'

    # mux
    source $ANTIGEN_BUNDLES/robbyrussell/oh-my-zsh/plugins/tmuxinator/tmuxinator.plugin.zsh
    # Auto completion
    source /Users/chuan.du/github/tmuxinator/completion/tmuxinator.zsh

    # Antibody load
    # source ~/.zsh_plugins.sh
    plugin_config

    if [ $commands[autojump] ]; then
        [ -f /usr/local/etc/profile.d/autojump.sh ] && . /usr/local/etc/profile.d/autojump.sh
    fi

    autoload -Uz compinit
    if [ $(date +'%j') != $(/usr/bin/stat -f '%Sm' -t '%j' ${ZDOTDIR:-$HOME}/.zcompdump) ]; then
        compinit
    else
        # compinit -c
        compinit -C
    fi
    # compdef _tmuxinator tmuxinator mux

    ## Disable async loader to test theme
    async_stop_worker lazyloader
    ac_my_colors
}
# }}}
# async_cust_init {{{2
source /Users/chuan.du/github/zsh-async/async.zsh
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
# }}}

### }}}
[[ $IS_ASYNC -eq "1" ]] && async_cust_init
# Section: Theme Config {{{1
# --------------------------------------------------------------------------
# Set name of the theme to load.

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
        source ~/antigen.zsh
        # Load the oh-my-zsh's library. It will conflict with other theme
        # echo "load oh-my-zsh"
        antigen use oh-my-zsh
    fi
    # Antigen loading package {{{2

    antigen bundle vi-mode
    antigen bundle iterm2                        # Iterm2 profile, color

    antigen bundle shayneholmes/zsh-iterm2colors # Iterm2 Color "0.01"
    antigen bundle paulmelnikow/zsh-startup-timer
    antigen bundle djui/alias-tips # Alias helper

    # antigen bundle gretzky/auto-color-ls             # Async load
    # antigen bundle johanhaleby/kubetail              # Lazy load

    # antigen bundle zsh-users/zsh-syntax-highlighting # Async load
    # antigen bundle zsh-users/zsh-autosuggestions     # 0.02  Async load
    # antigen bundle zdharma/history-search-multi-word # 0.02s Async load

    # echo "THEME: spaceship"
    # antigen theme denysdovhan/spaceship-prompt
    # Async prompt
    antigen theme maximbaz/spaceship-prompt

    # echo "THEME: pure"
    # antigen bundle mafredri/zsh-async
    # antigen bundle sindresorhus/pure
    # }}}

    antigen apply

    # Lazy load bundles {{{2
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
    # tmuxinator() {
    #     unfunction "$0" && antigen bundle tmuxinator && $0 "$@"
    # }
    # }}}
}
# }}}
# Section: Antibody {{{1
# --------------------------------------------------------------------------
load_Antibody() {
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
# Section: ZSH Plugin Loading {{{1
# --------------------------------------------------------------------------

# echo "ZSH_PLUGIN_LOADED $ZSH_PLUGIN_LOADED (1 for loaded)"
if [[ ! -v ZSH_PLUGIN_LOADED ]]; then
    # load_Antigen0>>$ZSH_LOADING_LOG
    load_Antigen
    # load_Antigen >>$ZSH_LOADING_LOG
    # load_Antigen_init>>$ZSH_LOADING_LOG
    # load_Antibody >>$ZSH_LOADING_LOG
fi

# Section: Script Tools {{{1
# --------------------------------------------------------------------------
# Tool: A {{{2
# --------------------------------------------------------------------------
# Change iTerm2 Profile
# this might work as well: iterm2_profile Performance
alias performance='echo -e "\033]50;SetProfile=Performance\x7"'

# Convert CD to markdown format
cc() {
    res=$(pbpaste | sed -e :a -e '$!N; s/\n//; ta' | sed 's/\(CD-[0-9]*\)/[[\1]](https:\/\/cenx-cf.atlassian.net\/browse\/\1) /')
    echo $res
}

color-test() {
    clear
    cat /Users/chuan.du/script-tool/iterm-syntax-test.txt
}

# Tool: B {{{2
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

# Section: Dev small and Docker {{{1
# --------------------------------------------------------------------------
# TODO Use tput cols to determine images info
#Section: Docker functions {{{2
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
#Section: Docker Alias {{{2
# --------------------------------------------------------------------------

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

# Section: Local wildfly {{{1
# --------------------------------------------------------------------------
# Unfinished
## export JBOSS_HOME=/usr/local/opt/wildfly-as/libexec
## export PATH=${PATH}:${JBOSS_HOME}/bin

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

# Section: lein Java funcion {{{1
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

# FileSearch
f() {echo 'find . -iname "*'$1'*" '${@:2} && find . -iname "*$1*" ${@:2} }
r-old() {echo ' grep "'$1'" '${@:2}' -R .' && grep "$1" ${@:2} -R . }
r() {echo 'Replaced with ag'}

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

clean-terminal() {
    /bin/rm /var/log/cenx-alarm-pp/*
    /bin/rm ~/temp/*
    /bin/rm -rf /Users/chuan.du/.m2/repository/cenx/cenx-*
    docker-cleanup-deep
    /bin/rm -rf /usr/local/kafka_2.9.1-0.8.2.2/logs/*

}

# Section: Alias {{{1
# --------------------------------------------------------------------------

# Alias
alias vi='/usr/local/bin/vim'
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
# alias rs="ee 'source ~/.zshrc'"
alias rs='source ~/.zshrc'
alias rst='unset ZSH_PLUGIN_LOADED && source ~/.zshrc'
alias mz='vim ~/.zshrc && shfmt -i 4 -s -w -ci ~/.zshrc'
alias ca="less ~/.zshrc"
alias sch="qlmanage -p /Users/SuperiMan/Documents/2014\ Fall\ Time\ table.png"
alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"

alias mysql="/Applications/XAMPP/xamppfiles/bin/mysql --use=root"
alias notes="mvim ~/repo/Notes/CLojure.md"

# Cheatsheet
alias cidea='cat ~/repo/DotFiles/vim/ideavim-cheatsheet | grep $@'

alias py='python'
alias rmt='/bin/rm'
alias rm="trash"
alias ems='open -a /Applications/Emacs.app $@'
alias em='ems'

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

# CD to any directory with auto complete
export repodir_p_r="/Users/chuan.du/repo/cenx-platform/"
export repodir_p="/Users/chuan.du/repo/cenx-platform/cenx-"
p() {cd $repodir_p_r$1}
compctl -g $repodir_p'*(:t:r)' p

export repodir2="/opt/cenx/application/"
opt() {cd $repodir2$1}
compctl -g $repodir2'*(:t:r)' opt

export repodir3="/Users/chuan.du/github/"
gh() {cd $repodir3$1}
compctl -g $repodir3'*(:t:r)' gh

#============= Powerful and Common alias =============
alias cpwd='echo "copy currenty directory" && pwd |pbcopy'
#Copy file path to clipboard
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
    ee "gitopen -b $(get_current_branch)"
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

# Section: Some Autocompletion {{{1
# --------------------------------------------------------------------------
# Autocompletion for teamocil
compctl -g '~/.teamocil/*(:t:r)' itermocil
# }}}
# Section: Lazy Plugin Load {{{1
# --------------------------------------------------------------------------

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

# Section: plugin_config {{{1
# --------------------------------------------------------------------------
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

# }}}
# plugin_config {{{2
plugin_config() {
    # Some good keybind is overwrite by plugins or oh-my-zsh
    # Also includes plugin variables

    # zsh-iterm2colors
    alias ac=_iterm2colors_apply
    alias acl='echo $_iterm2colors_current'
    alias acr=_iterm2colors_apply_random
    alias ac-light='ac "ayu_light"'
    alias ac-darkside='ac "Darkside"'
    alias ac-ocean='ac "OceanicMaterial"'
    alias ac-sd='ac "Solarized Darcula"'

    ac_my_colors() {
        # Light Theme "ayu_light"
        local MYCOLORS=("Darkside" "OceanicMaterial" "Solarized Darcula" "Broadcast" "Desert" "DotGov" "Misterioso" "Galaxy")
        local RANDOM=$$$(gdate +%N)
        local NEW_COLOR=${MYCOLORS[RANDOM % ${#MYCOLORS[@]} + 1]}
        _iterm2colors_apply $NEW_COLOR
        mlog "COLOR THEME: " $_iterm2colors_current
    }

    alias rc='ac_my_colors &&  echo "COLOR THEME: " $_iterm2colors_current'

    alias exa='/usr/local/bin/exa --time-style=long-iso --git --group-directories-first'
    alias e=exa
    alias l=exa

    alias ea='exa -a'
    alias el='exa -l'
    alias ll=el
    alias ela='exa -l -a'
    alias la=ela
    alias laa='el .?* -d'

    alias lta='exa --git --group-directories-first -lT'
    alias lt1='exa --git --group-directories-first -lT -L 1'
    alias lt2='exa --git --group-directories-first -lT -L 2'
    alias lt3='exa --git --group-directories-first -lT -L 3'
    alias lt4='exa --git --group-directories-first -lT -L 4'
    alias lt=lt2

    alias lc='colorls -l --sd --gs'
    alias lca='colorls -lA --sd --gs'

    bindkey '^k' autosuggest-accept
    bindkey '^\n' autosuggest-execute
    # bindkey "^R" history-incremental-search-backward
    # bindkey "^S" history-incremental-search-forward

    # Vi-mode
    export KEYTIMEOUT=1

    ## Fun
    alias test-passed='if [ "$?" -eq "0" ]; then lolcat ~/.tp -a -s 40 -d 2; fi;'
}
# }}}
# }}}

# Section: Load local config && plugin config
spaceship_power_version_init
spaceship_config
source ~/.zshrc-local.sh

# Section: After Load {{{1
# --------------------------------------------------------------------------

if [ "$IS_ASYNC" -eq "1" ]; then
    # No need stop worker
    # async_stop_worker my_worker
else
    if [ "$START_MESSAGE" -eq "1" ]; then
        prepare_start_message
    fi
fi

[[ $LOADING_BAR -eq "1" ]] && revolver stop # Stop loading Bar
[[ $START_MESSAGE -eq "1" ]] && print_start_message

# Only load omz and theme once
export ZSH_PLUGIN_LOADED=1
export IS_ASYNC=0
# unset ZSH_PLUGIN_LOADED && unset IS_ASYNC

[[ $ZPROF_TRACK -eq "1" ]] && zprof # bottom of .zshrc
# }}}
