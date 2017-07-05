# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
PATH="/usr/local/sbin:/Users/deridder/bin:./node_modules/.bin:$PATH"
NODE_PATH=/usr/lib/node_modules

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="agnoster-cus"

# Autocompletion for teamocil
compctl -g '~/.teamocil/*(:t:r)' itermocil

#========== Dev Small stuff ===========
alias doppelganger="docker run -it --rm -v `pwd`:/transport docker.cenx.localnet:5000/doppelganger:0.1.3-SNAPSHOT-b6"
export CORTX_IP=192.168.59.103
export ZK_PORT=2181
export KAFKA_PORT=9092
export SOLR_PORT=8983
export PARKER_REPL_PORT=4081
export TERMINUS_REPL_PORT=4083
export NARANATHU_REPL_PORT=4015

function dstats() {
    docker stats $(docker inspect --format='{{.Name}}' $(docker ps -$1q --no-trunc));
}

function pre-docker() {
    export DOCKER_MACHINE_IP=$(docker-machine ip)
    eval $(docker-machine env)
}

#========== server ssh  ===========
alias qa04-4="ssh med14.cenx.localnet"
alias cs1="ssh cassandra01.cenx.localnet"
alias cs2="ssh cassandra02.cenx.localnet"
alias cs3="ssh cassandra03.cenx.localnet"
alias qa04-3="ssh med13.cenx.localnet"
alias med22="ssh med22.cenx.localnet"
alias med23="ssh med23.cenx.localnet"
alias med24="ssh med25.cenx.localnet"
alias med25="ssh med25.cenx.localnet"
alias med26="ssh med26.cenx.localnet"

alias system01log="ssh deployer@system01-dev10.cenx.localnet tail -10000f /cenx/docker/log/wildfly/wildfly_apps/server.log"

#========== Orca ===========
export ORCA_VERSION='1.0.0-rc5-SNAPSHOT'
#========== Docker ===========
#export DOCKER_HOST=tcp://192.168.59.103:2375
unset DOCKER_HOST
unset DOCKER_CERT_PATH
unset DOCKER_TLS_VERIFY

alias cortxf='docker run --rm -t -v `pwd`:/opt/cenx docker.cenx.localnet:5000/deployer -f /opt/cenx/solr.yaml'
alias cortx0='docker run --rm -t -v `pwd`:/opt/cenx docker.cenx.localnet:5000/deployer'
alias cortx-small='docker run --rm -t -v /opt/cenx:/opt/cenx docker.cenx.localnet:5000/deployer'

#========== local wildfly ===========
## export JBOSS_HOME=/usr/local/opt/wildfly-as/libexec
## export PATH=${PATH}:${JBOSS_HOME}/bin
export PATH=${PATH}:/usr/local/kafka/bin
export PATH=${PATH}:/usr/local/Cellar/kafka/0.8.2.1/bin

function pre-wildfly() {
    echo "export JBOSS_HOME"
    export JBOSS_HOME=/Users/chuan.du/Downloads/wildfly-10.1.0.Final
    ## export PATH=${PATH}:${JBOSS_HOME}/bin
    export WILDFLY_DEPLOY=${JBOSS_HOME}/standalone/deployments
}

export WILDFLY_HOME=/Users/chuan.du/CENX/wildfly-10.1.0.Final
export WILDFLY_DEPLOY=${WILDFLY_HOME}/standalone/deployments
export WILDFLY_BIN=${WILDFLY_HOME}/bin

alias deploy-clean='echo "Clean wildfly deploy directory" && rm $WILDFLY_DEPLOY/*.war*'
alias deploy-war="echo 'deploy war from target' && cp target/*.war $WILDFLY_DEPLOY"
alias deploy-war-enable="echo 'enable app from deployment' && rm $WILDFLY_DEPLOY/*.undeployed && rm $WILDFLY_DEPLOY/*.failed"
alias deploy-war-disable-all="echo 'disable app from ' $JBOSS_HOME && rm $WILDFLY_DEPLOY/*.deployed"
alias wildfly-restart='$WILDFLY_BIN/jboss-cli.sh -c --command=":shutdown(restart=true)"'
alias wildfly-standalone='$WILDFLY_BIN/standalone.sh'
#wildfly commands http://developer-should-know.com/post/76413579867/how-to-restart-jboss-and-wildfly-using-command

alias lcid="lci && deploy-war"
alias deploy-war-and-standalone="echo 'deploy war and run standalone' && deploy-clean && deploy-war && wildfly-standalone"


#========== Add docker-deployer script ===========
export PATH=${PATH}:/Users/chuan.du/repo/docker-deployer/utilities/

#========== Clojure Repl ===========
alias lr="lein do clean, repl"
alias lcr="echo 'lein do clean, repl' && lein do clean, repl"
alias lci="echo 'lein do clean, install' && lein do clean, install"
alias lein-cenx-clean="rm /Users/chuan.du/.m2/repository/cenx"

# ================================
#
# Configuration only with local computer
# /Library/Java/JavaVirtualMachines/jdk1.8.0_45.jdk/Contents/Home
export JAVA_HOME=$(/usr/libexec/java_home)
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.7.0_79.jdk/Contents/Home/
export DSE_BIN=/Users/chuan.du/CENX/dse/bin
export PATH=$PATH:$JAVA_HOME/bin:$DSE_BIN

alias vi='/usr/local/bin/vim'

alias time-out-hide='subl /Applications/VLC.app/Contents/Info.plist'

alias tf="t"
alias later="nohup /Users/chuan.du/repo/DotFiles/otherTool/later.pl"

# FileSearch
function f() {echo 'find . -iname *'$1'*" '${@:2} && find . -iname "*$1*" ${@:2} }
function r() {echo ' grep "'$1'" '${@:2}' -R .' &&  grep "$1" ${@:2} -R . }

# Watch function
#function watch {while :; do a=$($@); clear; echo "$(date)\n\n$a"; sleep 1;  done}
#function watch-zookeeper {while :; do clear; echo "$(date)\n\n$(echo stat |nc localhost 2181)"; sleep 1;  done}
#function watch-zookeeper2 {while :; a=$@; do clear; echo "$(date)\n\n$(echo stat |nc $a 2181)"; sleep 1;  done}
#function watch-zookeeper-cnumber {while :; do clear; echo "$(date)\n\n$(echo stat | nc localhost 2181 |grep 127.0.0.1 |wc -l)"; sleep 1;  done}

# t(){ timerToStartApplication "$@" &}
timerToStartApplication() {
    # sleep 1 && open -a /Applications/Time\ Out\ Free.app
    ti=$@
    print "sleep to start Timeout Free\nSleep $ti"
    pkill "Time Out Free"
    sleep $@
    print 'start Timeout Free'
    open -a /Applications/Time\ Out\ Free.app
open -a /Applications/Font\ Book.app }

# Alias
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
alias rs="source ~/.zshrc"
alias mz="vim ~/.zshrc"
alias ma="mz"
alias ca="less ~/.zshrc"
alias sch="qlmanage -p /Users/SuperiMan/Documents/2014\ Fall\ Time\ table.png"
alias subl="/Applications/Sublime\ Text.app/Contents/SharedSupport/bin/subl"
alias mysql="/Applications/XAMPP/xamppfiles/bin/mysql --use=root"

alias notes="mvim ~/repo/Notes/CLojure.md"
alias vnotes="mvim ~/repo/Notes/Git_Vim_Linux.md"

alias py='python'
alias rmt='/bin/rm'
alias rm="trash"
alias trs='trash'
alias ems='open -a /Applications/Emacs.app $@'
alias em='ems'
# alias emc="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

emt() { emc "$@" & }
alias sourcetree='open -a SourceTree'
alias st='sourcetree'
# alias sip='/System/Library/Frameworks/Python.framework/Versions/2.7/bin/sip'

# Alias for tools
alias ccc='/Users/SuperiMan/repo/colorgcc/colorgcc.pl'

#============= Dir alias =============
alias cdatomic='cd /Users/chuan.du/datomic-home/datomic-pro-0.9.5153'
alias csubl='cd /Users/duyongqinchuan/Library/Application\ Support/Sublime\ Text\ 3/Packages/User'
alias cla='cd /Users/SuperiMan/Dropbox/Code/leapArm'
alias comp="cd /Users/SuperiMan/Dropbox/Courses/COMP\ 3005\ Database\ Management\ Systems"
alias czsh='cd ~/.oh-my-zsh'
alias cb='cd ~/repo/CourseBuilder/'
alias glight='cd ~/repo/GestureLight'
alias gesl='cd /Users/SuperiMan/repo/GestureLight/'
alias gblog='cd /Users/SuperiMan/Dropbox/Code/WebSite/YongqinchuanDu.com/duqcyxwd.github.com'
alias cblog='cd /Users/SuperiMan/Dropbox/Code/WebSite/YongqinchuanDu.com/duqcyxwd.github.com'
alias myparker='cd ~/repo/parker/'
alias nova='cd ~/repo/nova'
alias mp='myparker'
alias mp2='cd ~/repo/parker2/'
alias mp3='cd ~/repo/parker3/'

alias cm='/Users/chuan.du/repo/devops/deployments/medium'

alias plataea-clean="rm ~/.m2/repository/cenx/plataea/*"
alias clean-plataea=plataea-clean

alias cmr=ramesseum
alias cmp=plataea
alias cml=levski
alias cany='cd /opt/cenx/application/analytics/'

# CD to any directory with auto complete
export repodir="/Users/chuan.du/repo/"
function c () {cd $repodir$1}
compctl -g $repodir'*(:t:r)' c


export repodir2="/opt/cenx/application/"
function opt () {cd $repodir2$1}
compctl -g $repodir'*(:t:r)' opt

#============= Powerful and Common alias =============
alias copy="echo 'Copy file path' && tr -d '\n' | pbcopy"
alias cpwd='echo "copy currenty directory" && pwd |pbcopy'

#Copy file path to clipboard
#greadlink -f development.pem
alias cpath=getPath
getPath() {greadlink -f $1 |tr -d '\n' | pbcopy}

alias cf='pbpaste | pbcopy' # clean format of clipboard

#============= Git alias =============
git config --global color.ui true
alias tag="echo ' git tag v1.0.0 \n git tag -a v1.2 9fceb02 \n git push origin v1.5 \n git push origin --tags'"

#=== Working ====

alias gu="git add project.clj && git commit -m 'Upversion'"
alias gbc="echo 'Copy current branch name' && git rev-parse --abbrev-ref HEAD |pbcopy && git branch"
alias gb-update-five-one="git fetch -p && git merge origin/r/5.1.x"
alias gbu51=gb-update-five-one
alias gbui="echo 'git branch update with integration' && git fetch -p && git merge origin/integration"
alias gbu61="git fetch -p && git merge origin/r/6.1.0.x"

alias gcoi="git checkout integration"
alias gcoip="git checkout integration && git pull"
alias gco51="git fetch -p && git checkout r/5.1.x && git pull"
alias gco61="git fetch -p && git checkout r/6.1.0.x && git pull"

alias gfco='git fetch -p && git checkout'

#=== Tool  ====
function parse_git_branch() { git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/' }

function git_create_branch() {
    if [ -z "$1" ] ; then
        echo "*******************************************"
        echo "*   !!! WARNING !!!  Branch not created   *"
        echo "*******************************************"
        echo ""
        echo "Silly me!!! I need to specify a parameter, the branch.."
    else
        current_branch=$(parse_git_branch)
        set -x
        git checkout -b $1
        if [ $? -eq 0 ] ; then
            git push --set-upstream origin $1
            if [ $? -ne 0 ] ; then
                git checkout ${current_branch}
                git branch -D $1
            fi
        fi
        set +x
    fi
}

alias gcob='gco -b'
alias gcobr='echo "Create branch and remote branch" & git_create_branch'

# Clean merged Branch
alias git-merged-branch-clean='git branch --merged | grep -v "\*" | xargs -n 1 git branch -d'
# Delete all branchs excep current branch
alias gbdelete='gb | grep "f/CD" | grep -v "\*" |xargs -n 1 git branch -D'

#=== Other  ====


# Configure git as personal repo
alias config-git="git config --local user.name 'Yongqinchuan Du' && git config --local user.email 'duqcyxwd@gmail.com'"

#===== Git alist =====
alias gds='git diff -w --stat'
alias gfp='git fetch -p'
#pretty git one line git log
alias gh='git log --pretty=tformat:"%h %ad | %s%d [%an]" --graph --date=short'
#show only the file names changed in commit
alias gsf='git show --pretty="format:" --name-only'
#run gitk
alias gk='gitk --all&'
#show all git aliases
alias gas='alias|grep git'
#clean all but the stuff the stuff that we would like preserved like .ccache, xmls catalog etc
#clean -dxf will wipe everything requiring user to source gitenv again
#alias gclean='pushd $MY_GIT_TOP > /dev/null && git submodule foreach --recursive 'git clean -xdf' && git clean -xdf -e .ccache -e .flex_dbg -e remap_catalog.xml && popd > /dev/null'

alias gre='git recent | head'


# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment following line if you want to  shown in the command execution time stamp
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)

plugins=(git)

source $ZSH/oh-my-zsh.sh
# User configuration

export PATH=$HOME/bin:$HOME/script-tool:/usr/local/bin:$PATH
# export MANPATH="/usr/local/man:$MANPATH"

# # Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
export PATH="/Users/chuan.du/repo/dev-env/bin:$PATH"

#========== Other helper script ===========

# Convert m4a/wma to mp3
# mkdir newfiles
# for f in *.m4a; do ffmpeg -i "$f" -codec:v copy -codec:a libmp3lame -q:a 2 newfiles/"${f%.m4a}.mp3"; done
# for f in *.wma; do ffmpeg -i "$f" -codec:v copy -codec:a libmp3lame -q:a 2 newfiles/"${f%.wma}.mp3"; done

#
## Hacky
#if [ $(which docker-credential-osxkeychain) ]; then
#        unlink $(which docker-credential-osxkeychain)
#    fi
