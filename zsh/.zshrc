# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh
export TERM="xterm-256color"

#=============================== PATH ===========================================
export PATH="./node_modules/.bin:$PATH"
export PATH="/Users/chuan.du/repo/dev-env/bin:$PATH"
export PATH=$HOME/bin:/usr/local/sbin:$HOME/script-tool:/usr/local/bin:$PATH

# export is required for python path
# export PYTHONPATH="/Users/chuan.du/repo/autotest/tests/component/cenx-rest-api:${PYTHONPATH}"
export NODE_PATH=/usr/lib/node_modules

# Tmuxinator
export EDITOR='vim'
# source ~/.bin/tmuxinator.zsh

#============================= Script tool: version ===============================
function echoAndEval() { echo $@ && eval $@; }
alias ee=echoAndEval
alias version='py /Users/chuan.du/script-tool/version.py ./'
alias v=version

function noti() {
	# terminal-notifier
	# Can use terminal-notifier if we want icon modification. Can't do noti confirm
	# Buildin noti
	osascript -e "display notification \"$1\" with title \"$2\""
}

function cc() {
	res=$(pbpaste | sed -e :a -e '$!N; s/\n//; ta' | sed 's/\(CD-[0-9]*\)/[[\1]](https:\/\/cenx-cf.atlassian.net\/browse\/\1) /')
	echo $res
}

function color-test() {
	clear
	cat /Users/chuan.du/temp/iterm-syntax-test.txt
}

#============================= system-clean-up ===============================
alias clean-m2-cenx="rm /Users/chuan.du/.m2/repository/cenx"

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
function docker-stats() { docker stats --format "table {{.Name}}\t{{.Container}}\t{{.CPUPerc}}\t{{.MemPerc}}\t{{.MemUsage}}\t{{.NetIO}}\t{{.BlockIO}}"; }
function docker-stats-peek() { docker stats --no-stream --format "table {{.Name}}\t{{.Container}}\t{{.CPUPerc}}\t{{.MemPerc}}\t{{.MemUsage}}\t{{.NetIO}}\t{{.BlockIO}}"; }
function docker-stop() {ee 'docker stop $(docker ps -q)'}
function docker-rm-stopped() {ee "docker rm $(docker ps -aq -f status=exited)"}

function docker-ps() {
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

function dpsa() { echo "docker list all containers" && dps -a $@; }

function dps-old() { echo "Docker ps old" && docker ps -a --format "table {{.Names}}\t{{.Image}}\t{{.Status}}" | awk 'NR == 1; NR > 1 { print $0 | "sort" }'; }
function dps-old-p() { echo "Docker ps old with port" && docker ps -a --format "table {{.Names}}\t{{.Image}}\t{{.Status}}\t{{.Ports}}" | awk 'NR == 1; NR > 1 { print $0 | "sort" }'; }

function pre-docker() {
	export DOCKER_MACHINE_IP=$(docker-machine ip)
	eval $(docker-machine env)
}

#============================== Docker old ===============================
#export DOCKER_HOST=tcp://192.168.59.103:2375
#unset DOCKER_HOST
#unset DOCKER_CERT_PATH
#unset DOCKER_TLS_VERIFY

#alias cortxf='docker run --rm -t -v `pwd`:/opt/cenx docker.cenx.localnet:5000/deployer -f /opt/cenx/solr.yaml'
#alias cortx0='docker run --rm -t -v `pwd`:/opt/cenx docker.cenx.localnet:5000/deployer'
#alias cortx-small='docker run --rm -t -v /opt/cenx:/opt/cenx docker.cenx.localnet:5000/deployer'

#============================ local wildfly ==============================
# Unfinished
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

# export WILDFLY_HOME=/Users/chuan.du/CENX/wildfly-10.1.0.Final
# Wildfly created by Eclipse
export WILDFLY_HOME=/Users/chuan.du/wildfly-10.0.0.Final
export WILDFLY_DEPLOY=${WILDFLY_HOME}/standalone/deployments
export WILDFLY_BIN=${WILDFLY_HOME}/bin

alias wildfly-standalone-deploy-clean='echo "Clean wildfly deploy directory" && rm $WILDFLY_DEPLOY/*.war*'
alias wildfly-standalone-deploy-war="echo 'deploy war from target' && cp target/*.war $WILDFLY_DEPLOY"
alias wildfly-standalone-deploy-war-enable="echo 'enable app from deployment' && rm $WILDFLY_DEPLOY/*.undeployed && rm $WILDFLY_DEPLOY/*.failed"
alias wildfly-standalone-deploy-war-disable-all="echo 'disable app from ' $JBOSS_HOME && rm $WILDFLY_DEPLOY/*.deployed"
alias wildfly-standalone-restart='$WILDFLY_BIN/jboss-cli.sh -c --command=":shutdown(restart=true)"'
alias wildfly-standalone='$WILDFLY_BIN/standalone.sh'
#wildfly commands http://developer-should-know.com/post/76413579867/how-to-restart-jboss-and-wildfly-using-command

alias lcid="lci && deploy-war"
alias deploy-war-and-standalone="echo 'deploy war and run standalone' && deploy-clean && deploy-war && wildfly-standalone"

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

alias vi='/usr/local/bin/vim'

alias time-out-hide='subl /Applications/VLC.app/Contents/Info.plist'
alias later="nohup /Users/chuan.du/repo/DotFiles/otherTool/later.pl"

# FileSearch
function f() {echo 'find . -iname "*'$1'*" '${@:2} && find . -iname "*$1*" ${@:2} }
function r-old() {echo ' grep "'$1'" '${@:2}' -R .' && grep "$1" ${@:2} -R . }
function r() {echo ' Replaced with ag'}

# Watch function
# TODO Add this back
# function mywatch() {while :; do a=$($@); clear; echo "$(date)\n\n$a"; sleep 2;  done}
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
	open -a /Applications/Font\ Book.app
}

# Alias
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
alias rs='ee "source ~/.zshrc"'
alias mz='ee "vim ~/.zshrc"'
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
# alias sip='/System/Library/Frameworks/Python.framework/Versions/2.7/bin/sip'

# Alias for tools
alias ccc='/Users/SuperiMan/repo/colorgcc/colorgcc.pl'

# Alias for echo done
alias tf='echo "Task finished"'

#============= Database =============
#alias sql="echo 'psql to localhost' && psql -h 'localhost' -U 'postgres'"
alias sql="echo 'psql to localhost' && ee \"export PAGER='less -SF' && psql -h 'localhost' -U 'postgres'\""

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

# CD to any directory with auto complete
export repodir="/Users/chuan.du/repo/"
function c() {cd $repodir$1}
compctl -g $repodir'*(:t:r)' c

export repodir2="/opt/cenx/application/"
function opt() {cd $repodir2$1}
compctl -g $repodir2'*(:t:r)' opt

export repodir3="/Users/chuan.du/github/"
function cd-gh() {cd $repodir3$1}
compctl -g $repodir3'*(:t:r)' cd-gh

#============= Powerful and Common alias =============
# alias copy="echo 'Copy file path' && tr -d '\n' | pbcopy"
# Don't know
# alias copy="tr -d '\n' | pbcopy"
alias cpwd='echo "copy currenty directory" && pwd |pbcopy'

#Copy file path to clipboard
#greadlink -f development.pem
function getPath() {greadlink -f $1 | tr -d '\n' | pbcopy }
alias cpath=getPath
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
alias gbu60x="git fetch -p && git merge origin/r/6.0.x"
alias gbu800="git fetch -p && git merge origin/r/8.0.0.x"
alias gbu810="git fetch -p && git merge origin/r/8.1.0.x"

alias gcoi="git checkout integration && git pull"
alias gcoip="git checkout integration && git pull"
alias gco51="git fetch -p && git checkout r/5.1.x && git pull"
alias gco61="git fetch -p && git checkout r/6.1.0.x && git pull"

alias gfco='git fetch -p && git checkout'

#=== Special Git Tool  ====
function get_git_current_branch() { git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'; }

function get_current_branch() { git branch 2>/dev/null | sed -e "/^[^*]/d" -e "s/* \(.*\)/\1/"; }

function git_create_branch() {
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

function gitopen-current-branch() {
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

#pretty git one line git log
alias gh="ee 'git log --pretty=tformat:\"%h %ad | %s%d [%an]\" --graph --date=short'"
#show only the file names changed in commit
alias gsf='git show --pretty="format:" --name-only'

#show all git aliases
alias galias='alias|grep git'

# git clean up
#clean all but the stuff the stuff that we would like preserved like .ccache, xmls catalog etc
#clean -dxf will wipe everything requiring user to source gitenv again
#alias gclean='pushd $MY_GIT_TOP > /dev/null && git submodule foreach --recursive 'git clean -xdf' && git clean -xdf -e .ccache -e .flex_dbg -e remap_catalog.xml && popd > /dev/null'

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

alias lc='colorls -lA --sd'
alias lcg='colorls -lA --sd --gs'
#source $(dirname $(gem which colorls))/tab_complete.sh

# Autocompletion for teamocil
compctl -g '~/.teamocil/*(:t:r)' itermocil

#======================== Terminal Config ==================================
source ~/.zshrc-local.sh

#=============================== oh-my-zsh  ======================================
# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/

# ZSH_THEME="agnoster-cus"
# ZSH_THEME="agnoster"
# ZSH_THEME="powerlevel9k/powerlevel9k"

function load_POWERLEVEL9K() {
	power-version() {
		local color='%F{yellow}'
		PROJECT_VERSION=$(python /Users/chuan.du/script-tool/version.py ./ powermode)
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
		echo "THEME: SIMPLE POWERLEVEL9K"
		POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir)
		POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(command_execution_time)
	else
		echo "THEME: POWERLEVEL9K"
		POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(ram load ip vcs custom_version newline ssh dir)
		POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status vi_mode root_indicator command_execution_time background_jobs time)
	fi

	alias signs="open https://github.com/bhilburn/powerlevel9k#vcs-symbols"
}

#=========================== Antigen ==================================
function load_Antigen() {
	echo "Use antigen"
	source ~/antigen.zsh

	# Load the oh-my-zsh's library.
	antigen use oh-my-zsh

	# Bundles from the default repo (robbyrussell's oh-my-zsh).
	antigen bundle git
	antigen bundle git-extras
	antigen bundle osx
	antigen bundle mvn
	antigen bundle npm
	antigen bundle brew
	antigen bundle docker
	antigen bundle lein
	antigen bundle vi-mode
	antigen bundle tmuxinator
	antigen bundle paulmelnikow/zsh-startup-timer
	antigen bundle shayneholmes/zsh-iterm2colors

	alias ac=_iterm2colors_apply
	alias acl='echo $_iterm2colors_current'
	alias acr=_iterm2colors_apply_random

	# My  bundles
	antigen bundle command-not-found
	antigen bundle gimbo/iterm2-tabs.zsh
	antigen bundle gretzky/auto-color-ls
	# Not working Cuold be removed
	antigen bundle djui/alias-tips
	antigen bundle "MichaelAquilina/zsh-you-should-use"

	antigen bundle sei40kr/zsh-tmux-rename

	# Syntax highlighting bundle.
	antigen bundle zsh-users/zsh-syntax-highlighting
	antigen bundle zsh-users/zsh-autosuggestions
	# antigen bundle jimeh/zsh-peco-history
	# antigen bundle b4b4r07/zsh-history
	antigen bundle zdharma/history-search-multi-word
	# Something looks very powerful but not sure why I need it
	antigen bundle psprint/zsh-cmd-architect
	antigen bundle popstas/zsh-command-time

	# Theme
	antigen theme bhilburn/powerlevel9k powerlevel9k

	# Tell Antigen that you're done.
	antigen apply
}

#============================== antibody ======================================

function load_Antibody() {
	echo "Use antibody"
	source <(antibody init)
	antibody bundle bhilburn/powerlevel9k
	antibody bundle <~/.zsh_plugins.txt
}

#========================= oh-my-zsh DEFAULT ==================================
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# vi-mode
# plugins=(git git-extras osx mvn npm brew docker)

load_POWERLEVEL9K
if [ "$FIRSTTIME" -ne "1" ]; then
	echo "Start oh-my-zsh"
	source $ZSH/oh-my-zsh.sh
fi
FIRSTTIME=1
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

#========================= load zsh plugins ===================================
chooseantigen=0
chooseantibody=1
bundlechoose=0

if [ "$bundlechoose" -eq 0 ]; then
	load_Antigen
else
	load_Antibody
fi

# Some good keybind is overwrite by plugins or oh-my-zsh
bindkey '^k' autosuggest-accept
bindkey '^\n' autosuggest-execute
# bindkey "^R" history-incremental-search-backward
# bindkey "^S" history-incremental-search-forward

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
