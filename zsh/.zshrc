# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="agnoster-cus"

# Configuration only with local computer

alias gcoi="git checkout integration"
alias gcoip="git checkout integration && git pull"
alias cortxf='docker run --rm -t -v `pwd`:/opt/cenx docker.cenx.localnet:5000/deployer -f /opt/cenx/solr.yaml'
alias cortx0='docker run --rm -t -v `pwd`:/opt/cenx docker.cenx.localnet:5000/deployer'
alias cortx-small='docker run --rm -t -v /opt/cenx:/opt/cenx docker.cenx.localnet:5000/deployer'
alias time-out-hide='subl /Applications/VLC.app/Contents/Info.plist'

alias tf="t"
alias later="nohup /Users/chuan.du/repo/DotFiles/otherTool/later.pl"

t(){ timerToStartApplication "$@" &}
timerToStartApplication() {
   # sleep 1 && open -a /Applications/Time\ Out\ Free.app
   ti=$@
   print "sleep to start Timeout Free\nSleep $ti"
   pkill "Time Out Free"
   sleep $@
   print 'start Timeout Free'
   open -a /Applications/Time\ Out\ Free.app
   open -a /Applications/Font\ Book.app }

export DOCKER_HOST=tcp://192.168.59.103:2375
unset DOCKER_CERT_PATH
unset DOCKER_TLS_VERIFY

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

# Configure git as personal repo
alias cgit="git config --local user.name 'Yongqinchuan Du' & git config --local user.email 'duqcyxwd@gmail.com'"
# Alias for tools

alias ccc='/Users/SuperiMan/repo/colorgcc/colorgcc.pl'

# Dir alias
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

#Common alias
# alias ll='ls -la'
# CD to any directory with auto complete 
export repodir="/Users/chuan.du/repo/"
function c () {cd $repodir$1}
compctl -g $repodir'*(:t:r)' c

alias copy="tr -d '\n' | pbcopy"

# Git alias
git config --global color.ui true
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

export PATH=$HOME/bin:/usr/local/bin:$PATH
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
