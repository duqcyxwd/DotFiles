# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="blinks"
ZSH_THEME="agnoster"
# Alias
alias zshconfig="vim ~/.zshrc"
alias ohmyzsh="vim ~/.oh-my-zsh"
alias rs="source ~/.zshrc"
alias mz="vim ~/.zshrc"
alias ma="mz"
alias ca="less ~/.zshrc"
alias sch="qlmanage -p /Users/SuperiMan/Documents/2014\ Fall\ Time\ table.png"
alias cf='pbpaste | pbcopy' # clean format of clipboard
alias mysql="/Applications/XAMPP/xamppfiles/bin/mysql --use=root"
alias notes="mvim /Users/SuperiMan/repo/Notes/Git_Vim_Linux.md"
alias pcb='pwd |pbcopy'
alias py='python'

# Alias for tools

alias ccc='/Users/SuperiMan/repo/colorgcc/colorgcc.pl'

# Dir alias
alias csubl='cd /Users/duyongqinchuan/Library/Application\ Support/Sublime\ Text\ 3/Packages/User'
alias comp="cd /Users/SuperiMan/Dropbox/Courses/COMP\ 3005\ Database\ Management\ Systems"
alias czsh='cd ~/.oh-my-zsh'
alias cb='cd ~/repo/CourseBuilder/'
alias glight='cd ~/repo/GestureLight'
alias gesl='cd /Users/SuperiMan/repo/GestureLight/'

#Common alias
# alias ll='ls -la'
# Git sliases
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
alias gclean='pushd $MY_GIT_TOP > /dev/null && git submodule foreach --recursive 'git clean -xdf' && git clean -xdf -e .ccache -e .flex_dbg -e remap_catalog.xml && popd > /dev/null'


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

