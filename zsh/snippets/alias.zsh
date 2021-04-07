#!/bin/sh
# SECTION: : GENERAL ALIAS
# --------------------------------------------------------------------------
# alias vi='/usr/local/bin/vim'
alias mvimr='mvim --remote'
alias vi='nvim'
alias vim='nvim'
# Remote control Neovim processes.
# TODO auto complete for nvr
alias vimr='nvr'
alias vimr='nsvc'

alias history='fc -l 1'

alias -g timeElapsed="pv -F 'Elapsed time: %t'"

alias vimdiff="nvim -d"
alias mvimdiff="mvim -d"


#============= zshrc config =============
alias mz='nvim $ZDOTDIR/.zshrc && shfmt -i 4 -s -w -ci $ZDOTDIR/.zshrc'

alias fzfc='fzf | tr -d "\n" | pbcopy && pbpaste'


alias mysql="/Applications/XAMPP/xamppfiles/bin/mysql --use=root"
alias notes="mvim ~/repo/Notes/CLojure.md"

# Not use this anymore
alias kafka21="cd /usr/local && ln -s kafka_2.12-2.1.0 kafka"
alias kafka08="cd /usr/local && ln -s kafka_2.9.1-0.8.2.2 kafka"

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

#============= Powerful and Common alias =============
alias cpwd='echo "copy currenty directory" && pwd |pbcopy'
#Copy file path to clipboard
cpath() {greadlink -f $1 | tr -d '\n' | pbcopy }
alias cf='pbpaste | pbcopy' # clean format of clipboard
alias dir='dirs -v'


export GREP_COLOR='1;33'
alias grepc='grep --color=always'
alias cgrep='grep --color=always'


## Fun
alias test-passed='if [ "$?" -eq "0" ]; then lolcat ~/.tp -a -s 40 -d 2; fi;'

# --------------------------------------------------------------------------
