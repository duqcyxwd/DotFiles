#!/bin/sh
# SECTION: : GENERAL ALIAS
# --------------------------------------------------------------------------
alias mvimr='mvim --remote'

{
    if [ $commands[nvim] ]; then
      alias vim='nvim'
      alias vi='nvim'
      alias vir='usr/bin/vi'
    fi
}
alias mypath='echo ${PATH//:/\\n}'
alias mvnt='/usr/local/bin/mvn -T 1C'



# ln -s ~/.SpaceVim ~/.vim to use Spacevim
alias svi=/usr/local/bin/vi                   # Use vi to open spacevim
# Macvim still using .vim

# alias v='nvim -u ~/.nvim/nvimrc'

# Remote control Neovim processes.
# TODO auto complete for nvr
# https://github.com/mhinz/neovim-remote
alias vim_remote_notes='echo "nsvc is to start static vim server, nvr is the client"'
# alias vr='nvr'
alias nvr="/usr/local/bin/nvr --nostart -p"
alias nvrr="/usr/local/bin/nvr"

alias history='fc -l 1'

alias -g timeElapsed="pv -F 'Elapsed time: %t'"

alias vimdiff="nvim -d"
alias mvimdiff="mvim -d"

# https://github.com/ranger/ranger/issues/68
# Fix ranger color issue in tmux
alias ranger='TERM=xterm-256color ranger'


#============= zshrc config =============
# alias mz='nvim $ZDOTDIR/.zshrc && shfmt -i 4 -s -w -ci $ZDOTDIR/.zshrc'
alias mz='nvim $ZDOTDIR/.zshrc'

alias fzfc='fzf | tr -d "\n" | pbcopy && pbpaste'


alias mysql="/Applications/XAMPP/xamppfiles/bin/mysql --use=root"


#============= Kafka =============

alias kafkacat=kcat
alias topics="kcat -b localhost:9092 -L -J | jq -S -r '.topics |=sort_by(.topic) | .topics[].topic'"

alias topicsi="runcached kcat -b localhost:9092 -L -J | awk 'NR>1'| jq -S -r '.topics |=sort_by(.topic) | .topics[].topic' | fzf"
alias topics0='kafka-topics.sh --list --zookeeper localhost'
alias topics1='kafka-topics.sh --bootstrap-server localhost:9092 --command-config $KAFKA_CONFIG/cust.properties --list'

alias offset='ee "kafka-run-class.sh kafka.tools.GetOffsetShell --broker-list localhost:9092 --time -1 --topic"'
# coffset --group groupID
alias coffset='ee "kafka-run-class.sh kafka.tools.ConsumerOffsetChecker --zookeeper localhost:2181 --topic"'
# kafka-run-class.sh kafka.tools.GetOffsetShell --broker-list localhost:9092 --time -1 --topic
# kafka-consumer-groups --bootstrap-server localhost:9092 --describe --group nbi-adaptor-2

# Not use this anymore
alias kafka21="cd /usr/local && ln -s kafka_2.12-2.1.0 kafka"
alias kafka08="cd /usr/local && ln -s kafka_2.9.1-0.8.2.2 kafka"


#============= Applications =============
alias copen='open -a Google\ Chrome'
alias ems='open -a /Applications/Emacs.app $@'
alias py='python'


case `uname` in
  Darwin)
    # commands for OS X go here
    alias rmt='/bin/rm'
    alias rm="trash"
    ;;
  Linux)
    # commands for Linux go here
    ;;
esac

alias rmi="lai | xargs rm -r"


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
#Copy file path to clipboard greadlink: GNU readlink
alias cf='pbpaste | pbcopy' # clean format of clipboard
alias dir='dirs -v'


export GREP_COLOR='1;33'
alias grepc='grep --color=always'
alias cgrep='grep --color=always'


## Fun
alias test-passed='if [ "$?" -eq "0" ]; then lolcat ~/.tp -a -s 40 -d 2; fi;'

## grc/grcat
alias gcat="grcat ~/.config/grc/log"

# --------------------------------------------------------------------------

function take()    { mkdir -p $@ && cd ${@:$#} }
function whichl()  { exa -lbFa -la $(which $@) }
function whichv()  { vim $(which $@) }
function whichcd() { cd $(dirname $(greadlink -f $(which $@))) }
function whichc()  { cat $(which $@) }
function whichb()  { bat $(which $@) }
function whichp()  { quick-preview $(which $@) }

function cdf() { cd $(dirname $(greadlink -f $@)) }
function cdroot() { cd $(git root) }
alias cdr=cdroot


alias sm='smerge .'
