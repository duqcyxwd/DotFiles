#!/bin/sh
# ZSH Directory

setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -- -='builtin cd -'
alias 1='builtin cd -'
alias 2='builtin cd -2'
alias 3='builtin cd -3'
alias 4='builtin cd -4'
alias 5='builtin cd -5'
alias 6='builtin cd -6'
alias 7='builtin cd -7'
alias 8='builtin cd -8'
alias 9='builtin cd -9'

alias md='mkdir -p'
alias rd=rmdir

