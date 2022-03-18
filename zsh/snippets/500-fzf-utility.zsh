#!/bin/zsh

# TOOL: FZF RELATED FUNCTIONS

# FZF with Brew {{{1

# brew list | fzf_tp --query="$1" +m --preview 'brew info {}'

__BRREW_PREVIEW_CMD="BACKGROUND_UPDATE_MAX_AGE=30 runcached brew list --versions {1} && BACKGROUND_UPDATE_MAX_AGE=300 runcached brew info {1}"

# Install (one or multiple) selected application(s)
# using "brew search" as source input
# mnemonic [B]rew [I]nstall [P]lugin
brew_install_plugin_int() { # {{{2
  local inst=$(runcached brew search "$@" | fzf_tp --header-lines=1 -m --preview "$__BRREW_PREVIEW_CMD")

  if [[ $inst ]]; then
    for prog in $(echo $inst);
    do; brew install $prog; done;
  fi
}


# Update (one or multiple) selected application(s)
# mnemonic [B]rew [U]pdate [P]lugin
brew_update_plugin_int() {
  local upd=$(runcached --bg-update brew leaves | fzf_tp --header-lines=1 -m --preview "$__BRREW_PREVIEW_CMD")

  if [[ $upd ]]; then
    for prog in $(echo $upd);
    do; brew upgrade $prog; done;
  fi
}
# Delete (one or multiple) selected application(s)
# mnemonic [B]rew [C]lean [P]lugin (e.g. uninstall)
brew_clean_plugin_int() {
  local uninst=$(runcached --bg-update brew leaves | fzf_tp --header-lines=1 -m --preview "$__BRREW_PREVIEW_CMD")

  if [[ $uninst ]]; then
    for prog in $(echo $uninst);
    do; brew uninstall $prog; done;
  fi
}


brew_update_outdated_package_int() {
  local upd=$(runcached --bg-update brew outdated --verbose | fzf_tp --delimiter=\  --header-lines=1 -m --preview "$__BRREW_PREVIEW_CMD" | awk '{print $1}')

  if [[ $upd ]]; then
    for prog in $(echo $upd);
    do; brew upgrade $prog; done;
  fi
}


brew_view_plugin_int() {
  local uninst=$(runcached --bg-update brew list | fzf_tp --header-lines=1 -m --preview "$__BRREW_PREVIEW_CMD")

  if [[ $uninst ]]; then
    for prog in $(echo $uninst);
    do; brew info $prog; done;
  fi
}


alias bip=brew_install_plugin_int
alias bup=brew_update_plugin_int
alias bui=brew_clean_plugin_int
alias bcp=brew_clean_plugin_int
alias bvp=brew_view_plugin_int
alias buo=brew_update_outdated_package_int


# Install or open the webpage for the selected application
# using brew cask search as input source
# and display a info quickview window for the currently marked application
b_install() {
    local token
    token=$(runcached --bg-update brew search --casks "$1" | fzf_tp --query="$1" +m --preview "$__BRREW_PREVIEW_CMD" )

    if [ "x$token" != "x" ]
    then
        echo "(I)nstall or open the (h)omepage of $token"
        read input
        if [ $input = "i" ] || [ $input = "I" ]; then
            brew install --cask $token
        fi
        if [ $input = "h" ] || [ $input = "H" ]; then
            brew home $token
        fi
    fi
}

btap() {
  brew tap | fzf_tp +m --preview 'runcached brew ls --full-name --formula | grep {}'
}

buntap() {
  tap=$(brew tap | fzf_tp +m --preview 'runcached brew ls --full-name | grep {}')

  if [[ "$tap" == "" ]] ; then
    return 1
  fi

  ee brew untap $tap
}


# FZF with kill {{{1
# fkill - kill processes - list only the ones you can kill. Modified the earlier script.
# This is better than default completion
fkilll() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID  | fzf -m --header-lines=1 | awk '{print $2}')
    else
        pid=$(ps -ef | fzf -m --header-lines=1 | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}


f() { #{{{1
  # Use fd and fzf to get the args to a command.
  # Works only with zsh
  # Examples:
  # f mv # To move files. You can write the destination after selecting the files.
  # f 'echo Selected:'
  # f 'echo Selected music:' --extension mp3
  # fm rm # To rm files in current directory
    sels=( "${(@f)$(fd "${fd_default[@]}" "${@:2}"| fzf)}" )
    test -n "$sels" && print -z -- "$1 ${sels[@]:q:q}"
}

pathi() { #{{{1
  echo $PATH | tr ':' '\n' | sort | fzf
}
fpathi() { #{{{1
  echo $FPATH | tr ':' '\n' | sort | fzf
}

#}}}
