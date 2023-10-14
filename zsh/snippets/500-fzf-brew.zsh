#!/bin/zsh

# FZF with Brew {{{1
# -------------------------------------------------------------------------------

# brew list | fzf_tp --query="$1" +m --preview 'brew info {}'

__BRREW_PREVIEW_CMD="BACKGROUND_UPDATE_MAX_AGE=30 runcached brew list --versions {1} && BACKGROUND_UPDATE_MAX_AGE=300 runcached brew info {1}"

brew_list_int() { # {{{2
  local formula=$(runcached --bg-update brew list --full-name | fzf_tp --header-lines=1 -m --preview "$__BRREW_PREVIEW_CMD")

  if [[ $formula ]]; then
    for prog in $(echo $formula);
    do; brew info $prog; done;
  fi
}



# Install or open the webpage for the selected application
# using brew cask search as input source
# and display a info quickview window for the currently marked application
brew_install_int() { # {{{2
  local formula=$(runcached brew search "$@" | fzf_tp --header-lines=1 -m --preview "$__BRREW_PREVIEW_CMD")

  if [[ $formula ]]; then
    for prog in $(echo $formula);
    do; HOMEBREW_NO_AUTO_UPDATE=0 brew install $prog; done;
  fi
}

# Delete (one or multiple) selected application(s)
# mnemonic [B]rew [C]lean [P]lugin (e.g. uninstall)
brew_uninstall_int() { # {{{2
  local uninst=$(runcached --bg-update brew list --full-name | fzf_tp --header-lines=1 -m --preview "$__BRREW_PREVIEW_CMD")

  if [[ $uninst ]]; then
    for prog in $(echo $uninst);
    do; brew uninstall $prog; done;
  fi
}


brew_update_int() { # {{{2
  local formula=$(runcached --bg-update brew outdated --verbose | fzf_tp --delimiter=\  --header-lines=1 -m --preview "$__BRREW_PREVIEW_CMD" | awk '{print $1}')

  if [[ $formula ]]; then
    for prog in $(echo $formula);
    do; HOMEBREW_NO_AUTO_UPDATE=0 brew upgrade $prog; done;
  fi
}


brew_cask_install() { # {{{2
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

bbrew_tapi() { # {{{2
  brew tap | fzf +m --preview 'runcached brew ls --full-name --formula | grep {}'
}

buntapi() { # {{{2
  tap=$(brew tap | fzf_tp +m --preview 'runcached brew tap-info {} && echo "\nInstalled: " && runcached brew ls --full-name | grep {}')

  if [[ "$tap" == "" ]] ; then
    return 1
  fi

  ee brew untap $tap
}

brew_upgrade_safe(){
  HOMEBREW_NO_INSTALL_CLEANUP=0 HOMEBREW_NO_AUTO_UPDATE=0 brew upgrade $1
}

# FZF with Brew Alias # {{{1
# -------------------------------------------------------------------------------
alias bip=brew_install_int
alias buni=brew_uninstall_int
alias buninstalli=brew_uninstall_int
alias bli=brew_list_int
alias buoi=brew_update_int
alias bui=brew_update_int
alias bupi=brew_update_int

# other alias
# Dependencies
alias bdep="brew deps --installed --tree"
# brew autoremove
