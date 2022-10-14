#!/bin/bash

DOTFILE=$(git rev-parse --show-toplevel)
echo "$DOTFILE"

# Atom editor settings
# echo -n "Copying Atom settings.."

# mv -f ~/.atom ~/dotfiles_old/
# ln -s $DOTFILE/atom ~/.atom
# echo "done"

export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local
export XDG_STATE_HOME=$HOME/.local/state
export XDG_CACHE_HOME=$HOME/.cache

mkdir -p $XDG_CONFIG_HOME
mkdir -p $XDG_DATA_HOME
mkdir -p $XDG_STATE_HOME
mkdir -p $XDG_CACHE_HOME
mkdir -p $XDG_CACHE_HOME/fag

# ZSH
mkdir -p $HOME/.zsh_history_bk
ln -s $DOTFILE/zsh $HOME/.config/zsh
ln -s $DOTFILE/zsh/.zshenv ~/.zshenv
ln -s $DOTFILE/zsh/.p10k.zsh ~/.p10k.zsh
ln -s $DOTFILE/vim/.ideavimrc ~/.ideavimrc
ln -s $DOTFILE/gitglobalignore ~/.config/gitglobalignore

touch ~/.zshrc-local.sh

case `uname` in
  Darwin)
    # commands for OS X go here
    ;;
  Linux)
    # commands for Linux go here

    if ! type zsh > /dev/null; then
      sudo apt install zsh
    fi

    if ! type fzf > /dev/null; then

        echo "install fzf"
        curl -L -o fzf.tar.gz https://github.com/junegunn/fzf/releases/download/0.33.0/fzf-0.33.0-linux_amd64.tar.gz
        tar -xzvf fzf.tar.gz
        sudo mv ./fzf /usr/local/bin
        sudo mkdir -p ~/.cache/fzf
        /usr/bin/rm fzf.tar.gz

    fi

    # Seutp fd
    if ! type fd > /dev/null; then
        sudo apt install fd-find
        sudo ln -s /usr/bin/fdfind /usr/local/bin/fd
    fi

    # Seutp exa
    if ! type exa > /dev/null; then
        curl -L -o exa.zip https://github.com/ogham/exa/releases/download/v0.10.1/exa-linux-x86_64-v0.10.1.zip
        sudo apt-get install unzip
        unzip exa.zip -d exa
        sudo mv ./exa/bin/exa /usr/local/bin
        /usr/bin/rm -r exa exa.zip
    fi

    ZINIT_HOME="${XDG_DATA_HOME:-${HOME}/.local}/zinit/zinit.git"
    if [[ ! -f "${ZINIT_HOME}/zinit.zsh" ]] ; then
      mkdir -p "$(dirname $ZINIT_HOME)"
      git clone https://github.com/zdharma-continuum/zinit.git "$ZINIT_HOME"
    fi

esac

