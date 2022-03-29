#!/bin/bash
# This script is from https://github.com/kawarimidoll/dotfiles/blob/master/etc/mac/brewer.sh
# homebrew update script

set -e

brew_with_echo() {
  echo "brew $*"
  brew "$@"
}

# brew_with_echo doctor
brew_with_echo update
brew_with_echo upgrade
brew_with_echo upgrade --cask
brew_with_echo cleanup -s

echo "log brew list > ${XDG_CONFIG_HOME}/zsh/brew-list.log"
{
  date "+timestamp: %F %T %Z"
  brew_with_echo tap              | sed 's/^/tap: /'
  brew_with_echo leaves           | sed 's/^/brew: /'
  brew_with_echo list --cask      | sed 's/^/cask: /'
} > "${XDG_CONFIG_HOME}/zsh/brew-list.log"
echo "log brew list with version > ${XDG_CONFIG_HOME}/zsh/brew-list-version.log"
{
  date "+timestamp: %F %T %Z"
  brew_with_echo list --version   | sed 's/^/brew: /'
} > "${XDG_CONFIG_HOME}/zsh/brew-list-version.log"
echo 'done.'
