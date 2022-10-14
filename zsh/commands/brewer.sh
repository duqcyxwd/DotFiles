#!/bin/bash
# This script is from https://github.com/kawarimidoll/dotfiles/blob/master/etc/mac/brewer.sh
# homebrew update script
set -e

brew_with_echo() {
  echo "brew $*"
  brew "$@"
}

# Update
# # brew_with_echo doctor
# brew_with_echo update
# brew_with_echo upgrade
# brew_with_echo upgrade --cask
# brew_with_echo cleanup -s

