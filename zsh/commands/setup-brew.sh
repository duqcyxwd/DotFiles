#!/bin/bash
# This script is from https://github.com/kawarimidoll/dotfiles/blob/master/etc/mac/brewer.sh

die() {
  echo "$1"
  echo "  terminated."
  exit 1
}
brew_list="${XDG_CONFIG_HOME}/zsh/brew-list.log"

which curl >> /dev/null || die "curl is required."
which brew >> /dev/null || /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
which brew >> /dev/null || die "brew is required."
brew doctor || die "brew doctor raised error."
brew update

if [ -e "$brew_list" ]; then
  grep ^tap: "$brew_list"  | sed 's/tap: //'  | xargs -I_ brew tap _
  grep ^brew: "$brew_list" | sed 's/brew: //' | xargs brew install
  grep ^cask: "$brew_list" | sed 's/cask: //' | xargs brew install --cask
else
  echo "  brew-list.log is needed."
fi
brew cleanup
