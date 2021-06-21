#!/usr/bin/env bash

echo ""
echo "Starting bootstrapping"
echo "-------------------------------------------------------------------------------"

# Install template from https://gist.github.com/codeinthehole/26b37efa67041e1307db
# Check for Homebrew, install if we don't have it
if test ! $(which brew); then
    echo "Installing homebrew..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# # Update homebrew recipes
# # Not working, I think my OS is out of date
# brew update

# # Install GNU core utilities (those that come with OS X are outdated)
# brew tap homebrew/dupes
# brew install coreutils
# brew install gnu-sed --with-default-names
# brew install gnu-tar --with-default-names
# brew install gnu-indent --with-default-names
# brew install gnu-which --with-default-names
# brew install gnu-grep --with-default-names


# # Install Bash 5.1
# brew install bash


PACKAGES=(

    ack
    alacritty
    bat
    boxes
    cmake               # Cross-platform make
    exa
    ffmpeg
    fzf
    git
    git-extras
    gotop
    grc
    helm
    htop
    jq
    lolcat
    luarocks            # The Lua package manager
    maven               # https://github.com/rajivkanaujia/alphaworks/wiki/Installing-Maven
    pget                # Parallel file download client
    ripgrep             # rg, https://github.com/BurntSushi/ripgrep
    terminal-notifier
    the_silver_searcher # ag
    tmux
    tree
    vim
    watch
    wget

    # python
    # python3

    # WIP
    # nnn #I build nnn
    ranger

)

# echo "Installing packages: ${PACKAGES[@]}"
# echo ""
# brew install ${PACKAGES[@]}


echo "Upgrade packages: ${PACKAGES[@]}"
echo ""
brew upgrade ${PACKAGES[@]}



# Head version of neovim
# brew install --HEAD neovim



# NPM Packages:

# For fzf-preview in neovim
# npm install -g neovim


echo ""
