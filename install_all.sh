#!/usr/bin/env bash(brew --prefix)

echo ""
echo "Starting bootstrapping"
echo "-------------------------------------------------------------------------------"
# Brew: {{{1
echo ""
echo "brew "
echo "-------------------------------------------------------------------------------"

# Brew core {{{2
# Install template from https://gist.github.com/codeinthehole/26b37efa67041e1307db
# Check for Homebrew, install if we don't have it
if test ! $(which brew); then
    echo "Installing homebrew..."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi



# # Update homebrew recipes
# brew update

# # Install GNU core utilities (those that come with OS X are outdated)
# brew tap homebrew/dupes
brew install coreutils
brew install gnu-sed

# # Install Bash 5.1
# brew install bash

PACKAGES=( # Brew Package {{{2
    ack
    bat
    boxes
    cmake               # Cross-platform make
    clojure-lsp-native  # clojure-lsp
    exa
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
    grep                # grep
    tmux
    tree
    vim
    watch
    wget
    php

    # python
    # python3

    # WIP
    # nnn #I build nnn
    ranger
    jump
    hub

)


UNINSTALL_PROGRAMS=""

# Brew install #{{{2
# echo "Installing packages: ${PACKAGES[@]}"
# echo ""
# brew install ${PACKAGES[@]}


# Some special thing....

# Brew Pins:
# brew pin maven
# brew pin openjdk
# brew pin openjdk@8
# brew tap AdoptOpenJDK/openjdk
# brew cask install adoptopenjdk8
# # https://stackoverflow.com/questions/24342886/how-to-install-java-8-on-mac
# brew cask install adoptopenjdk/openjdk/adoptopenjdk8
# # Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk
# export JAVA_HOME=/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home


# brew install zookeeper #zkcli
# # Better zkcli https://github.com/let-us-go/zkcli
# # brew tap let-us-go/zkcli
# # brew install zkcli


# Brew Upgrade #{{{2
echo "Upgrade packages: ${PACKAGES[@]}"
echo ""
brew upgrade ${PACKAGES[@]}

for program in $UNINSTALL_PROGRAMS ; do brew uninstall --force $program ; done

# Brew Cleanup #{{{2
# echo "Cleaning up..."
# brew cleanup

# Brew Cask #{{{2
echo "Installing cask..."
CASKS=(
    amethyst
    alacritty
    emacs
    graphiql
    cool-retro-term
    graphql-playground

)

echo "Installing cask apps..."
# brew install --cask ${CASKS[@]}
brew upgrade --cask "${CASKS[@]}"

#   brew cask install rectangle

# Npm {{{1

echo ""
echo "npm "
echo "-------------------------------------------------------------------------------"

NPM_PACKAGES=(
    how-2
    tldr
    gitopen
    git-recent
    )


echo "npm installing ${NPM_PACKAGES[@]}"
npm install -g "${NPM_PACKAGES[@]}"

# npm install -g gitopen
# npm install -g git-recent



# Pip {{{1
echo ""
echo "Installing Python packages..."
echo "-------------------------------------------------------------------------------"
PYTHON_PACKAGES=(
    ipython
    virtualenv
    virtualenvwrapper

    kafka-python
    python-snappy
)
# Uninstall other kafka and snappy
# sudo pip install ${PYTHON_PACKAGES[@]}

# MY others  {{{1
# ---------------------------------------------------------------------------------
# Head version of neovim
# brew install --HEAD neovim
brew install --HEAD tree-sitter luajit neovim
brew install neovim-remote

npm install -g diagnostic-languageserver


# brew list | fzf_tp --query="$1" +m --preview 'runcached brew info {}'

# NPM Packages:

# For fzf-preview in neovim
# npm install -g neovim


WIP
brew tap conduktor/brew
brew install conduktor

brew install borkdude/brew/jet
brew install lua-language-server



# }}}1

echo ""
