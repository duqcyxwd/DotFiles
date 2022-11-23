#!/bin/bash
# ------------------------------------------------------------------
# [Yongqinchuan]
#                Setup zsh environment
# ------------------------------------------------------------------
# Absolute path to this script, e.g. /home/user/bin/foo.sh
SCRIPT=$(readlink -f "$0")
# Absolute path this script is in, thus /home/user/bin
BASEDIR=$(dirname "$SCRIPT")

# Clean up
# rm -rf $HOME/.config/zsh
# rm -rf ~/.zshenv
# rm -rf ~/.p10k.zsh
# ln -s $BASEDIR/ $HOME/.config/zsh
# ln -s $BASEDIR/.zshenv ~/.zshenv
# ln -s $BASEDIR/.p10k.zsh ~/.p10k.zsh


# Install zinit
# https://github.com/zdharma-continuum/zinit
# bash -c "$(curl --fail --show-error --silent --location https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"
