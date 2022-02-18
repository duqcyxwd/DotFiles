#!/bin/bash

DOTFILE=$(git rev-parse --show-toplevel)
echo "$DOTFILE"

# Atom editor settings
# echo -n "Copying Atom settings.."

# mv -f ~/.atom ~/dotfiles_old/
# ln -s $DOTFILE/atom ~/.atom
# echo "done"

# ZSH
ln -s $DOTFILE/zsh $HOME/.config/zsh
ln -s $DOTFILE/zsh/.zshenv ~/.zshenv
ln -s $DOTFILE/zsh/.p10k.zsh ~/.p10k.zsh

ln -s /Users/EYONDUU/duqcyxwd/DotFiles/gitglobalignore ~/.config/gitglobalignore

