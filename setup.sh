#!/bin/bash

DOTFILE=$(git rev-parse --show-toplevel)
echo $DOTFILE

mkdir ~/dotfiles_old/


# Atom editor settings
echo -n "Copying Atom settings.."

mv -f ~/.atom ~/dotfiles_old/
ln -s $DOTFILE/atom ~/.atom
echo "done"
