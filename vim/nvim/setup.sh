#!/bin/bash
DOTFILE=$(git rev-parse --show-toplevel)
echo "DOTFILE PATH: "$DOTFILE

# ln -s $DOTFILE/vim/nvim $HOME/.config/vim
# ln -s $DOTFILE/vim/nvim/init.lua $HOME/.config/nvim/init.lua
# ln -s "$DOTFILE/vim/nvim/coc/package.json" "$XDG_DATA_HOME/coc/extensions/package.json"

ln -s $DOTFILE/vim/nvim/lua $HOME/.config/nvim/lua
ln -s $DOTFILE/vim/nvim/coc-settings.json $HOME/.config/nvim/coc-settings.json
ln -s $DOTFILE/vim/nvim/init.lua $HOME/.config/nvim/init.lua
ln -s $DOTFILE/vim/nvim/init.vim $HOME/.config/nvim/vimrc.vim
ln -s $DOTFILE/vim/nvim/log $HOME/.config/nvim/log
