#!/bin/bash
DOTFILE=$(git rev-parse --show-toplevel)
echo "$DOTFILE"

ln -s "$DOTFILE/DotFiles/.tmux.conf" "$HOME/.tmux.conf"
