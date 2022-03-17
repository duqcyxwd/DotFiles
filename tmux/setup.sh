#!/bin/bash
DOTFILE=$(git rev-parse --show-toplevel)
echo "$DOTFILE"

ln -s "$DOTFILE/tmux/.tmux.conf" "$HOME/.tmux.conf"
