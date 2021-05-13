#!/bin/bash
DOTFILE=$(git rev-parse --show-toplevel)
echo "$DOTFILE"

mkdir -p  "$HOME/.config/alacritty"
ln -s "$DOTFILE/.config/alacritty/alacritty.yml" "$HOME/.config/alacritty/alacritty.yml"
