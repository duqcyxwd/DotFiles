#!/bin/bash

DOTFILE=$(git rev-parse --show-toplevel)
echo "$DOTFILE"

ln -s $DOTFILE/typora/base.user.css "/Users/EYONDUU/Library/Application Support/abnerworks.Typora/themes/base.user.css"
