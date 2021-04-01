#!/bin/sh
# SECTION: : ZSH PLAYGROUND {{{1
# --------------------------------------------------------------------------
_clean_my_cache() {
  # fd -d 1 --type d --changed-before=500d
  fd -d 1 . ~/work_credential/temp/cache --changed-before=20d | xargs trash
}

# _clean_my_cache



# WIP fzf_ls_widget {{{1
fzf_ls_widget() {

  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2> /dev/null
  setopt localoptions pipefail no_aliases 2> /dev/null

  # --bind=ctrl-z:ignore
  LBUFFER="${LBUFFER}$(ls_fuzzy_preview)"
  local ret=$?

  zle reset-prompt
  return $ret
}
zle -N fzf_ls_widget

# WIP fh - repeat history {{{1
# FZF preview
function fh() {
  # print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')
  
  # Method 1
  # print -z $( ([ -n "$ZSH_NAME" ] && fc -li 1 || history) | \
  #   sed 's/^[ ]*//' | \
  #   fzf --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}' | \
  #   sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //' \
  #            )

  # Method 2
  # print -z $( ([ -n "$ZSH_NAME" ] && fc -li 1 || history) | \
  #   fzf --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}' | \
  #   sed 's/^[ ]*//' | sed 's/* /  /' | \
  #   sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //' \
  #            )
  
  # Method 3
  # fc -rli 0 > ~/.zsh_history_cache
  print -z $( cat ~/.zsh_history_cache | \
    sed 's/^[ ]*//' | \
    fzf --reverse --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}' | \
    sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //' \
             )


  # --reverse
}

# TODO a pip fzf preview, can search and view {{{1

# fc -li 1
#  cat temp| sed 's/^[ ]*//' |sed 's/* /  /' | fzf --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}'
#  cat temp| sed 's/^[ ]*//' |sed 's/* /  /' | fzf --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}' | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //'
# cat ~/.zsh_history | sed 's/;/  /'| fzf --with-nth 3..

# crontab crontab-config

