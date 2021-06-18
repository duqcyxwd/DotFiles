#!/bin/sh
# print ${functions_source[_helm]}

# SECTION: : ZSH PLAYGROUND {{{1
# --------------------------------------------------------------------------
_clean_my_cache() {
  # fd -d 1 --type d --changed-before=500d
  fd -d 1 . ~/work_credential/temp/cache --changed-before=20d | xargs trash
}

# _clean_my_cache



# WIP fzf_ls_widget {{{1

# WIP fh - repeat history {{{1
# FZF history
function fh() {
  # print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')

  # Method 1
  # print -z $( ([ -n "$ZSH_NAME" ] && fc -li 1 || history) | \
  #   sed 's/^[ ]*//' | \
  #   fzf --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}' | \
  #   sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //' \
  #            )

  # Method 2
  # Use fzf delimiter
  # print -z $( ([ -n "$ZSH_NAME" ] && fc -rli 1 || history) | \
  #   sed 's/^[ ]*//' | \
  #   fzf --delimiter "  " --preview 'bat --color=always --highlight-line {1} $ZDOTDIR/.zsh_history' --preview-window '+{1}-5' | \
  #   sed 's/^[ ]*//' | sed 's/* /  /' | \
  #   sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //' \
  #            )

  # Method 3
  # Use Cached but rquire extra work
  # fc -rli 0 > ~/.zsh_history_cache
  # print -z $( cat ~/.zsh_history_cache | \
  #   sed 's/^[ ]*//' | \
  #   fzf --reverse --delimiter "  " --preview 'bat --color=always --highlight-line {1} ~/.zsh_history' --preview-window '+{1}' | \
  #   sed -E 's/ *[0-9]*\*? *//' | sed -E 's/[0-9\-]{10} [0-9:]{5}  //' \
  #            )

  # Method 4 live reload with cached
  # Use fzf delimiter
  HISTORY_CACHED_FILE=~/.cache/zsh/__history_reverse_cached
  HISTORY_CACHED_FILE_C=~/.cache/zsh/__history_reverse_cached_c
  HISTORY_CACHED_FILE_INDEXED=~/.cache/zsh/__history_reverse_cached_indexed
  # HISTORY_CACHED_FILE_ORIGINAL=~/.cache/zsh/__history_cached
  # tail -r $HISTORY_CACHED_FILE > $HISTORY_CACHED_FILE_ORIGINAL

  __history_search_cache() {
    fc -nrli 1 > $HISTORY_CACHED_FILE
    cat $HISTORY_CACHED_FILE | nl -s ': ' -b a > $HISTORY_CACHED_FILE_INDEXED
    cp $HISTORY_CACHED_FILE $HISTORY_CACHED_FILE_C
    mlog "cached created"
  }

  # __history_search_cache &|

  print -z $(
    ([ -n "$ZSH_NAME" ] && { cat $HISTORY_CACHED_FILE_INDEXED && __history_search_cache &| } ) | \
      fzf-tmux -p 85% --delimiter=': ' \
      --bind "ctrl-r:reload(cat $HISTORY_CACHED_FILE_INDEXED)" \
      --preview 'bat --color=always --highlight-line {1} '$HISTORY_CACHED_FILE_C --preview-window '+{1}-5' | \
      cut -c 27-
  )


  unset HISTORY_CACHED_FILE
  unset HISTORY_CACHED_FILE_INDEXED
  unset HISTORY_CACHED_FILE_ORIGINAL
}

# crontab crontab-config

