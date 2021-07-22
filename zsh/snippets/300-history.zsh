# ZSH HISTORY

# SECTION: : ZSH History
# --------------------------------------------------------------------------
# ZSH History
# https://unix.stackexchange.com/questions/273861/unlimited-history-in-zsh
# http://zsh.sourceforge.net/Doc/Release/Parameters.html#index-HISTSIZE
# http://zsh.sourceforge.net/Guide/zshguide02.html#l18 History option

export HIST_STAMPS="yyyy-mm-dd" # ZSH History time format
export HISTSIZE=990000          #The maximum number of events stored in the internal history list.
export SAVEHIST=990000          #The maximum number of history events to save in the history file.

# Need to update file /ect/zshrc
# Need to update file OMZ::lib/history.zsh/history.zsh

setopt EXTENDED_HISTORY       # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY     # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY          # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS       # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS   # Delete old recorded entry if new entry is a duplicate.
setopt HIST_IGNORE_SPACE      # Don't record an entry starting with a space.
setopt HIST_FIND_NO_DUPS      # Do not display a line previously found.
setopt HIST_SAVE_NO_DUPS      # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS     # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY            # Don't execute immediately upon history expansion.
setopt HIST_NO_FUNCTIONS      # Don't store function

setopt AUTO_CD                # ZSH AUTO CD into directories

# Notes
# https://scarff.id.au/blog/2019/zsh-history-conditional-on-command-success/
zshaddhistory() { # {{{1
  emulate -L zsh

  # return 1 when first command is unknown command
  whence ${${(z)1}[1]} >| /dev/null || return 1

  local line=${1%%$'\n'}
  local cmd=${line%% *}

  # mlog "zshaddhistory: "
  # mlog ${${(z)1}[1]}
  # mlog ${(z)1}
  # mlog $@
  # mlog $line
  # mlog $cmd
  # mlog $(pwd)
  # his="$(date +%s): $(pwd): $@"
  his="$(date +%s): $(pwd): ${(z)1}"
  echo $his >> ~/.zsh_history_bk/.zsh_hist_cust_bk

  # Modified from https://mollifier.hatenablog.com/entry/20090
  # Not necessary, Most of them will be cleanup by zsh
  if [[ ${#line} -ge 4
          && ${cmd} != ("")
          && ${cmd} != (gc)
          && ${cmd} != (l|l[salt]|l[salt][a])
          && ${cmd} != (m|man) ]] ; then
    # Following code will cache command to avoid duplicate entry
    # Stop writing same command within a limited time period
    print -sr -- "${1%%$'\n'}"
    fc -p
  else
    return 1
  fi

  # Notes
  # https://superuser.com/questions/352788/how-to-prevent-a-command-in-the-zshell-from-being-saved-into-history
}


function fh() { # {{{1
  # WIP
  # print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -E 's/ *[0-9]*\*? *//' | sed -E 's/\\/\\\\/g')

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

# }}}


alias history_count="cat $ZDOTDIR/.zsh_history| wc -l"
dis_zsh_history() { local HISTSIZE=0; }
# fc -p "$HISTFILE" This will chagne history file

alias hist-c='zsh-history-bk && vi $ZDOTDIR/.zsh_history'

uuu() {
  echo "Clean last command from history"
  ZSH_HISTORY_TEMP="zsh_history_temp"
  ghead -n -2 $HISTFILE >$ZSH_HISTORY_TEMP
  cp $ZSH_HISTORY_TEMP $HISTFILE
  rm $ZSH_HISTORY_TEMP
}

# Modify last command
# https://superuser.com/questions/902241/how-to-make-zsh-not-store-failed-command
# fc -e nano -1
# edit the entire history
# fc -W; nano "$HISTFILE"; fc -R

function bla() { return 1 }
function bla2() { return 0 }


