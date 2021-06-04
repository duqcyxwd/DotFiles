#!/bin/sh

fag() {
  if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi

  local FZF_FAG_BIND_OPTS=" \
    --bind=\"ctrl-space:execute(bat --style=numbers --color=always --paging always --highlight-line {2} {1} | LESS='-R +{2}' less)\"
    --bind=\"ctrl-o:execute(echo {} | cut -d ':' -f1 | xargs fzf-exec )\"
    --bind=\"ctrl-v:execute(echo {} | agnvim_open )\"
    --bind=\"ctrl-r:execute-silent(echo {} | agnvim_remote_open )\"
    --bind=\"ctrl-y:execute-silent(echo {} | cut -d ':' -f1 | xargs | tr -d '\\\n' | pbcopy )\"
    --header \"ctrl-o:fzfexec, ctrl-y:pbcopy, ctrl-r:nvim_remote, ctrl-v:nvim\"
  "
  # -0 exit when no match
  # -1 Automatically select the only match
  #
  ag --hidden --nobreak --noheading --color $@ | FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS $FZF_COLOR_SCHEMA_BORDER $FZF_FAG_BIND_OPTS"  \
    fzf-tmux -p 85% -0 --delimiter : \
    --preview 'bat --style=numbers --color=always --highlight-line {2} {1}' \
    --preview-window +{2} \
    --preview-window border-none
    # Not working with :: in zinit --delimiter
    # | cut -d ':' -f1 | xargs fzf-exec   # This is not working with ctrl-v, anything command launches other script
}

