#!/bin/sh
#
#   DEVTOOL: __ls_fuzzy_preview
# -------------------------------------------------------
# TODO move to functions?

# fd_search_cur_dir{{{4
export FD_SEARCH_CUR_DIR_DEPTH=1
__fd_search_cur_dir_dir() {
  fd -d $FD_SEARCH_CUR_DIR_DEPTH --hidden --no-ignore-vcs --color=always --type file
}

__fd_search_cur_dir_file() {
  fd -d $FD_SEARCH_CUR_DIR_DEPTH --hidden --no-ignore-vcs --color=always --type directory
}

fd_search_cur_dir() {
  # fd -d $FD_SEARCH_CUR_DIR_DEPTH --hidden --no-ignore-vcs --color=always
  { __fd_search_cur_dir_file && __fd_search_cur_dir_dir}
}
# }}}4
# short_pwd{{{4
# Short pwd for ls_fuzzy_preview
short_pwd() {
  local pwd_str="$(pwd)"
  local SHORT_PWD_LENGTH=38
  if [[ ${#pwd_str} -gt $SHORT_PWD_LENGTH ]]; then
    echo $pwd_str | awk -v len=${SHORT_PWD_LENGTH} '{print "..."substr($0,length($0)-len,len+1)"/"}'
  else
    echo "${pwd_str}/"
  fi
}
# ls_fuzzy_preview {{{4
# Notes: we can mix use of bind and while loop key 
# Can't used execute and while loop key together
ls_fuzzy_preview() {

  local searchTerm=""
  local FZF_FUZZY_BIND_OPTS=" 
    --bind=\"ctrl-r:execute-silent(echo {} | agnvim_remote_open )\"
    --bind=\"ctrl-y:execute-silent(echo {} | pbcopy )\"
  "
  while out=$( fd_search_cur_dir | FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS $FZF_COLOR_SCHEMA_BORDER $FZF_FUZZY_BIND_OPTS" fzf-tmux -0 \
    --preview "quick-preview {}" --exit-0 \
    --expect=ctrl-v,ctrl-e,ctrl-space,enter,alt-left,alt-right,alt-up,alt-down,shift-left,shift-right \
    --print-query --header "[${FD_SEARCH_CUR_DIR_DEPTH}]:$(short_pwd)" \
    --preview-window right:50% --height ${FZF_TMUX_HEIGHT:-100%} \
    -q "$searchTerm" \
    );
  do
    searchTerm=$(head -1 <<< "$out"| tail -1)
    key=$(head -2 <<< "$out"| tail -1)
    input=$(echo "$out" | tail +3 | tr '\n' ' ')

    #echo $out
    #echo "input: " $input
    #echo "key: " $key
    #echo "searchT: " $searchTerm

    if [[ "$key" == 'alt-left' ]]; then
      pushd +0 1>/dev/null;
    elif [[ "$key" == 'alt-right' ]] || [[ "$key" == 'alt-down' ]]; then
      pushd -1 1>/dev/null;
    elif [[ "$key" == 'alt-up' ]]; then
      cd ../
    elif [[ "$key" == 'shift-right' ]]; then
      FD_SEARCH_CUR_DIR_DEPTH=$(expr $FD_SEARCH_CUR_DIR_DEPTH + 1)
    elif [[ "$key" == 'shift-left' ]]; then
      local new_dept=$(expr $FD_SEARCH_CUR_DIR_DEPTH - 1)
      if [[ "$new_dept" -ge "1" ]];then
        FD_SEARCH_CUR_DIR_DEPTH=$new_dept
      fi

    elif [[ "$key" == 'ctrl-v' ]]; then
      nvim $input
    elif [[ "$key" == 'ctrl-o' ]]; then
      fzf-exec $input
      break;

    elif [[ "$key" == 'ctrl-space' ]] || ; then
      if [[ -d "${input}" ]] ; then
        cd "${input}"
      elif [[ -f "${input}" ]]; then
        bat --color always --paging always --style full $input
        # bat --color always --pager always $input | LESS='-R' less
      fi
    elif [[ "$key" == 'enter' ]]; then
      echo $input
      break;
    fi
  done
}

    alias lf=ls_fuzzy_preview
