#!/bin/sh
#   FUNCTION: __fzf_git_config
__fzf_git_config(){
    # Interactive git and rewrite some helper function
    # antigen bundle 'wfxr/forgit'
    
    # load fzf default options
    FORGIT_FZF_DEFAULT_OPTS="
    $FZF_DEFAULT_OPTS
    --height='80%'
    +1
    "

    __git_pager=$(git config core.pager || echo 'cat')                           # to use diff-so-fancy

    forgit::stash::show_cust() {
      # ctrl-d for drop stash
      forgit::inside_work_tree || return 1
      local cmd opts
      cmd="echo {} |cut -d: -f1 |xargs -I% fzf_preview_git_stash % |$__git_pager"
      opts="
          $FORGIT_FZF_DEFAULT_OPTS
          +s +m -0 --tiebreak=index --bind=\"enter:execute($cmd)\"
          $FORGIT_STASH_FZF_OPTS
      "
      git stash list | FZF_DEFAULT_OPTS="$opts" fzf --preview="$cmd"
    }

    export FORGIT_NO_ALIASES=true
    # forgit_log=glo
    # forgit_diff=gd
    # forgit_add=ga
    # forgit_reset_head=grh
    # forgit_ignore=gi
    # forgit_checkout_file=gcf
    # forgit_clean=gclean
    # forgit_stash_show=gss
    # forgit_cherry_pick=gcp
    # forgit_rebase=grb
    # forgit_fixup=gfu
    # forgit_checkout_branch=gcb

  # FZF ALIAS {{{1
  # --------------------------------------------------------------------------
  alias fga='forgit::add'
  alias fgrs='forgit::restore'
  alias fgclean='forgit::clean'
  alias fgd='forgit::diff'
  # alias fgl='forgit::log'
  # alias fgss='forgit::stash::show'
  alias fgrh='forgit::reset::head'
  alias fgi='forgit::ignore'

  alias gai=forgit::add
  alias gdi=forgit::diff
  alias gstsi0=forgit::stash::show
  alias gstsi=forgit::stash::show_cust
  alias grhi=forgit::reset::head

    # }}}
    # FZF: GIT COMMIT: log, git rebase/reset/revert {{{1
  # --------------------------------------------------------------------------
    
    # glo oneline for fzf select
    _glo_one_line() {
      git log --graph --color=always --format='%C(auto)%h%d %s %C(black)%C(bold)%cr' $@
    }

    __git_commit_preview_cmd="echo {} |grep -Eo '[a-f0-9]+' | head -1 |xargs -I% fzf_preview_git_commit % |$__git_pager | LESS='-R' less"
    # __git_commit_fzf_opts=" -0" # $FZF_DEFAULT_OPTS is used by default 
    __git_commit_fzf_opts="$FZF_DEFAULT_OPTS -m -0
    --bind=\"ctrl-space:execute($__git_commit_preview_cmd)\"
    --bind=\"ctrl-y:execute-silent(echo {} |grep -Eo '[a-f0-9]+' | head -1 | tr -d '\\\n' | pbcopy)\"
    " # $FZF_DEFAULT_OPTS is used by default 

    git_log_interactive_preview(){
      _glo_one_line $@  \
        | FZF_DEFAULT_OPTS="$__git_commit_fzf_opts" fzf --preview="$__git_commit_preview_cmd"
    }
    # get git commit SHA1 short
    git_log_interactive_select(){
      _glo_one_line $@  \
        | FZF_DEFAULT_OPTS="$__git_commit_fzf_opts" fzf --preview="$__git_commit_preview_cmd" \
        | /usr/bin/grep -Eo '[a-f0-9]{7,41}' \
        | tr -d '\n' \
        | pbcopy && pbpaste

    }

    git_revert_interactive() {
      git_log_interactive_select | xargs git revert $@
    }  

    git_reset_interactive(){
      git_log_interactive_select | xargs git reset $@
    }

    git_rebase_interactive(){
      git_log_interactive_select | xargs git rebase $@
    }

    # alias glop=glopi
    alias gloi=git_log_interactive_select
    alias glo=gloi
    # get git commit SHA1 long
    alias glois='git_log_interactive_select |/usr/bin/grep -Eo "[a-f0-9]{7,41}" | xargs git show | head -1 | cut -d" " -f2'

    alias grevi=git_revert_interactive
    alias greseti=git_reset_interactive
    alias grebi=git_rebase_interactive

  # FZF: GIT FILES {{{1
  # --------------------------------------------------------------------------

    # Restore modified file
    alias grsi=forgit::checkout::file
    alias gcoi=forgit::checkout::file

    # Cleanup just untracked file 
    # git clean selector
    # alias git_clean_dry_run=
    git_clean_interactive() {

      local FZF_GIT_FILE_BIND_OPTS=" \
        --bind=\"ctrl-space:execute(bat --style=numbers --color=always --paging always {} )\"
        --bind=\"ctrl-o:execute(echo {} | cut -d ':' -f1 | xargs fzf-exec )\"
        --bind=\"ctrl-v:execute(echo {} | agnvim_open )\"
        --bind=\"ctrl-r:execute-silent(echo {} | agnvim_remote_open )\"
        --bind=\"ctrl-y:execute-silent(echo {} | cut -d ':' -f1 | xargs | tr -d '\\\n' | pbcopy )\"
        --header \"ctrl-o:fzfexec, ctrl-y:pbcopy, ctrl-r:nvim_remote, ctrl-v:nvim\"
      "

      forgit::inside_work_tree || return 1
      local files opts
      opts="
        $FORGIT_FZF_DEFAULT_OPTS
        -m -0
        $FORGIT_CLEAN_FZF_OPTS
        "
        # Note: Postfix '/' in directory path should be removed. Otherwise the directory itself will not be removed.
        files=$(git clean -dfn "$@"| sed 's/^Would remove //' | FZF_DEFAULT_OPTS="$FORGIT_FZF_DEFAULT_OPTS $FZF_COLOR_SCHEMA_BORDER $FZF_GIT_FILE_BIND_OPTS" \
          fzf --preview 'quick-preview {}' | sed 's#/$##')
        [[ -n "$files" ]] && echo "$files" | tr '\n' '\0' | xargs -0 -I% git clean -xdf '%' && return
        echo 'Nothing to clean.'
    }

    # Cleanup untracked file include ignore file
    alias gcleani0=forgit::clean

    # Cleanup all untracked file
    # gcleani -x
    alias gcleani=git_clean_interactive

    # }}}3
  # FZF: GIT BRANCH {{{1
  # --------------------------------------------------------------------------
  
    # Git loves FZF
    # My personal git preivew scripts
    
    # A, input, B parse selection and preview, C, output
    #

    __git_branch_fzf_opts="$FORGIT_FZF_DEFAULT_OPTS -m -0
    --bind=\"ctrl-y:execute-silent(echo {} | $__gb_clean_cmd_str | tr -d '\\\n' | pbcopy)\"
    --preview-window hidden
    --preview-window right:70%
    " # $FZF_DEFAULT_OPTS is used by default 
    # "echo {} |grep -Eo '[a-f0-9]+' | head -1 |xargs -I% fzf_preview_git_commit % |$__git_pager | LESS='-R' less"

    # git branch fzf: show last commit preview (take commit)
    __git_branch_commit_preview_cmd="xargs -I%% fzf_preview_git_commit %% |$__git_pager | LESS='-R' less"
    # git branch fzf: history preview (take branch name)
    __git_branch_history_preview_cmd="xargs -I$$  git log -100 --graph --color=always --format='%C(auto)%d %s %C(bold blue)<%an>%Creset %C(black)%C(bold)%cr%Creset' $$ -- | $__git_pager | LESS='-R' less"

  # fzf: git branch old way {{{1
    # Old way, can't take parameter
    # git branch fzf: show last commit preview
    # alias fzf_gb_commit_after_pipe='FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | cut -c3-1000 | $__git_branch_commit_preview_cmd" '
    # alias git_branch_interactive_preview_commit="git branch --sort=-committerdate --color=always | fzf_gb_commit_after_pipe"

    # git branch fzf: history preview
    # alias fzf_gb_history_after_pipe='FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | cut -c3-1000 | $__git_branch_history_preview_cmd" '
    # alias git_branch_interactive_preview_history="git branch --sort=-committerdate --color=always | fzf_gb_history_after_pipe"
    
  # fzf: git branch functions {{{1

    __gb_response_clean_pipe() { 
      #  Can't use alias
      # alias -g __gb_response_clean_pipe="cut -c3-1000 | cut -f1 -d' ' | tr -d '\\\n' | pbcopy && pbpaste"
      while read data; 
      # Support pipe
      do; 
        # echo $data | cut -c3-1000 | cut -f1 -d' ' | tr -d '\n' | pbcopy && pbpaste
        # echo $data | eval $__gb_clean_cmd_str | tr -d '\n' | pbcopy && pbpaste 
        # Keep \n will merges all lines
        echo $data | eval $__gb_clean_cmd_str | pbcopy && pbpaste
      done; 
    }

    git_branch_interactive(){
      gb $@ \
        | FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | $__gb_clean_cmd_str | $__git_branch_history_preview_cmd" \
        | __gb_response_clean_pipe
      # This script is depending on format from gb
    }

    git_branch_interactive_preview_commit(){
      gb $@ \
        | FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | $__gb_clean_cmd_str | $__git_branch_commit_preview_cmd" \
        | __gb_response_clean_pipe
      # This script is depending on format from gb
    }

    # git branch remote + delete + by me
    git_branch_remote_interactive_select(){
      gbr $@ \
        | FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | $__gb_clean_cmd_str | $__git_branch_history_preview_cmd" \
        | __gb_response_clean_pipe
      # This script is depending on format from gbr
    }

    # ctrl-y not working since it is using different format
    git_branch_remote_interactive_select_name(){
      local __gbr2_clean_cmd_str="cut -d ' ' -f6";             # get commit SHA-1
      local __gbr2_clean_branch_name_cmd_str="cut -d ' ' -f4"; # get branch name

      gbr2 $@ \
        | FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | $__gbr2_clean_cmd_str | $__git_branch_history_preview_cmd" \
        | eval $__gbr2_clean_branch_name_cmd_str \
        | sed 's/origin\///' \
        | tr -d '\n' \
        | pbcopy && pbpaste
      # This script is depending on format from gbr2
    }

  # fzf: git branch  }}}1

    # Sam as function but function can take parameter
    alias gbi=git_branch_interactive
    alias gbic=git_branch_interactive_preview_commit    

    # If gbri is working, will deprecate dbri2 later
    alias gbri=git_branch_remote_interactive_select
    alias gbri2=git_branch_remote_interactive_select_name


    #======================== DELETE BRANCH ===============================
    # Delete branches local
    git_branch_delete_interactive(){ git_branch_interactive $@ | xargs -n 1 git branch -d }
    git_branch_delete_interactive_D(){ git_branch_interactive $@ | xargs -n 1 git branch -D }

    alias gbdi=git_branch_delete_interactive
    alias gbDi=git_branch_delete_interactive_D


    # Delete branches remote
    git_branch_remote_delete_interactive(){ git_branch_remote_interactive_select $@ | xargs -n 1 git push -d origin }
    alias gbrdi=git_branch_remote_delete_interactive

    # duplicate with gbrdi, choose one
    # alias gbd_remote='ee "git push -d origin"'
    # alias gbrdi='gbd_remote $(gbri)'

    #===== Branch clean up (House Clean) ======
    #
    # Removed remote merged branch 
    # alias gb_merged_remote="git for-each-ref --merged HEAD --sort=-committerdate refs/remotes/ --format='(%(color:green)%(committerdate:relative)%(color:reset)) %(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(color:red)%(objectname:short)%(color:reset) - %(authorname)' --color=always"
    # alias gb_merged_remote_me='ee "gb_merged_remote |grep chuan"'
    # alias gb-merged='git branch --merged'
    # Replaced by
    # gbdri --merged
    #
    #
    # Clean merged Branch (Delete local branches which are already merged)
    # alias gbd-merged-branch-local='git branch --merged | grep -v "\*" | xargs -n 1 git branch -d'
    # alias git-house-clean="echo gbd-merged-branch-local \ngb_merged_remote_me\n"
    # gbdi --merged


    #======================== SWITCH BRANCH ===============================
    alias gcobi='git_branch_interactive | xargs git checkout'
    # Git branch checkout remote
    alias gcobri="git_branch_remote_interactive_select | sed 's/origin\///' | xargs git checkout"
    # Same as gcobri
    alias gcobri2="git_branch_remote_interactive_select_name | xargs git checkout"


    # gbr show merged branched
    # Cleanup
    # Can be replaced with gbi --merged
    # alias gbri_merged="gb_merged_remote | fzf | cut -d ' ' -f6 | cut -c8-1000"
    # alias gbri_me="gbr | grep chuan | fzf | cut -d ' ' -f6 | cut -c8-1000"

  # fzf: git branch? }}}3

  # Git stash
    FORGIT_STASH_FZF_OPTS='
    --bind="ctrl-d:reload(git stash drop $(cut -d: -f1 <<<{}) 1>/dev/null && git stash list)"
    '
    __git_stash_preview() {
      git stash show --color=always --ext-diff $@
    }


}
__fzf_git_config
