#!/bin/sh

# Git loves FZF
# My personal git preivew scripts

# A, input   | B parse selection and preview | C, output action
# GIT commit | fzf                           | reset/checkout/rebase
# GIT branch | fzf                           | checkout/delete
# GIT file   | fzf                           | checkout/delete
# GIT stash  | fzf                           | drop/checkout/preview

# FZF forgit basic config {{{1
# --------------------------------------------------------------------------
{
    # Interactive git and rewrite some helper function
    # antigen bundle 'wfxr/forgit'

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
    # load fzf default options

    FORGIT_FZF_DEFAULT_OPTS=" $FZF_DEFAULT_OPTS --height='80%' +1 "
    FZF_GIT_DEFAULT_OPTS=" $FZF_DEFAULT_OPTS --height='80%' +1 "

    __git_pager=$(git config core.pager || echo 'cat')                           # to use diff-so-fancy

}

 # FZF: GIT COMMIT: log, git rebase/reset/revert {{{1
# --------------------------------------------------------------------------
  {
    # After I updates git_commit_preview, it can also preview its parenets
    __git_commit_preview_cmd="echo {} | sha1grep | xargs -I% git_commit_preview % |$__git_pager | LESS='-R' less"
    __git_commit_fzf_opts="$FZF_DEFAULT_OPTS -m +s -0
    --bind=\"ctrl-space:execute($__git_commit_preview_cmd)\"
    --bind=\"ctrl-y:execute-silent(echo {} | sha1grep | head -1 | tr-newline | pbcopy)\"
    " # $FZF_DEFAULT_OPTS is used by default

    fzf_git_commit_preview(){
      FZF_DEFAULT_OPTS="$__git_commit_fzf_opts" fzf_tp --preview="$__git_commit_preview_cmd" $@
    }

    # Usage: gloo | fgcp
    # Usage: glog | fgcp
    alias fgcp=fzf_git_commit_preview

    # gloo is an alias but we should not use alias in function
    # https://stackoverflow.com/questions/45601589/zsh-not-recognizing-alias-from-within-function
    # It works only they are sourced in different stage

    # get git commit SHA1 short
    git_log_interactive_select(){
      # glog $@  \
      #   | fzf_git_commit_preview \
      #   | sha1grep \
      #   | pbcopy && pbpaste

      # git log preview with filtered file/directory
      local __git_commit_preview_cmd2="echo {} | sha1grep | xargs -I% git_commit_preview % $@ |$__git_pager | LESS='-R' less"
      glog $@  | FZF_DEFAULT_OPTS="$__git_commit_fzf_opts" fzf_tp --preview="$__git_commit_preview_cmd2" \
        | sha1grep \
        | pbcopy && pbpaste
    }


    git_revert_interactive() { git_log_interactive_select | xargs git revert $@ }

    git_reset_interactive(){ git_log_interactive_select | xargs git reset $@ }

    git_rebase_interactive(){ git_log_interactive_select | xargs git rebase $@ }

    git_gerritopen_interactive(){
      forgit::inside_work_tree || return 1
      # if [ $(git rev-parse --is-inside-work-tree) = false ]; then
      #   echo "Not in git repo"
      #   return
      # fi
      change_id="$(git_log_interactive_select | sha1grep | xargs git show --no-patch | grep Change-Id | awk '{print $2}')"
      server="$(git config --get remote.origin.url | grep -o 'gerrit.\w*.\w*')"
      ee "open 'https://$server/#/q/$change_id'"
    }

    # alias glop=glopi
    alias gloi=git_log_interactive_select
    alias glo=gloi
    alias gloigo=git_gerritopen_interactive
    # get git commit SHA1 long
    alias gloil='git_log_interactive_select | sha1grep | xargs git show | head -1 | sha1grep'

    alias grevi=git_revert_interactive
    alias greseti=git_reset_interactive
    alias grebi=git_rebase_interactive
  }
  # FZF: GIT FILES {{{1
  # --------------------------------------------------------------------------
  {

    # Restore modified file
    alias grsi=forgit::checkout::file
    alias gcoi=forgit::checkout::file

    # Cleanup just untracked file
    # git clean selector
    # alias git_clean_dry_run=
    git_clean_interactive() {
      forgit::inside_work_tree || return 1

      local FZF_GIT_FILE_BIND_OPTS=" \
        --bind=\"ctrl-space:execute(bat --style=numbers --color=always --paging always {} )\"
        --bind=\"ctrl-o:execute(echo {} | cut -d ':' -f1 | xargs fzf-exec )\"
        --bind=\"ctrl-v:execute(echo {} | agnvim_open )\"
        --bind=\"ctrl-r:execute-silent(echo {} | agnvim_remote_open )\"
        --bind=\"ctrl-y:execute-silent(echo {} | cut -d ':' -f1 | xargs | tr-newline | pbcopy )\"
        --header \"ctrl-o:fzfexec, ctrl-y:pbcopy, ctrl-r:nvim_remote, ctrl-v:nvim\"
      "

      forgit::inside_work_tree || return 1
      local files opts
      opts="
        $FZF_GIT_DEFAULT_OPTS
        -m -0
        $FORGIT_CLEAN_FZF_OPTS
        "
        # Note: Postfix '/' in directory path should be removed. Otherwise the directory itself will not be removed.
        files=$(git clean -dfn "$@"| sed 's/^Would remove //' | FZF_DEFAULT_OPTS="$FZF_GIT_DEFAULT_OPTS $FZF_GIT_FILE_BIND_OPTS" \
          fzf_tp --preview 'quick-preview {}' | sed 's#/$##')
        [[ -n "$files" ]] && echo "$files" | tr '\n' '\0' | xargs -0 -I% git clean -xdf '%' && return
        echo 'Nothing to clean.'
    }

    gsti() {
      # this can be used in git checkout/add/reset or diff?
      forgit::inside_work_tree || return 1
      # Add files if passed as arguments
      [[ $# -ne 0 ]] && git add "$@" && git status -su && return

      local changed unmerged untracked files opts preview extract
      changed=$(git config --get-color color.status.changed red)
      unmerged=$(git config --get-color color.status.unmerged red)
      untracked=$(git config --get-color color.status.untracked red)
      # NOTE: paths listed by 'git status -su' mixed with quoted and unquoted style
      # remove indicators | remove original path for rename case | remove surrounding quotes
      extract="
          sed 's/^.*]  //' |
          sed 's/.* -> //' |
          sed -e 's/^\\\"//' -e 's/\\\"\$//'"
      preview="
          file=\$(echo {} | $extract)
          if (git status -s -- \$file | grep '^??') &>/dev/null; then  # diff with /dev/null for untracked files
              git diff --color=always --no-index -- /dev/null \$file | $forgit_diff_pager | sed '2 s/added:/untracked:/'
          else
              git diff --color=always -- \$file | $forgit_diff_pager
          fi"
      opts="
          $FZF_GIT_DEFAULT_OPTS
          -0 -m --nth 2..,..
          $FORGIT_ADD_FZF_OPTS
      "
      git -c color.status=always -c status.relativePaths=true status -su |
          grep -F -e "$changed" -e "$unmerged" -e "$untracked" |
          sed -E 's/^(..[^[:space:]]*)[[:space:]]+(.*)$/[\1]  \2/' |
          FZF_DEFAULT_OPTS="$opts" fzf_tp --preview="$preview" |
          sh -c "$extract"
    }


    # Cleanup untracked file include ignore file
    alias gcleani0=forgit::clean

    # Cleanup all untracked file
    # gcleani -x
    alias gcleani=git_clean_interactive

  }
  # FZF: GIT stash {{{1
  # --------------------------------------------------------------------------
  {
    FORGIT_STASH_FZF_OPTS='
    --bind="ctrl-d:reload(git stash drop $(cut -d: -f1 <<<{}) 1>/dev/null && git stash list)"
    '

    git_stash_list_interactive() {
      # ctrl-d for drop stash
      forgit::inside_work_tree || return 1
      local cmd opts
      cmd="echo {} |cut -d: -f1 |xargs -I% git_stash_preview % |$__git_pager"
      opts="
          $FZF_GIT_DEFAULT_OPTS
          +s +m -0
          $FORGIT_STASH_FZF_OPTS
      "
      git stash list | FZF_DEFAULT_OPTS="$opts" fzf_tp --preview="$cmd" | cut -d: -f1
    }

  }
  # FZF: GIT BRANCH {{{1
  # --------------------------------------------------------------------------
  {

    # branch FZF config {{{2
    __git_branch_fzf_opts="$FZF_GIT_DEFAULT_OPTS -m -0 +s
    --bind='ctrl-y:execute-silent(echo {} | git_branch_grep | tr-newline | pbcopy)'
    --preview-window right:70%
    --preview-window hidden
    "


  # fzf: git branch preview functions {{{2
    __git_branch_history_preview_cmd="xargs -I$$ git log -50 --stat --graph --color=always --format='$_glog_auth_format' $$"
    fzf_git_branch_to_history_preview(){
      FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | git_branch_grep | $__git_branch_history_preview_cmd"  "$@" \
        | git_branch_grep
    }

    fzf_git_branch_to_commit_preview(){
      FZF_DEFAULT_OPTS="$__git_branch_fzf_opts" fzf --preview="echo {} | git_branch_grep | xargs -I% git_commit_preview %"  "$@" \
        | git_branch_grep | tr-newline | pbcopy && pbpaste
    }
    alias fgbp=fzf_git_branch_to_history_preview
    alias fgbcp=fzf_git_branch_to_commit_preview


  # 2}}}
  # fzf: git branch alias {{{2

    gbi(){ gb $@ | fzf_git_branch_to_history_preview }
    gbic(){ gb $@ | fzf_git_branch_to_commit_preview }


    # gbri2 is better when branch name is too long
    gbri(){ gbr $@ | fzf_git_branch_to_history_preview }
    gbri2(){ gbr2 $@ | fzf_git_branch_to_history_preview \
        | sed 's/origin\///' \
        | pbcopy && pbpaste }


    #======================== DELETE BRANCH ===============================
    # Delete branches local
    git_branch_delete_interactive(){ gbi $@ | xargs -n 1 git branch -d }
    git_branch_delete_interactive_D(){ gbi $@ | xargs -n 1 git branch -D }

    alias gbdi=git_branch_delete_interactive
    alias gbDi=git_branch_delete_interactive_D


    # Delete branches remote
    # xargs will run command per line
    # https://unix.stackexchange.com/questions/7558/execute-a-command-once-per-line-of-piped-input
    # printf "foo bar\nbaz bat" | xargs -n1 printf "message %s\n"
    # Now gbrdi works for delete multiple branch at same time
    git_branch_remote_delete_interactive(){ echo "git push -d origin" && gbri $@ | sed 's/origin\///' | xargs -n1 git push -d origin }
    alias gbrDi=git_branch_remote_delete_interactive

    # duplicate with gbrdi, choose one
    # alias gbd_remote='ee "git push -d origin"'
    # alias gbrdi='gbd_remote $(gbri)'


    #======================== SWITCH BRANCH ===============================
    alias gcobi='gbi | xargs git checkout'
    # Git branch checkout remote
    alias gcobri="gbri | sed 's/origin\///' | xargs git checkout"
    # Same as gcobri
    alias gcobri2="gbri2 | xargs git checkout"

    #===== Branch clean up (House Clean) ======

    # Removed remote merged branch
    # gbrdi --merged

    # Clean local merged Branch (Delete local branches which are already merged)
    # gbdi --merged

    # 2}}}
  }

  # 1}}}

# FZF: ALIAS {{{1
# --------------------------------------------------------------------------
{
  alias fgd='forgit::diff'
  alias fgrh='forgit::reset::head'
  alias fgi='forgit::ignore'

  alias gai=forgit::add
  alias gdi=forgit::diff
  alias gstsi=git_stash_list_interactive
  alias gstli=git_stash_list_interactive
  alias gstaai='git_stash_list_interactive | xargs git stash apply $@'
  alias grhi=forgit::reset::head
}

# 1}}}
# FZF: Git Review {{{1
# --------------------------------------------------------------------------
{
  # grvw() { git review $@ }
  # grvl() { runcached --bg-update --ignore-env --ttl 1800  git review -l --color=always $@ }
  grvl() { RUNCACHED_IGNORE_PWD=0 runcached --bg-update --ttl 1800 git review -l --color=always $@ }
  grvli() { grvl $@ | fzf --header-lines=1 | awk '{print $1}' }
  gcorvi() { grvl $@ | fzf --header-lines=1 | awk '{print $1}' | xargs git review -d }
}

# 1}}}


# Git: Subfolders Run git command against a list of folders {{{1
# --------------------------------------------------------------------------
{
  __git_folders() {
    # Find git repos under current director
    for i in */.git; do ( echo $i ); done
  }

  __git_foldersi() {
    __git_folders | sed 's/\/.git//' | fzf --preview "git_folder_preview {}"
  }

  git_folders_pull() {
    __git_foldersi | xargs -I% git_pull_at_directory %
  }

  git_folders_clean() {
    __git_foldersi | xargs -I % sh -c "echo 'path: %' && git -C % clean -d -f $@"
  }

  git_folders_run() {
    folders=$(__git_foldersi)
    read CMD\?"cmd: "
    echo $folders | xargs -I % sh -c "echo 'path: %' && git -C % $CMD"
  }

  alias gsp=git_folders_pull
  alias gsc=git_folders_clean
  alias gsr=git_folders_run

}
# 1}}}
