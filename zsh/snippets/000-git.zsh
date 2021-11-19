
# Git config {{{2
git config --global color.ui true
git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.ci commit
git config --global alias.st status
git config --global alias.a add
git config --global alias.root "rev-parse --show-toplevel"

git config --global core.excludesfile $XDG_CONFIG_HOME/gitglobalignore

# Configure git as my personal repo
alias config-git-local="ee \"git config --local user.name 'Yongqinchuan Du' && git config --local user.email 'duqcyxwd@gmail.com'\""


# Git functions {{{2
# --------------------------------------------------------------------------
#=== Special Git Tool  ====

# Three git functions from ohmzsh, I don't want load whole lib becase it took too
# long
# https://github.com/ohmyzsh/ohmyzsh/blob/master/lib/git.zsh

# We wrap in a local function instead of exporting the variable directly in
# order to avoid interfering with manually-run git commands by the user.
function __git_prompt_git() {
  GIT_OPTIONAL_LOCKS=0 command git "$@"
}

function git_repo_name() {
  local repo_path
  if repo_path="$(__git_prompt_git rev-parse --show-toplevel 2>/dev/null)" && [[ -n "$repo_path" ]]; then
    echo ${repo_path:t}
  fi
}

# Outputs the name of the current branch
# Usage example: git pull origin $(git_current_branch)
# Using '--quiet' with 'symbolic-ref' will not cause a fatal error (128) if
# it's not a symbolic ref, but in a Git repo.
function git_current_branch() {
  local ref
  ref=$(__git_prompt_git symbolic-ref --quiet HEAD 2> /dev/null)
  local ret=$?
  if [[ $ret != 0 ]]; then
    [[ $ret == 128 ]] && return  # no git repo.
    ref=$(__git_prompt_git rev-parse --short HEAD 2> /dev/null) || return
  fi
  echo ${ref#refs/heads/}
}

get_git_current_branch() { git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'; }

# gb --show-current
get_current_branch() { git rev-parse --abbrev-ref HEAD }

git_create_branch() {
    if [ -z "$1" ]; then
        echo "*******************************************"
        echo "*   !!! WARNING !!!  Branch not created   *"
        echo "*******************************************"
        echo ""
        echo "Silly me!!! I need to specify a parameter, the branch.."
    else
        current_branch=$(get_git_current_branch)
        set -x
        git checkout -b $1
        if [ $? -eq 0 ]; then
            git push --set-upstream origin $1
            if [ $? -ne 0 ]; then
                git checkout ${current_branch}
                git branch -D $1
            fi
        fi
        set +x
    fi
}

gitopen_current_branch() {
    ee "gitopen -b $(get_current_branch)"
}

git_blame() {
    ruby ~/repo/DotFiles/otherTool/git-blame-colored $1 | less -R
}


# Delete local and remote branch
git-branch-delete-remote-current-branch() {
  if [ $# -eq 0 ]; then
    echo "Require branch"
    return
  fi
  local branch="$(get_current_branch)"
  git checkout -
  ee "git branch -d $branch"
  ee "git push -d $branch"
}


# Unalias git log {{{2
# --------------------------------------------------------------------------
# Unalias from OMZ::plugins/git/git.plugin.zsh
unalias -m glg
unalias -m glgp
unalias -m glgg
unalias -m glgga
unalias -m glgm
unalias -m glo
unalias -m glol
unalias -m glols
unalias -m glod
unalias -m glods
unalias -m glola
unalias -m glog
unalias -m gloga
unalias -m glp
unalias -m gb
unalias -m gbr

# Git Alias {{{2
# --------------------------------------------------------------------------
__git_alias() {


# Git Colors {{{3
# --------------------------------------------------------------------------
# git log pretty format
# https://git-scm.com/docs/git-log#_pretty_formats

# git branch format
# https://git-scm.com/docs/git-for-each-ref#Documentation/git-for-each-ref.txt---formatltformatgt

# A list of colr
# I do not have an old version of git to verify that the colors other than red, blue and green are supported.

# Although, one thing I noticed even with the recent versions of git (like 1.7.10 I used) is that colors other than red, green and blue need to be within parentheses (). For red, green and blue, the parentheses are optional.

# So give this a try:

# git log --pretty=format:"normal %C(black)black%Creset %Credred%Creset %Cgreengreen%Creset %C(Yellow)yellow%Creset %Cblueblue%Creset %C(magenta)magenta%Creset %C(cyan)cyan%Creset %C(white)white%Creset" --color=always | head -1
# The list of colors I'm aware of at least are:

# normal
# black
# red
# green
# yellow
# blue
# magenta
# cyan
# white

# Better Git branch

_gb_format="%(HEAD) %(align:65,left)%(color:green)%(refname:short)%(color:reset)%(end) - %(align:19,left)%(color:blue)%(authorname)%(color:reset)%(end) %(align:21,right)%(color:green italic)%(committerdate:relative)%(color:reset)%(end) %(align:18,left)%(color:white italic)%(objectname:short)%(color:reset)%(end)"
gb() { git branch --format="$_gb_format" --sort=-committerdate --color=always $@ }

# alias gbr='git branch --remote'
_gbr_format=" %(align:0,left)%(color:yellow)%(refname:short)%(color:reset)%(end) - %(align:0,left)%(color:blue)%(authorname)%(color:reset)%(end) - %(align:0,right)(%(color:green italic)%(committerdate:relative)%(color:reset))%(end) "
alias gbr2='git for-each-ref --sort=-committerdate refs/remotes/ --format="$_gbr_format" --color=always'


# alias gbr="git for-each-ref --sort=-committerdate refs/remotes/ --format=\"$_gb_format\" --color=always"
gbr() { git for-each-ref --sort=-committerdate refs/remotes/ --format="$_gb_format" --color=always $@ }

# Git Others Alias {{{3
# --------------------------------------------------------------------------
#
alias gopen=gitopen
# Non interactive git log
alias glos='git log --stat'


# git log one line
_glog_format="%C(green bold)%h%Creset %C(auto)%d%Creset %C(normal)%s$Creset %C(cyan)%cr"
alias gloo='git log --graph --color=always --format="$_glog_format"'

# git log with author
_glog_auth_format="%C(green bold)%h%Creset -%C(auto)%d%Creset %s %C(bold blue)<%an>%Creset %C(cyan)%C(bold)%cr%Creset"
alias glog="git log --graph --pretty='$_glog_auth_format'"

# git log graph simple graph with stat
alias glogs="git log --stat --graph --pretty='%C(green bold)%h%Creset -%C(auto)%d%Creset %s %C(bold blue)<%an>%Creset %C(cyan)%C(bold)%cr%Creset'"

alias gke='\gitk --all $(git log -g --pretty=%h)'

# --follow Continue listing the history of a file beyond renames (works only for a single file).
alias glof='git log --follow -p --'


# git pull --rebase is shorthand for a fetch and a rebase!
# Use git rebase instead of git merge
# https://learngitbranching.js.org/
# alias gl="git pull rebase" Not working for develop branch
alias gl="echo 'use gupv for feature branch update' && git pull --ff-only"

alias git-tag-tips="echo ' git tag v1.0.0 \n git tag -a v1.2 9fceb02 \n git push origin v1.5 \n git push origin --tags'"
alias git-hidden="git ls-files -v | grep '^[a-z]' | cut -c3-"
alias git-hide='ee "git update-index --assume-unchanged"'
alias git-unhide-all='ee "git update-index --really-refresh"'
alias git-update-all='find . -type d -depth 1 -exec git --git-dir={}/.git --work-tree=$PWD/{} pull origin master \;'


alias gbcopy="echo 'Copy current branch name' && git rev-parse --abbrev-ref HEAD |pbcopy && git branch"

alias gbc="git create-branch"

# Create cust gco for cust completion
git_checkout_branch_cust() { git checkout $@ }
alias gcob=git_checkout_branch_cust


alias gbrr="ee 'gbr |grep r/'"                                      # ggsup or gpsup  git create-branch -r development


# Others
alias gcam='git commit --amend'

alias gre="ee 'git recent | head'"
alias grec="ee 'git recent | grep -i chuan | grep -v gone'"
alias gstau='git stash -u'         # git stash include untracked
alias gru="ee 'git remote update origin --prune'"

#======================= Git Alias for work  =========================================

# alias gbui="echo 'git branch update with integration' && git fetch -p && git merge origin/integration"
alias gbud="echo 'git branch update with develop' && git pull origin develop"

#  --ff-only is same to use
alias gcod="ee 'git checkout develop && git merge origin/develop --ff-only'"

# The following way will remove my unstashed change
# alias gcod="git checkout develop && git reset origin/develop --hard"

alias gf='git fetch --prune'
alias gfco='git fetch -p && git checkout'
alias gitf='open -a GitFiend --args $(git rev-parse --show-toplevel)'

# run gitk
alias gk="ee 'gitk --all&'"

# git diff
git_pager=$(git config core.pager || echo 'cat')
alias gdd="{git diff --stat --color origin/develop.. && git diff --color origin/develop.. } | ${git_pager}"
alias gitxdd="git diff origin/develop.. | gitx"
alias gds="ee 'git diff -w --stat'"

}

# Delete all branchs excep current branch
# alias gbdelete-all='gb | grep "f/CD" | grep -v "\*" |xargs -n 1 git branch -D'

# }}}}


# git clean up
#clean all but the stuff the stuff that we would like preserved like .ccache, xmls catalog etc
#clean -dxf will wipe everything requiring user to source gitenv again
#alias gclean='pushd $MY_GIT_TOP > /dev/null && git submodule foreach --recursive 'git clean -xdf' && git clean -xdf -e .ccache -e .flex_dbg -e remap_catalog.xml && popd > /dev/null'

__git_alias
