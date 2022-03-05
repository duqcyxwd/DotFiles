#!/bin/sh

# SECTION: : Autocompletion
# --------------------------------------------------------------------------
# Autocompletion for teamocil

_comp_dir_repodir="${HOME}/code/"
co() { cd $_comp_dir_repodir$1 }

_comp_dir_gh_dir="${HOME}/github/"
gh() { cd $_comp_dir_gh_dir$1 }

_comp_dir_duqcyxwd_dir="${HOME}/duqcyxwd/"
d() { cd $_comp_dir_duqcyxwd_dir$1 }


# https://github.com/zsh-users/zsh-completions/blob/master/zsh-completions-howto.org
_code_directory_completion() { #{{{
  # _alternative "dirs:user directory:($(/bin/ls $_comp_dir_repodir/))"
  # _alternative "dirs:user directory:($(/usr/local/bin/fd -d 3 -t d '' $_comp_dir_repodir))"
  # local files=($(/usr/local/bin/fd -d 1 -t d '' $_comp_dir_repodir | cut -c${#_comp_dir_repodir}- | cut -c2- ))
  local files=($(/usr/local/bin/fd -d 1 -t d --base-directory=$_comp_dir_repodir ))
  compadd -a -f files
}
# }}}

_gh_directory_completion() {
  _alternative \
    "dirs:user directory:($(/bin/ls $_comp_dir_gh_dir/))"
}

_duqcyxwd_directory_completion() {
  # Method 1
  # _alternative  "dirs:user directory:($(/usr/local/bin/fd -d 3 -t d '' $_comp_dir_duqcyxwd_dir | cut -c${#_comp_dir_duqcyxwd_dir}- | cut -c2- ))"

  # Method2
  local files=($(/usr/local/bin/fd -d 3 -t d --base-directory=$_comp_dir_duqcyxwd_dir ))
  compadd -a -f files
}

compdef _code_directory_completion co
compdef _gh_directory_completion gh
compdef _duqcyxwd_directory_completion d
