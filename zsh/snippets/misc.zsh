#!/bin/sh

alias signs="open https://github.com/romkatv/powerlevel10k#what-do-different-symbols-in-git-status-mean"


# DEVTOOL: : bat {{{1
{
    if [ $commands[bat] ]; then
      export MANPAGER="sh -c 'col -bx | bat -l man -p --color=always'"
    fi
}
# DEVTOOL: : exa {{{1
{
    if [ $commands[exa] ]; then
      alias exa='/usr/local/bin/exa --time-style=long-iso --group-directories-first -F --icons --color=always'
      alias e=exa
      alias ea='exa -a'
      alias eaa='exa .?* -d'

      alias ls='e'

      # g git, a, all
      alias l='exa -lhbF'                                                # list, size, type
      alias ld='exa -lhbFD'                                              # list, size, type
      alias lg='l --git'                                                 # list, size, type, git
      alias lss='l -s ext'                                               # list, size, type

      alias la='exa -lbFa'                     
      alias laa='la .?* -d'                                              # Show hidden files only
      alias lag=la --git

      alias ll='l -G'                                                    # long list
      alias lls='ll -s ext'                                              # long list sort
      alias llg='ll --git'                                               # long list

      alias lla='ll -a'                                                  # long list
      alias llag='lla --git'                                             # long list
      alias llaa='lla .?* -d'                                            # long list

      alias lx='exa -lbhHigUmuSa --time-style=long-iso --color-scale'    # all list
      alias lxaa='lx .?* -d'                                            


      alias lta='exa --group-directories-first -lT'
      alias lt1='exa --group-directories-first -lT -L 1'
      alias lt2='exa --group-directories-first -lT -L 2'
      alias lt3='exa --group-directories-first -lT -L 3'
      alias lt4='exa --group-directories-first -lT -L 4'
      alias lt=lt2
    fi
}




# TOOL: FZF RELATED FUNCTIONS {{{1
# FZF with Brew {{{3
# Install (one or multiple) selected application(s)
# using "brew search" as source input
# mnemonic [B]rew [I]nstall [P]lugin
bip() {
  local inst=$(brew search | fzf -m)

  if [[ $inst ]]; then
    for prog in $(echo $inst);
    do; brew install $prog; done;
  fi
}
# Update (one or multiple) selected application(s)
# mnemonic [B]rew [U]pdate [P]lugin
bup() {
  local upd=$(brew leaves | fzf -m)

  if [[ $upd ]]; then
    for prog in $(echo $upd);
    do; brew upgrade $prog; done;
  fi
}
# Delete (one or multiple) selected application(s)
# mnemonic [B]rew [C]lean [P]lugin (e.g. uninstall)
bcp() {
  local uninst=$(brew leaves | fzf -m)

  if [[ $uninst ]]; then
    for prog in $(echo $uninst);
    do; brew uninstall $prog; done;
  fi
}

# FZF with File {{{3
# alternative using ripgrep-all (rga) combined with fzf-tmux preview
# implementation below makes use of "open" on macOS, which can be replaced by other commands if needed.
# allows to search in PDFs, E-Books, Office documents, zip, tar.gz, etc. (see https://github.com/phiresky/ripgrep-all)
# find-in-file - usage: fif <searchTerm> or fif "string with spaces" or fif "regex"
fif() {
    if [ ! "$#" -gt 0 ]; then echo "Need a string to search for!"; return 1; fi
    local file
    file="$(rga --max-count=1 --ignore-case --files-with-matches --no-messages "$@" | fzf-tmux +m --preview="rga --ignore-case --pretty --context 10 '"$@"' {}")" && open "$file"
}

# FZF with kill {{{3
# fkill - kill processes - list only the ones you can kill. Modified the earlier script.
fkilll() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID  | fzf -m --header-lines=1 | awk '{print $2}')
    else
        pid=$(ps -ef | fzf -m --header-lines=1 | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}


#}}}
#
# METHOD: : Lazy Plugin Load {{{2
# --------------------------------------------------------------------------
# alias helm3="/usr/local/Cellar/helm/3.2.1/bin/helm"
alias helm2="/usr/local/Cellar/helm@2/2.16.7/bin/helm"

# if [ $commands[helm] ]; then
#     helm() {
#         unfunction "$0"
#         source <(helm completion zsh | sed -E 's/\["(.+)"\]/\[\1\]/g')
#         $0 "$@"
#     }
# fi


nvm() {
  unfunction "$0"
  export NVM_DIR="$HOME/.nvm"
  # lazy load for nvm. Take 1.5 second to load
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"                   # This loads nvm
  [ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion" # This loads nvm bash_completion
  $0 "$@"
}

# Lazy load example {{{3
# if [ $commands[kubectl] ]; then
#
#   # Placeholder 'kubectl' shell function:
#   # Will only be executed on the first call to 'kubectl'
#   kubectl() {
#
#     # Remove this function, subsequent calls will execute 'kubectl' directly
#     unfunction "$0"
#
#     # Load auto-completion
#     source <(kubectl completion zsh)
#
#     # Execute 'kubectl' binary
#     $0 "$@"
#   }
# fi
# }}}

