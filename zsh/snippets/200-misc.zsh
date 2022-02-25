#!/bin/sh

alias p10ksigns="open https://github.com/romkatv/powerlevel10k#what-do-different-symbols-in-git-status-mean"


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
      alias lsa='exa -a'
      alias lsr='/bin/ls'                                                # Raw ls

      # g git, a, all
      alias l='exa -lhF'                                                 # list, size, type
      alias ld='exa -lhFD'                                               # list, size, type
      alias lg='l --git --no-user --no-permissions'                      # list, size, type, git
      alias lss='l -s ext'                                               # list, size, type

      alias la='exa -lbFa'
      alias laa='la .?* -d'                                              # Show hidden files only
      alias lag='la --git'

      alias ll='l -G'                                                    # long list
      alias lls='ll -s ext'                                              # long list sort
      alias llg='ll --git'                                               # long list

      alias lla='ll -a'                                                  # long list
      alias llag='lla --git'                                             # long list
      alias llaa='lla .?* -d'                                            # long list

      alias lx='exa -lbhHigUmuSa --time-style=long-iso --color-scale'    # all list
      alias lxaa='lx .?* -d'


      alias lta='exa --group-directories-first -lTa'
      alias lt1='exa --group-directories-first -lT -L 1'
      alias lt2='exa --group-directories-first -lT -L 2'
      alias lt3='exa --group-directories-first -lT -L 3'
      alias lt4='exa --group-directories-first -lT -L 4'
      alias lt5='exa --group-directories-first -lT -L 5'
      alias lt6='exa --group-directories-first -lT -L 6'
      alias lt7='exa --group-directories-first -lT -L 7'
      alias lt=lt2
    fi
}

#
# METHOD: : Lazy Plugin Load {{{2
# --------------------------------------------------------------------------
# alias helm3="/usr/local/Cellar/helm/3.2.1/bin/helm"
alias helm2="/usr/local/Cellar/helm@2/2.17.0/bin/helm"

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


nn () #{{{1
{
    # Block nesting of nnn in subshells
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The default behaviour is to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, remove the "export" as in:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    # NOTE: NNN_TMPFILE is fixed, should not be modified
    # export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}

# Notes: Use ranger instead of nnn

export NNN_BMS='c:~/code/cenx;d:~/duqcyxwd/DotFiles;z:~/.config/zsh/'
export NNN_FIFO=${XDG_CACHE_HOME}/nnn.fifo
# -n start in type-to-nav mode
# -E use $EDITOR for internal undetached edits
# -o open files only on Enter key
# -r show cp, mv progress
# -x show notis on selection cp, mv, rm completion
# -c indicates that the opener is a cli-only opener (overrides -e) [NOT working]
# export NNN_OPTS="cErx"

export NNN_OPTS="erxH"
BLK="04" CHR="04" DIR="04" EXE="00" REG="00" HARDLINK="00" SYMLINK="06" MISSING="00" ORPHAN="01" FIFO="0F" SOCK="0F" OTHER="02"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"

export BAT_STYLE=plain
NNN_PLUG=''

NNN_PLUG=$NNN_PLUG'd:diffs;'
NNN_PLUG=$NNN_PLUG'f:finder;'
NNN_PLUG=$NNN_PLUG'l:filegitlog;'
NNN_PLUG=$NNN_PLUG'n:nuke;'
NNN_PLUG=$NNN_PLUG't:treeview;'
NNN_PLUG=$NNN_PLUG'v:preview-tui;'

export NNN_PLUG


function kube-toggle() { #{{{1
  if (( ${+POWERLEVEL9K_KUBECONTEXT_SHOW_ON_COMMAND} )); then
    unset POWERLEVEL9K_KUBECONTEXT_SHOW_ON_COMMAND
  else
    POWERLEVEL9K_KUBECONTEXT_SHOW_ON_COMMAND='kubectl|helm|kubens|kubectx|oc|istioctl|kogito|k9s|helmfile|fluxctl|stern|n|set-ns|set|set-context|context|c|'
  fi
  p10k reload
  if zle; then
    zle push-input
    zle accept-line
  fi
}
