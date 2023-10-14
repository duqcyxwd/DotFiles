#!/bin/zsh

# TOOL: FZF RELATED FUNCTIONS

# FZF with process {{{1
# -------------------------------------------------------------------------------
fkilll() {

    # at test | awk '{for(i=8;i<=NF;i++){printf "%s ", $i}; printf "\n"}'
    # ps -ef | fzf_tp -m --header-lines=1 --border=none --preview-window=border-horizontal --preview-window=down,30% --preview "echo {} | awk '{for(i=8;i<=NF;i++){ printf \"%s \\n\", \$i };}' | bat"
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID  | fzf_tp -m --header-lines=1 --border=none --preview-window=border-horizontal --preview-window=down,30% --preview "echo {} | awk '{for(i=8;i<=NF;i++){ printf \"%s \\n\", \$i };}' | bat" | awk '{print $2}')
    else
        pid=$(ps -ef | fzf_tp -m --header-lines=1 --border=none --preview-window=border-horizontal --preview-window=down,30% --preview "echo {} | awk '{for(i=8;i<=NF;i++){ printf \"%s \\n\", \$i };}' | bat" | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}

psi() { #{{{2
  ps -ef | fzf_tp -m --header-lines=1 --border=none --preview-window=border-horizontal --preview-window=down,30% --preview "echo {} | awk '{for(i=8;i<=NF;i++){ printf \"%s \\n\", \$i };}' | bat"
}



# FZF Mist {{{1
# -------------------------------------------------------------------------------

pathi() { #{{{2
  echo $PATH | tr ':' '\n' | sort | fzf
}
fpathi() { #{{{2
  echo $FPATH | tr ':' '\n' | sort | fzf
}

diri() { #{{{2
  cd $(dirs | awk '{gsub(" ","\n", $0); print $0}' | fzf | sed "s|~|$HOME|g")
}

ifconfigi(){ #{{{2
  ifconfig -l | xargs -n 1 | fzf_tp --preview "ifconfig {1}" | xargs -I {} ifconfig {} $@
}
#}}}
