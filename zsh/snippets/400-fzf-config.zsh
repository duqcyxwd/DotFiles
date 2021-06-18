#!/bin/sh
#   DEVTOOL: __fzf_config
# -------------------------------------------------------

mlog fzf-config loaded

__fzf_config() {
   # require fzf junegunn/fzf
   # brew install fzf

  # FZF KEYBINDING {{{3
    export FZF_MY_KEYBINDING="
    --bind '?:toggle-preview'

    --bind 'left:up'
    --bind 'left:+toggle'
    --bind 'right:toggle+down'
    --bind 'tab:down'
    --bind 'btab:up'


    --bind='ctrl-k:preview-half-page-up'
    --bind='ctrl-j:preview-half-page-down'

    --bind='alt-up:preview-half-page-up'
    --bind='alt-down:preview-half-page-down'

    --bind 'ctrl-h:backward-char'
    --bind 'ctrl-l:forward-char'

    --bind='ctrl-b:half-page-up'
    --bind='ctrl-f:half-page-down'

    --bind='ctrl-s:toggle-sort'
    --bind='ctrl-w:toggle-preview-wrap'

    --bind='change:top'
    "
    # Notes: In tmux with vim, ctrl-kjhl is used....
    # Need to find other better keyding

    # --bind 'ctrl-p:up'
    # --bind 'ctrl-n:down'
    # --bind 'up:previous-history'
    # --bind 'down:next-history'
    # Why not tab:toggle+down
    # {{{
    # Need a way to move cursor
    # --bind 'left:up'

    # --bind='ctrl-u:half-page-up'
    # --bind='ctrl-d:half-page-down'
    # }}}

    # Other Default keybinding
    # ctrl-n
    # ctrl-p
    #
    # ctrl-e
    # ctrl-a
    # ctrl-u
    # ctrl-w

    #### My fzf shared general keybinding
    # ctrl-r vim open remote
    # ctrl-v vim open
    # ctrl-y copy
    #
    # ctrl-o fzf-exec
    # ctrl-space  bat preview
    #

    #### Fuzzy preview
    # enter echo input
    #
    # shift-right depth increase
    # shift-left depth decrease
    #
    # alt-up/down/left/right
    #
    # }}}3
  # FZF Theme {{{3

    export FZF_COLOR_SCHEMA_BORDER="--color 'fg:#bbccdd,fg+:#ddeeff,bg:#334455,preview-bg:#223344'"


    # Dracula Theme
    local __FZF_COLOR_SCHEMA_DRACULA='
    --color=dark
    --color=fg:-1,bg:-1,hl:#5fff87,fg+:-1,bg+:-1,hl+:#ffaf5f
    --color=info:#af87ff,prompt:#5fff87,pointer:#ff87d7,marker:#ff87d7,spinner:#ff87d7
    '

    # Theme Ayu Mirage
    local __FZF_COLOR_SCHEMA_AYU='
     --color=fg:#cbccc6,bg:#1f2430,hl:#707a8c
     --color=fg+:#707a8c,bg+:#191e2a,hl+:#ffcc66
     --color=info:#73d0ff,prompt:#707a8c,pointer:#cbccc6
     --color=marker:#73d0ff,spinner:#73d0ff,header:#d4bfff'

    # Theme NORD
    local __FZF_COLOR_SCHEMA_NORD='
    --color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C,fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C
    --color pointer:#BF616A,info:#4C566A,spinner:#4C566A,header:#4C566A,prompt:#81A1C1,marker:#EBCB8B
    '

    # FZF Theme }}}
  # FZF Default Config {{{3
    # Notes: Can't put FZF_TMUX_OPTS into FZF_DEFAULT_OPTS
    # fzf-tmux is impliment in a bash script
    export FZF_TMUX=1
    export FZF_TMUX_OPTS="-p 85% "
    export TMUX_FZF_OPTIONS=$FZF_TMUX_OPTS
    export FZF_MY_OPTS="--reverse --ansi --multi --exit-0 --cycle --height 80% --preview-window border-left"

    # Use fzf-tp instead
    alias fzfp='fzf-tmux -p 85%'



    # FZF Default options
    export FZF_DEFAULT_OPTS="$FZF_MY_OPTS $FZF_MY_KEYBINDING $__FZF_COLOR_SCHEMA_DRACULA "
    # export FZF_DEFAULT_OPTS="$FZF_MY_OPTS $FZF_MY_KEYBINDING $__FZF_COLOR_SCHEMA_AYU"
    # export FZF_DEFAULT_OPTS="$FZF_MY_OPTS $FZF_MY_KEYBINDING $__FZF_COLOR_SCHEMA_NORD"


    export FZF_TMUX_HEIGHT=80%        #Aslo been used by fzf-tab

    # FZF C-f
    local FZF_PREVIEW_FILE='bat --style="numbers,changes" --color=always {} -r 0:200| head -200'
    export FZF_CTRL_T_OPTS=" --preview \"${FZF_PREVIEW_FILE}\" --header 'Searching files' "                          #fzf file

    # FZF C-g
    local FZF_PREVIEW_DIR='exa --group-directories-first -F --icons --group-directories-first -T -lh -L 2 --color=always {}'
    export FZF_ALT_C_OPTS=" --preview \"${FZF_PREVIEW_DIR}\" --header 'Go to directory' "                                                      #fzf cd Folder

    # Options to fzf command
    # export FZF_COMPLETION_OPTS='+c -x'

  # FZF fd support {{{3
    # Setting fd as the default source for fzf
    if [ $commands[fd] ]; then
      # Use fd (https://github.com/sharkdp/fd) instead of the default find
      # export FZF_DEFAULT_COMMAND="fd --type file --hidden --color=always"
      export FZF_DEFAULT_COMMAND="fd --type file --hidden --color=always --exclude={.git,.idea,.vscode,.sass-cache,node_modules}"


      # https://github.com/Aloxaf/fzf-tab/issues/65
      # export FZF_DEFAULT_COMMAND='fd --hidden --follow --type=f'
      # _fzf_compgen_path() { fd --hidden --follow --type=f }

      # Directory finder
      export FZF_ALT_C_COMMAND="fd --type d --hidden --color=always --exclude={.git,.idea,.vscode,.sass-cache,node_modules}"
      # File finder
      export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

      # Use fd (https://github.com/sharkdp/fd) instead of the default find
      # command for listing path candidates.
      # - The first argument to the function ($1) is the base path to start traversal
      # - See the source code (completion.{bash,zsh}) for the details.
      _fzf_compgen_path() { fd --hidden --follow --exclude ".git" . "$1" }

      # Use fd to generate the list for directory completion
      _fzf_compgen_dir() { fd --type d --hidden --follow --exclude ".git" . "$1" }

    fi
    # }}}3

}

__fzf_config
