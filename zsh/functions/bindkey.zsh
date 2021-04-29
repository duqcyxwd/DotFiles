#!/bin/sh

zle -N _expand_stuff
function _expand_stuff() { zle _expand_alias || zle .expand-word || true }

__zsh_cust_bindkey() {
  mlog "bindkey.zsh: Update bindkey"

  bindkey '^T'      fzf-file-widget
  bindkey '\ec'     fzf-cd-widget
  bindkey '^R'      fzf-history-widget

  bindkey '^k'      autosuggest-accept
  bindkey '^\n'     autosuggest-execute
  bindkey "^R"      history-search-multi-word # Use multi word. fzf is too aggressive
  # bindkey "^R"      fzf-history-widget


  # Fzf related
  bindkey "^F"      fzf-file-widget           # fzf files
  bindkey '^G'      fzf-cd-widget             # Search and goto fzf
  bindkey '^B'      fzf_ls_widget

  bindkey '^T'      toggle-fzf-tab
  bindkey "^O"      zca-widget                # Zsh Command Architect zsh-cmd-architect

  # ^M for enter
  # bindkey "^M"      accept-line
  bindkey "^T"      forward-word

  bindkey '^ '      _expand_stuff                           # ctrl+space expand alias/glob/parameter


  # Notes "^I" is reserved for suggestion complete
  # fzf complete
  bindkey '^I'  fzf-tab-complete                 # fzf-tab-complete
  # bindkey '^I' __enhancd::completion::run
  # bindkey '^I' fzf-completion                  # fzf completion. **

}

__zsh_cust_bindkey
