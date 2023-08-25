#!/bin/sh

# Bindkey should be part of snippets. I move it under function so it can be called to
# overwrite some bindkey after plugin lazy loading


zle -N _expand_stuff
function _expand_stuff() { zle _expand_alias || zle .expand-word || true }

__zsh_cust_bindkey() {
  mlog "bindkey.zsh: Update bindkey"

  # jkhl is preserved by tmux

  # bindkey '^k'      autosuggest-accept
  bindkey "^O"      autosuggest-execute
  bindkey '^U'      vi-forward-blank-word-end  # only use one from $zsh_autosuggest_partial_accept_widgets
  bindkey '^N'      down-line-or-history
  bindkey "^R"      history-search-multi-word  # use multi word. fzf is too aggressive
  bindkey '^G'      fzf-cd-widget              # Search and goto fzf
  bindkey '^F'      ls_fuzzy_preview_widget
  bindkey '^T'      toggle-fzf-tab
  bindkey '^ '      _expand_stuff                           # ctrl+space expand alias/glob/parameter
  bindkey '^I'      fzf-tab-complete                 # fzf-tab-complete

  # ^M for enter
  # bindkey "^M"      accept-line
  # bindkey "^T"      forward-word

}

__zsh_cust_bindkey
