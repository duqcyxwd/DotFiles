#!/bin/sh

# Bindkey should be part of snippets. I move it under function so it can be called to
# overwrite some bindkey after plugin lazy loading


zle -N _expand_stuff
function _expand_stuff() { zle _expand_alias || zle .expand-word || true }

__zsh_cust_bindkey() {
  mlog "bindkey.zsh: Update bindkey"

  bindkey '^k'      autosuggest-accept
  bindkey "^n"      autosuggest-execute
  bindkey '^u'      vi-forward-blank-word-end  # only use one from $zsh_autosuggest_partial_accept_widgets
  bindkey '^\n'     autosuggest-execute
  bindkey "^r"      history-search-multi-word  # use multi word. fzf is too aggressive


  # Fzf related
  bindkey '^G'      fzf-cd-widget              # Search and goto fzf
  bindkey '^B'      ls_fuzzy_preview_widget    # My ls widget
  bindkey '^F'      ls_fuzzy_preview_widget

  bindkey '^T'      toggle-fzf-tab
  bindkey "^O"      zca-widget                # Zsh Command Architect zsh-cmd-architect

  # ^M for enter
  # bindkey "^M"      accept-line
  # bindkey "^T"      forward-word

  bindkey '^ '      _expand_stuff                           # ctrl+space expand alias/glob/parameter

  # Notes "^I" is reserved for suggestion complete
  bindkey '^I'  fzf-tab-complete                 # fzf-tab-complete
  # bindkey '^I' __enhancd::completion::run

}

__zsh_cust_bindkey
