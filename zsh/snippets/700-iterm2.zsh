#!/bin/sh

# iTerm2
if [[ "$TERM_PROGRAM" == 'iTerm.app' ]]; then
  # source ~/.iterm2_shell_integration.zsh
  # Overwrite iterm2 setting for tab color and brightness
  echo -e "\033]6;1;bg;red;brightness;40\a" 1>/dev/null
  echo -e "\033]6;1;bg;green;brightness;44\a" 1>/dev/null
  echo -e "\033]6;1;bg;blue;brightness;52\a" 1>/dev/null

  # Change iTerm2 Profile
  # this might work as well: iterm2_profile Performance
  alias performance='echo -e "\033]50;SetProfile=Performance\x7"'
  alias noperformance='echo -e "\033]50;SetProfile=Empty Default\x7"'

  function iterm_title {
    local name=$*
    source ~/.iterm2_shell_integration.zsh
    # Change iterm2 tab title
    echo -ne "\033]0;"$name"\007"

    iterm2_set_user_var badge $name
  }

  # Change iterm2 panel badge
  function iterm_badge {
    local name=$*
    source ~/.iterm2_shell_integration.zsh
    iterm2_set_user_var badge $name
  }

fi
