#!/bin/sh
# print ${functions_source[_helm]}

# SECTION: : ZSH PLAYGROUND {{{1
# --------------------------------------------------------------------------
_clean_my_cache() {
  # fd -d 1 --type d --changed-before=500d
  fd -d 1 . ~/work_credential/temp/cache --changed-before=20d | xargs trash
}

# _clean_my_cache



