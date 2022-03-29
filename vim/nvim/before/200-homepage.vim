" => startify {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:startify_custom_header_h = [
      \' /**********************************************************************/',
      \' /*       ___          ___          ___          ___          ___      */',
      \' /*      /\  \        /\  \        /\  \        /\__\        /|  |     */',
      \' /*      \:\  \       \:\  \      /::\  \      /:/  /       |:|  |     */',
      \' /*       \:\  \       \:\  \    /:/\:\  \    /:/  /        |:|  |     */',
      \' /*   ___ /::\  \  ___ /::\  \  /:/ /::\  \  /:/  /  ___  __|:|  |     */',
      \' /*  /\  /:/\:\__\/\  /:/\:\__\/:/_/:/\:\__\/:/__/  /\__\/\ |:|__|____ */',
      \' /*  \:\/:/  \/__/\:\/:/  \/__/\:\/:/  \/__/\:\  \ /:/  /\:\/:::::/__/ */',
      \' /*   \::/__/      \::/__/      \::/__/      \:\  /:/  /  \::/~~/~     */',
      \' /*    \:\  \       \:\  \       \:\  \       \:\/:/  /    \:\~~\      */',
      \' /*     \:\__\       \:\__\       \:\__\       \::/  /      \:\__\     */',
      \' /*      \/__/        \/__/        \/__/        \/__/        \/__/     */',
      \" /* Happy Hacking. Chuan's new vim                                     */",
      \' /**********************************************************************/',
      \ ]

let g:startify_custom_header = 'startify#center(g:startify_custom_header_h)'

let g:startify_padding_left = 5
let g:startify_files_number = 6
let g:startify_enable_special = 0
let g:startify_session_sort = 1
let g:startify_session_persistence = 1
let g:startify_bookmarks = [{'zn': '~/.config/vim/init.vim'},
      \ {'zz': '~/.config/zsh/.zshrc'},
      \ {'za': '~/.config/alacritty/alacritty.yml'},
      \ {'zt': '~/.tmux.conf'}
      \]

let g:startify_session_before_save = [ 'silent! tabdo NERDTreeClose' ]

" returns all modified files of the current git repo
" `2>/dev/null` makes the command fail quietly, so that when we are not
" in a git repo, the list will be empty
function! s:gitModified()
    let files = systemlist('git ls-files -m 2>/dev/null')
    return map(files, "{'line': v:val, 'path': v:val}")
endfunction

" same as above, but show untracked files, honouring .gitignore
function! s:gitUntracked()
    let files = systemlist('git ls-files -o --exclude-standard 2>/dev/null')
    return map(files, "{'line': v:val, 'path': v:val}")
endfunction

let g:startify_lists = [
      \ { 'type': 'sessions',  'header': ['   Sessions'], 'indices': ['a', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']},
      \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
      \ { 'type': 'commands',  'header': ['   Commands']       },
      \ { 'type': 'files',     'header': ['   MRU']            },
      \ { 'type': function('s:gitModified'),  'header': ['   git modified']},
      \ { 'type': function('s:gitUntracked'), 'header': ['   git untracked']},
      \ { 'type': 'dir',       'header': ['   PWD '. getcwd()] },
      \ ]


let g:startify_commands = [
            \ {'b': ['Back to last loaded Session', 'SLoad!']},
            \ ]

" # A Smart way to auto generate Sessions
function! GetUniqueSessionName()
  let path = fnamemodify(getcwd(), ':~:t')
  let path = empty(path) ? 'no-project' : path
  let branch = gitbranch#name()
  let branch = empty(branch) ? '' : '' . branch

  " let path =  '[' . path . ']'
  let path =  '_' . path . '_'
  return substitute(path . branch, '/', '-', 'g')
endfunction

function! GetCurrentSession()
    return fnamemodify(v:this_session, ':t')
endfunction

function! SaveCurrentSessions()
    let session_name = fnamemodify(v:this_session, ':t')
    if session_name ==# ''
      let session_name = '_last_open'
    endif

    " mksession! $XDG_DATA_HOME/nvim/session/_last_open
    " execute 'mksession! $XDG_DATA_HOME/nvim/session/' . session_name

    " Save _last_open first, so __LAST__ will point to current session
    silent execute 'SSave! _last_open'
    silent execute 'SSave! ' . session_name

    " silent execute 'SSave! ' . GetUniqueSessionName()
endfunction



" Manully update last open session so __LAST__ will not be updated
autocmd VimLeave *             call SaveCurrentSessions()

" tpope/vim-obsession has conflict with startify which lead to empty buffer

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
