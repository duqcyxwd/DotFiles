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
let g:startify_session_persistence = 1
let g:startify_bookmarks = [{'zn': '~/.config/vim/nvim/init.vim'},
      \ {'zz': '~/.config/zsh/.zshrc'},
      \ {'za': '~/.config/alacritty/alacritty.yml'},
      \ {'zt': '~/.tmux.conf'}
      \]
    " g:startify_session_dir|

let g:startify_session_before_save = [ 'silent! tabdo NERDTreeClose' ]

let g:startify_lists = [
      \ { 'type': 'sessions',  'header': ['   Sessions'], 'indices': ['a' ,'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']},
      \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
      \ { 'type': 'commands',  'header': ['   Commands']       },
      \ { 'type': 'files',     'header': ['   MRU']            },
      \ { 'type': 'dir',       'header': ['   PWD '. getcwd()] },
      \ ]


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
