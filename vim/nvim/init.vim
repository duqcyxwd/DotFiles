"  _   _         __     ___              ____             __ _
" | \ | | ___  __\ \   / (_)_ __ ___    / ___|___  _ __  / _(_) __ _
" |  \| |/ _ \/ _ \ \ / /| | '_ ` _ \  | |   / _ \| '_ \| |_| |/ _` |
" | |\  |  __/ (_) \ V / | | | | | | | | |__| (_) | | | |  _| | (_| |
" |_| \_|\___|\___/ \_/  |_|_| |_| |_|  \____\___/|_| |_|_| |_|\__, |
"                                                              |___/
" @duqcyxwd
" https://github.com/duqcyxwd

" Not using default filetypes to boost startup speed
let g:did_load_filetypes = 1
let g:loaded_matchit     = 1
runtime macros/matchit.vim

let vimrootPath = $XDG_CONFIG_HOME.'/vim/before/*.vim'
for f in split(glob(vimrootPath), '\n')
  exe 'source' f
endfor

let luarootPath = $XDG_CONFIG_HOME.'/vim/lua/plugins-before/*.lua'
for f in split(glob(luarootPath), '\n')
  exe 'luafile' f
endfor

function! s:LazyLoadPlugs(timer) abort
  " Load lua plugins and require plugin before config
  call plug#load(
        \   'nvim-lspconfig',
        \   'lspsaga.nvim',
        \   'lsp-status.nvim',
        \   'nvim-treesitter-textobjects',
        \   'nvim-treesitter-refactor',
        \   'nvim-ts-rainbow',
        \   'playground',
        \   'nvim-treesitter-context',
        \   'nvim-treesitter-pairs',
        \ )

  let vimrootPath = $XDG_CONFIG_HOME.'/vim/after/*.vim'
  for f in split(glob(vimrootPath), '\n')
    exe 'source' f
  endfor

  source $XDG_CONFIG_HOME/nvim/lua/core.lua

  " Some config need lazy load and some config need load after config
  " save current position by marking Z because plug#load reloads current buffer
  call plug#load(
        \   'vim-visual-multi',
        \   'quick-scope',
        \   'fzf',
        \   'fzf.vim',
        \   'vim-endwise',
        \   'indentLine',
        \ )
  echom "Async config loaded"

  silent! VMTheme neon
endfunction

" {time} is the waiting time in milliseconds.
call timer_start(20, function("s:LazyLoadPlugs"))

" set verbosefile=~/temp/log/vim.log
" set verbose=1
