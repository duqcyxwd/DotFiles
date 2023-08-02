"  _   _         __     ___              ____             __ _
" | \ | | ___  __\ \   / (_)_ __ ___    / ___|___  _ __  / _(_) __ _
" |  \| |/ _ \/ _ \ \ / /| | '_ ` _ \  | |   / _ \| '_ \| |_| |/ _` |
" | |\  |  __/ (_) \ V / | | | | | | | | |__| (_) | | | |  _| | (_| |
" |_| \_|\___|\___/ \_/  |_|_| |_| |_|  \____\___/|_| |_|_| |_|\__, |
"                                                              |___/
" @duqcyxwd
" https://github.com/duqcyxwd

" Pre config {{{1
" ------------------------------------------------------------------------------
" Not using default filetypes to boost startup speed
" let g:did_load_filetypes = 1
" let g:loaded_matchit     = 1
" runtime macros/matchit.vim

set verbosefile=$XDG_CACHE_HOME/nvim/nvim.log
set verbose=0

" Loading vimscripts {{{1
" ------------------------------------------------------------------------------
let vimrootPath = $XDG_CONFIG_HOME.'/vim/before/*.vim'
for f in split(glob(vimrootPath), '\n')
  exe 'source' f
endfor


" Loading lua before script {{{1
" ------------------------------------------------------------------------------
let luarootPath = $XDG_CONFIG_HOME.'/vim/lua/plugins-before/*.lua'
for f in split(glob(luarootPath), '\n')
  exe 'luafile' f
endfor

function! s:LazyLoadPlugs(timer) abort  " {{{1

  " 1. Lazy Load vim plugs before config
  " Load lua plugins and require plugin before config
  " call plug#load(
  "       \ )

        " \   'nvim-lspconfig',
        " \   'lspsaga.nvim',

  " 2. Lazy Load vim plugs before config
  let vimrootPath = $XDG_CONFIG_HOME.'/vim/after/*.vim'
  for f in split(glob(vimrootPath), '\n')
    exe 'source' f
  endfor

  " 3. Lazy Load vim lua plugs
  source $XDG_CONFIG_HOME/nvim/lua/core.lua

  " 4. Lazy Load vim lua plugs after
  " Some config need lazy load and some config need load after config
  " save current position by marking Z because plug#load reloads current buffer
  call plug#load(
        \   'vim-visual-multi',
        \   'quick-scope',
        \   'fzf',
        \   'fzf.vim',
        \   'indentLine',
        \ )
  echom "Async config loaded"

  silent! VMTheme neon
  silent! edit!

endfunction
" }}}1

" call lazy plugs {{{1
" ------------------------------------------------------------------------------
" {time} is the waiting time in milliseconds.
call timer_start(200, function("s:LazyLoadPlugs"))

" echom "Loading plugsins without lazy
" call s:LazyLoadPlugs(200)
