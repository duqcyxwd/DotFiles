" Lazy loader adapter to load orignal vim file
set verbosefile=$XDG_CACHE_HOME/nvim/nvim.log
set verbose=0

" Loading vimscripts {{{1
" ------------------------------------------------------------------------------
function! s:LazyLoadPlugs(timer) abort  " {{{1

  " 2. Lazy Load vim plugs before config
  let vimrootPath = $XDG_CONFIG_HOME.'/vim/after/*.vim'
  for f in split(glob(vimrootPath), '\n')
    exe 'source' f
  endfor

  " silent! VMTheme neon
  silent! edit!

endfunction
" }}}1

" call lazy plugs {{{1
" ------------------------------------------------------------------------------
" {time} is the waiting time in milliseconds.
call timer_start(200, function("s:LazyLoadPlugs"))
