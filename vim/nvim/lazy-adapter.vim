" Loading vimscripts {{{1
" ------------------------------------------------------------------------------
function! s:LazyLoadPlugs(timer) abort  " {{{1

  " 2. Lazy Load vim plugs before config
  let vimrootPath = $XDG_CONFIG_HOME.'/vim/after/*.vim'
  for f in split(glob(vimrootPath), '\n')
    exe 'source' f
  endfor

  " silent! VMTheme neon

  " Reload current file
  " silent! edit!

endfunction
" }}}1

" call lazy plugs {{{1
" ------------------------------------------------------------------------------
" {time} is the waiting time in milliseconds.
call timer_start(200, function("s:LazyLoadPlugs"))
