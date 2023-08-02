" Lazy loader adapter to load orignal vim file
set verbosefile=$XDG_CACHE_HOME/nvim/nvim.log
set verbose=0

" neovide config {{{1
" ------------------------------------------------------------------------------
if exists("g:neovide")
  set guifont=JetBrainsMono\ Nerd\ Font:h15
  " g:neovide_transparency should be 0 if you want to unify transparency of content and title bar.
  let g:neovide_transparency = 0
  let g:transparency = 0.92
  let g:neovide_background_color = '#0f1117'.printf('%x', float2nr(255 * g:transparency))
  let g:neovide_refresh_rate = 60
  let g:neovide_input_macos_alt_is_meta = v:false


  " system clipboard
  nmap <D-v> "+p
  inoremap <D-v> <c-r>+
  cnoremap <D-v> <c-r>+

endif

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
