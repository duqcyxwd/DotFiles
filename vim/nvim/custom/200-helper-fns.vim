" /******************************************************/
" /*        _             _          _                  */
" /* __   _(_)_ __ ___   | |__   ___| |_ __   ___ _ __  */
" /* \ \ / / | '_ ` _ \  | '_ \ / _ \ | '_ \ / _ \ '__| */
" /*  \ V /| | | | | | | | | | |  __/ | |_) |  __/ |    */
" /*   \_/ |_|_| |_| |_| |_| |_|\___|_| .__/ \___|_|    */
" /*                                  |_|               */
" /*   __                  _   _                        */
" /*  / _|_   _ _ __   ___| |_(_) ___  _ __             */
" /* | |_| | | | '_ \ / __| __| |/ _ \| '_ \            */
" /* |  _| |_| | | | | (__| |_| | (_) | | | |           */
" /* |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|           */
" /*                                                    */
" /******************************************************/

" [Function] Strip Whitespace {{{1
" Cleanup extra space when save
" Strip trailing whitespace without affect search and current cursor
 function! g:StripWhitespace()
         let save_cursor = getpos(".")
         let old_query = getreg('/')
         :%s/\s\+$//e
         call setpos('.', save_cursor)
         call setreg('/', old_query)
 endfunction

let s:autoStripSpaceEnabled = 0
function! ToggleAutoStripSpace()
  if s:autoStripSpaceEnabled
    augroup STRIP_WHITESPACE
      autocmd!
    augroup END
    let s:autoStripSpaceEnabled = 0
  else
    augroup STRIP_WHITESPACE
      autocmd!
      autocmd BufWritePre * call StripWhitespace()
    augroup END
    let s:autoStripSpaceEnabled = 1
  endif
endfunction
