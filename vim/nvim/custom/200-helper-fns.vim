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
" --------------------------------------------------------------------------------
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
call ToggleAutoStripSpace()

" [Function] Fold Related Fns {{{1
" --------------------------------------------------------------------------------

function! ToggleFoldColumn()
  if &foldcolumn == 3
    set foldcolumn=0
  else
    set foldcolumn=3
  endif
endfunction

let s:foldMethodList = ['manual', 'indent', 'expr', 'marker', 'syntax', 'diff']
let s:foldLength=len(s:foldMethodList)

let s:foldMethod = 0
function! LoopFoldMethod()
  execute "set foldmethod=".s:foldMethodList[s:foldMethod]
  let s:foldMethod +=1
  if s:foldMethod >= s:foldLength
    let s:foldMethod =0
  endif
endfunction

" [Function] Reload VIM file when save {{{1
" --------------------------------------------------------------------------------
" The only reason we modify vim file is source it.
" Why not make this happend in a magic way?!

function! SourceCurrentFile()
  echom "Auto Source Current file" expand("%")
  let save_cursor = getcurpos()
  source %
  call setpos('.', save_cursor)
endfun

" autocmd! BufWritePost vim call ReloadVimrc()



" WIP
let s:autoVimSourceEnabled = 0
function! ToggleAutoVimSource()
  if s:autoVimSourceEnabled
    echo "Turn off autoVimSource"
    augroup VimSource
      autocmd!
    augroup END
    let s:autoVimSourceEnabled = 0
  else
    echo "Turn on autoVimSource"
    augroup VimSource
      autocmd FileType vim
            \ autocmd! VimSource BufWritePost <buffer> call SourceCurrentFile()
    augroup END
    let s:autoVimSourceEnabled = 1
  endif
endfunction
" call ToggleAutoVimSource()

