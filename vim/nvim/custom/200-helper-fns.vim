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

" [Function] Toggle Strip Whitespace {{{1
" ------------------------------------------------------------------------------
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
    let s:autoStripSpaceEnabled = 0
    echo "autoStripSpaceEnabled off"
    augroup STRIP_WHITESPACE
      autocmd!
    augroup END
  else
    let s:autoStripSpaceEnabled = 1
    " echo "autoStripSpaceEnabled on"
    augroup STRIP_WHITESPACE
      autocmd!
      autocmd BufWritePre * call StripWhitespace()
    augroup END
  endif
endfunction
call ToggleAutoStripSpace()

" [Function] HighlightCharactersOver80 {{{1
" ------------------------------------------------------------------------------
" Highlight characters in column 81+ with a red background.
" (source: https://stackoverflow.com/a/235970/2338327)
let s:highlightCharacterOver80 = 1
function! s:HighlightCharactersOver80() abort
  if s:highlightCharacterOver80
    highlight OverLength ctermbg=red ctermfg=white guibg=#592929
          \ | match OverLength /\%81v.\+/
  else
    highlight OverLength ctermbg=red ctermfg=white guibg=#592929
          \ | match OverLength /\%999v.\+/
  endif
endfunction

function! ToggleTextWidth()
  if &textwidth == 0
    set textwidth=80
    let s:highlightCharacterOver80 = 1
    syntax on
  else
    set textwidth=0
    let s:highlightCharacterOver80 = 0
    syntax on
  endif
endfunction

augroup ColorSchemeMods
  autocmd!
  autocmd ColorScheme * call s:HighlightCharactersOver80()
augroup END


" [Function] Toggle Reload VIM file when save {{{1
" ------------------------------------------------------------------------------
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

" [Function] Fold Related Fns {{{1
" ------------------------------------------------------------------------------
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

" [Function] Run current file {{{1
" ------------------------------------------------------------------------------

function! RunUsingCurrentFiletype()
  execute 'write'
  execute '! clear; '.&filetype.' <% '
endfunction
" WIP
nmap <Space>rr :call RunUsingCurrentFiletype()<CR>


" }}}1
"
"
let s:whitespace_enable = 0
function! g:Toggle_whitespace() abort
  if s:whitespace_enable
    DisableWhitespace
    let s:whitespace_enable = 0
  else
    EnableWhitespace
    let s:whitespace_enable = 1
  endif
  call SpaceVim#layers#core#statusline#toggle_section('whitespace')
  call SpaceVim#layers#core#statusline#toggle_mode('whitespace')
endfunction


