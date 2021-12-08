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
endfunction

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
  if &foldcolumn == 0
    set foldcolumn=3
  else
    set foldcolumn=0
  endif
endfunction

" let s:foldMethodList = ['manual', 'indent', 'expr', 'marker', 'syntax', 'diff']
let s:foldMethodList = ['indent', 'expr', 'marker', 'syntax']
let s:foldLength=len(s:foldMethodList)

let s:foldMethod = 0
function! LoopFoldMethod()
  execute "set foldmethod=".s:foldMethodList[s:foldMethod]
  let s:foldMethod +=1
  if s:foldMethod >= s:foldLength
    let s:foldMethod =0
  endif
endfunction

function! s:DiffWithSaved() "{{{1
  " https://stackoverflow.com/questions/749297/can-i-see-changes-before-i-save-my-file-in-vim
  " Quick check before Save
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
command! DiffSaved call s:DiffWithSaved()

function! PlugLoaded(name) "{{{1
  "https://vi.stackexchange.com/questions/10939/how-to-see-if-a-plugin-is-active
  return (
        \ has_key(g:plugs, a:name) &&
        \ isdirectory(g:plugs[a:name].dir) &&
        \ stridx(&rtp, g:plugs[a:name].dir) >= 0)
endfunction

function! RunUsingCurrentFiletype() "{{{1
  " Run current file
  execute 'write'
  execute '! clear; '.&filetype.' <% '
endfunction

" }}}1


" https://stackoverflow.com/questions/10572996/passing-command-range-to-a-function/10573044#10573044
function! PrintGivenRange() range
    echo "firstline ".a:firstline." lastline ".a:lastline
    " Do some more things
endfunction

function! PrintGivenRange2() range
    " Print range
    " https://vi.stackexchange.com/questions/11025/passing-visual-range-to-a-command-as-its-argument
    " Get the line and column of the visual selection marks
    let [lnum1, col1] = getpos("'<")[1:2]
    let [lnum2, col2] = getpos("'>")[1:2]

    " Get all the lines represented by this range
    let lines = getline(lnum1, lnum2)

    " The last line might need to be cut if the visual selection didn't end on the last column
    let lines[-1] = lines[-1][: col2 - (&selection == 'inclusive' ? 1 : 2)]
    " The first line might need to be trimmed if the visual selection didn't start on the first column
    let lines[0] = lines[0][col1 - 1:]

    " Get the desired text
    let selectedText = join(lines, "\n")

    " Do the call to tmux
    echo selectedText
endfunction

command! -range PassRange <line1>,<line2>call PrintGivenRange()
