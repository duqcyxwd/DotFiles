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

" Setting toggle/Config Functions
" [Functions] Toggle Reload VIM file when save {{{1
" ------------------------------------------------------------------------------
" The only reason we modify vim file is source it.
" Why not make this happend in a magic way?!


" [Functions] Toggle Reload VIM file when save {{{1
" ------------------------------------------------------------------------------
" The only reason we modify vim file is source it.
" Why not make this happend in a magic way?!

function! g:SourceCurrentFile()
  echom "Auto Source Current file" expand("%")
  let save_cursor = getcurpos()
  source %
  call setpos('.', save_cursor)
endfunction

" autocmd! BufWritePost vim call ReloadVimrc()

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

" [Functions] Method to select current world or Selection {{{1
" -------------------------------------------------------------------------------
" An Sample code work with n mode and v mode

function! s:_get_last_selected() abort    " {{{2
  " https://github.com/tyru/open-browser.vim/blob/master/autoload/vital/_openbrowser/Vim/Buffer.vim#L94
  if visualmode() ==# "\<C-v>"
    let save = getreg('"', 1)
    let save_type = getregtype('"')
    try
      normal! gv""y
      return @"
    finally
      call setreg('"', save, save_type)
    endtry
  else
    let [begin, end] = [getpos("'<"), getpos("'>")]
    let lastchar = matchstr(getline(end[1])[end[2]-1 :], '.')
    if begin[1] ==# end[1]
      let lines = [getline(begin[1])[begin[2]-1 : end[2]-2]]
    else
      let lines = [getline(begin[1])[begin[2]-1 :]]
      \         + (end[1] - begin[1] <# 2 ? [] : getline(begin[1]+1, end[1]-1))
      \         + [getline(end[1])[: end[2]-2]]
    endif
    return join(lines, "\n") . lastchar . (visualmode() ==# 'V' ? "\n" : '')
  endif
endfunction


function! g:GetLastSelect() abort "{{{2
  let selected_text = s:_get_last_selected()
  let text = substitute(selected_text, '[\n\r]\+', ' ', 'g')
  return substitute(text, '^\s*\|\s*$', '', 'g')
endfunction

function! g:GetCurrentWord(mode) abort "{{{2
  if a:mode is# 'n'
    return expand('<cword>')
  else
    return GetLastSelect()
  endif
endfunction


function! g:ModeTestFn(mode) abort " {{{2
  echom "Message from TESTFN"
  if a:mode is# 'n'
    echom "n mode"
    echom 'Text:' expand('<cword>')
  else
    echom "v mode"
    echom 'Text:' GetCurrentWord('v')
  endif
endfunction


" Testing Mapping {{{2
nnoremap <silent> <Plug>(mytestFn) :<C-u>call ModeTestFn('n')<CR>
xnoremap <silent> <Plug>(mytestFn) :<C-u>call ModeTestFn('v')<CR>
" <SPC>rt run test
" I can avoid using
" vnoremap <Space>ss y:FFBLines <C-R>=escape(@",'/\()')<CR><CR>

"}}}2

" }}}1
function! g:RunUsingCurrentFiletype() "{{{1
  " Run current file
  " execute 'write'
  execute '! clear; '.&filetype.' <% '

endfunction

function! g:Show_documentation() abort "{{{1
  " Show vim regular docs
  echom "Show_documentation"
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
  endif
endfunction


" }}}1
