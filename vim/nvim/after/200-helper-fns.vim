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
" [Functions] Vimdiff whitespace toggle {{{1
" ------------------------------------------------------------------------------
function! VimDiffWhiteToggle() abort
  if &diffopt =~ 'iwhite'
    set diffopt-=iwhite
  else
    set diffopt+=iwhite
  endif
endfunction

" [Functions] HighlightCharactersOver80 {{{1
" ------------------------------------------------------------------------------
" Highlight characters in column 81+ with a red background.
" (source: https://stackoverflow.com/a/235970/2338327)
" let s:highlightCharacterOver80 = 1
" function! s:HighlightCharactersOver80() abort
"   if s:highlightCharacterOver80
"     highlight OverLength ctermbg=red ctermfg=white guibg=#592929
"           \ | match OverLength /\%81v.\+/
"   else
"     highlight OverLength ctermbg=red ctermfg=white guibg=#592929
"           \ | match OverLength /\%999v.\+/
"   endif
" endfunction

function! ToggleTextWidthWithColor()
  if &textwidth == 0
    set textwidth=80
    let s:highlightCharacterOver80 = 1
    highlight OverLength ctermbg=red ctermfg=white guibg=#592929
          \ | match OverLength /\%81v.\+/
    syntax on
  else
    set textwidth=0
    let s:highlightCharacterOver80 = 0
    highlight OverLength ctermbg=red ctermfg=white guibg=#592929
          \ | match OverLength /\%999v.\+/
    syntax on
  endif
endfunction

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

" [Functions] Toggle-find-root-scope {{{1
" ------------------------------------------------------------------------------
function! ToggleFindRootScope()
  if g:findroot_search_parent == 0
    let g:findroot_search_parent = 1
    let g:findroot_patterns = g:default_patterns
    execute 'FindRoot!'
    echo printf("[Parent] find root path %s", getcwd())
  else
    let g:findroot_search_parent = 0
    let g:findroot_patterns = g:default_patterns + [ '.vimroot' ]
    execute 'FindRoot!'
    echo printf("[VimRoot] find root path %s", getcwd())
  endif
endfunction

" [Functions] Fold Related Fns {{{1
" ------------------------------------------------------------------------------
function! ToggleFoldColumn()
  if &foldcolumn == 0
    set foldcolumn=3
  else
    set foldcolumn=0
  endif
endfunction

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

" [Functions] GET Last selections {{{1
" -------------------------------------------------------------------------------
"
" An Sample code work with n mode and v mode
" Example from https://github.com/tyru/open-browser.vim/blob/master/autoload/vital/__openbrowser__/OpenBrowser.vim#L444
"

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
nnoremap <silent> <Plug>(mytestFn) :<C-u>call TestFn('n')<CR>
xnoremap <silent> <Plug>(mytestFn) :<C-u>call TestFn('v')<CR>
" I can avoid using
" vnoremap <Space>ss y:FFBLines <C-R>=escape(@",'/\()')<CR><CR>

"}}}2

" }}}1


" Standalone Functions
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

function! g:PlugLoaded(name) "{{{1
  "https://vi.stackexchange.com/questions/10939/how-to-see-if-a-plugin-is-active
  return (
        \ has_key(g:plugs, a:name) &&
        \ isdirectory(g:plugs[a:name].dir) &&
        \ stridx(&rtp, g:plugs[a:name].dir) >= 0)
endfunction

function! g:RunUsingCurrentFiletype() "{{{1
  " Run current file
  execute 'write'
  execute '! clear; '.&filetype.' <% '
endfunction

function! g:SafeFzfQuery(str) "{{{1
  " echom substitute("test/test[x[x[x[x]]]] ok (TEST) * - # ok\"", "[\"\n\/\.\\][()#*-]", " ", "g")
  " echom substitute(a:str, "[\"\n\/\.\\][()#*-]", " ", "g")
  return substitute(a:str, "[\"\n\/\.\\][()#*-]", " ", "g")
endfunc


function! g:OpenGithubPlugin() "{{{1
  " MY Function to open vim Plugin page in Github
  " let s:uri = matchstr(getline("."), "'[a-z]*'")
  let s:uri = matchstr(getline("."), "[0-9a-z\-\_\.]*\/[0-9a-z\-\_\.]*")
  let s:uri = "https://www.github.com/".s:uri
  if s:uri != ""
    silent exec "!open '".s:uri."'"
    :redraw!
  else
    echo "no packages found"
  endif
endfunction

function! g:Tab_buf_switch(num) abort "{{{1
  " A single function to switch buffers or tabs
  " https://www.jianshu.com/p/5d23dbf1b7b2
  if exists('g:feat_enable_airline') && g:feat_enable_airline == 1
    execute 'normal '."\<Plug>AirlineSelectTab".a:num
  else
    if exists( '*tabpagenr' ) && tabpagenr('$') != 1
      " Tab support && tabs open
      execute 'normal '.a:num.'gt'
    else
      let l:temp=a:num
      let l:buf_index=a:num
      let l:buf_count=len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
      if l:temp > l:buf_count
        return
      endif
      while l:buf_index != 0
        while !buflisted(l:temp)
          let l:temp += 1
        endw
        let l:buf_index -= 1
        if l:buf_index != 0
          let l:temp += 1
        endif
      endw
      execute ':'.l:temp.'b'
    endif
  endif
endfunction

function! g:GotoFirstFloat() abort  " {{{1
  for w in range(1, winnr('$'))
    let c = nvim_win_get_config(win_getid(w))
    if c.focusable && !empty(c.relative)
      execute w . 'wincmd w'
      nmap <buffer> q :q<CR>
      return
    endif
  endfor
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

function! g:Show_buffer_info() abort "{{{1
  verbose set syntax filetype foldmethod foldexpr
  echo "\n"
  echo "Current session: " . GetCurrentSession()
  echo "Project Path:    " . getcwd()
  echo "Current file:    " . expand("%:p")
  " ':verbose set syntax filetype foldmethod foldexpr<CR>:echo "\\nProject Path: " . getcwd()<CR>:echo "Current session: " . GetCurrentSession()<CR>:echo "Current file: " . expand("%:p")<CR>'
endfunction

" }}}1


" Plugin Enhancement
" [Functions] MyFzfLua search {{{1
let s:TYPE = {'dict': type({}), 'funcref': type(function('call')), 'string': type(''), 'list': type([])}
function! s:LuaBlines(query, ...) abort  "{{{2
  if empty(a:query)
    lua require('fzf-lua').lines({ current_buffer_only = true })
  else
    execute "lua require('fzf-lua').lines({ search = '" . a:query . " ', current_buffer_only = true })"
  endif
endfunction

function! s:LuaLines(query, ...) abort  "{{{2
  if empty(a:query)
    lua require('fzf-lua').lines()
  else
    execute "lua require('fzf-lua').lines({ search = '" . a:query . " '})"
  endif
endfunction

function! s:LuaGrep(query, ...) abort  "{{{2
  if empty(a:query)
    lua require('fzf-lua').grep()
  else
    execute "lua require('fzf-lua').grep({ search = '" . a:query . " '})"
  endif
endfunction
" }}}2

" Writing above functions to handle optional parameter. There is a easy way if I don't want to worry about optional parameter
" For example:  command! -nargs=1 MyFzfLuaGrep lua require('fzf-lua').grep({ search = <f-args> })

command! -nargs=* MyFzfLuaLines  call    s:LuaLines(<q-args>)
command! -nargs=* MyFzfLuaBlines call    s:LuaBlines(<q-args>)
command! -nargs=* MyFzfLuaGrep   call    s:LuaGrep(<q-args>)
command! -nargs=0 ProjectFiles   execute 'FFFiles' expand('%:p:h')

" }}}1

" COMMAND
" -------------------------------------------------------------------------------
command! CDC cd %:p:h             " CDC = Change to Directory of Current file



" Test Scripts {{{1
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

function! g:TestFunc( k, val = 10 )
  echomsg 'k: ' a:k
  echomsg 'val: ' a:val
endfunction

command! -nargs=* Test call TestFunc(<f-args>)
