" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann

function! CmdLine(str) abort
    exe "menu Foo.Bar :" . a:str
    emenu Foo.Bar
    unmenu Foo
endfunction

" NOT WORKING
function! VisualSelection(direction, extra_filter) range abort
  let l:saved_reg = @"
  execute "normal! vgvy"

  let l:pattern = escape(@", '\\/.*$^~[]')
  let l:pattern = substitute(l:pattern, "\n$", "", "")

  if a:direction == 'b'
    execute "normal ?" . l:pattern . "^M"
  elseif a:direction == 'gv'
    call CmdLine("Ack \"" . l:pattern . "\" " )
  elseif a:direction == 'replace'
    call CmdLine("%s" . '/'. l:pattern . '/')
  elseif a:direction == 'f'
    execute "normal /" . l:pattern . "^M"
  endif

  let @/ = l:pattern
  let @" = l:saved_reg
endfunction

" vnoremap <silent> * :call VisualSelection('f', '')<CR>
" vnoremap <silent> # :call VisualSelection('b', '')<CR>


" Search based current word
" :echo expand('<cWORD>')
" nnoremap <Space>* :execute '/' . expand('<cWORD>')<CR>
" nnoremap <Space>sB :execute ':FFBLines ' . expand('<cWORD>')<CR>

vnoremap <silent> * :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy/<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
  \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gVzv:call setreg('"', old_reg, old_regtype)<CR>
vnoremap <silent> # :<C-U>
  \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
  \gvy?<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
  \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
  \gVzv:call setreg('"', old_reg, old_regtype)<CR>

" Easy version
" https://vim.fandom.com/wiki/Search_for_visually_selected_text#:~:text=To%20use%20the%20mapping%2C%20visually,search%20for%20the%20next%20occurrence.
" vnoremap * y/\V<C-R>=escape(@",'/\')<CR><CR>
" vnoremap # y?\V<C-R>=escape(@",'/\')<CR><CR>

" Escape the special characters for Ag
" vnoremap ,g y:MyFzfAg <C-R>=escape(@",'/\()')<CR>

