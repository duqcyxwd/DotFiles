" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann

" vnoremap <silent> * :<C-U>
"   \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
"   \gvy/<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
"   \escape(@", '/\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
"   \gVzv:call setreg('"', old_reg, old_regtype)<CR>
" vnoremap <silent> # :<C-U>
"   \let old_reg=getreg('"')<Bar>let old_regtype=getregtype('"')<CR>
"   \gvy?<C-R>=&ic?'\c':'\C'<CR><C-R><C-R>=substitute(
"   \escape(@", '?\.*$^~['), '\_s\+', '\\_s\\+', 'g')<CR><CR>
"   \gVzv:call setreg('"', old_reg, old_regtype)<CR>

" Easy version
" https://vim.fandom.com/wiki/Search_for_visually_selected_text#:~:text=To%20use%20the%20mapping%2C%20visually,search%20for%20the%20next%20occurrence.
" vnoremap * y/\V<C-R>=escape(@",'/\')<CR><CR>
" vnoremap # y?\V<C-R>=escape(@",'/\')<CR><CR>
