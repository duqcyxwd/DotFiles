" /***********************************************************/
" /*  ____  _                                             _  */
" /* |  _ \| | __ _ _   _  __ _ _ __ ___  _   _ _ __   __| | */
" /* | |_) | |/ _` | | | |/ _` | '__/ _ \| | | | '_ \ / _` | */
" /* |  __/| | (_| | |_| | (_| | | | (_) | |_| | | | | (_| | */
" /* |_|   |_|\__,_|\__, |\__, |_|  \___/ \__,_|_| |_|\__,_| */
" /*                |___/ |___/                              */
" /***********************************************************/
"
"
"
"
function! g:ToggleVerbose()
  if !&verbose
    set verbosefile=~/temp/log/vim.log
    set verbose=15
  else
    set verbose=0
    set verbosefile=
  endif
endfunction

function! g:UnmapDelimitMate()
  inoremap <silent><buffer> " "
  inoremap <silent><buffer> { {
endfunction



augroup VIM_VIM_FILE
  autocmd!
  autocmd FileType vim call g:UnmapDelimitMate()
augroup END


function! g:Meow()
  echom "Meow!"
endfunction


command! CDC cd %:p:h             " CDC = Change to Directory of Current file

" map <Leader>ew :e <C-R>=expand("%:p:h") . "/" <CR>
