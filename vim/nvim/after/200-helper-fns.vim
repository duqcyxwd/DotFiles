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
" Standalone Functions {{{1
function! g:SafeFzfQuery(str) "{{{1
  return substitute(a:str, "[\"\n\/\.\\][()#*-]", " ", "g")
endfunc


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
