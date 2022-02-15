" terminal color / italics finagling
let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"

let g:colorscheme_mode = v:null


" 1. Italicize comments.
augroup ColorSchemeMods
  autocmd!
  autocmd ColorScheme *
        \ highlight Comment cterm=italic gui=italic
augroup END

" Dark {{{1
function! s:DarkMode() abort
  let g:colorscheme_mode = 'dark'

  " let g:airline_theme='onehalfdark'
  " let g:lightline.colorscheme = 'onehalfdark'
  " colorscheme onehalfdark

  " let ayucolor="mirage" " for mirage version of theme
  " let ayucolor="dark"   " for dark version of theme
  " colorscheme ayu

  " let g:oceanic_bold = 0
  " colorscheme OceanicNext
  " let g:airline_theme='oceanicnext'

  " Hack: Call colorschem twice to get correct corlor
  colorscheme dracula
  colorscheme dracula
  let g:airline_theme='dracula'

endfunction
command! DarkMode call s:DarkMode()

" Light {{{1
function! s:LightMode() abort
  let g:colorscheme_mode = 'light'

  " github theme
  let g:github_colors_soft = 1
  let g:github_colors_block_diffmark = 1
  let g:lightline.colorscheme = 'github'
  let g:airline_theme = "github"
  set background=light
  colorscheme github


  " let ayucolor="light"   " for dark version of theme
  " colorscheme ayu

  " colorscheme onehalflight
  " let g:airline_theme='onehalflight'
endfunction
command! LightMode call s:LightMode()

" Toggle {{{1
function! s:ToggleColorschemeMode() abort
  if g:colorscheme_mode ==# 'dark'
    call s:LightMode()
  else
    call s:DarkMode()
  endif
 syntax enable               " Enable syntax highlighting
endfunction
command! ToggleColorschemeMode call s:ToggleColorschemeMode()

" Start in dark mode
call s:DarkMode()


set termguicolors

syntax enable               " Enable syntax highlighting
syntax on                   " syntax for a lot of stuff
