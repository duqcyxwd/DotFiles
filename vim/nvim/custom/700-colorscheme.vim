" terminal color / italics finagling
let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
set termguicolors

" HighlightCharactersOver80 {{{1
" Highlight characters in column 81+ with a red background.
" (source: https://stackoverflow.com/a/235970/2338327)
function! s:HighlightCharactersOver80() abort
  highlight OverLength ctermbg=red ctermfg=white guibg=#592929
        \ | match OverLength /\%81v.\+/
endfunction

" These commands are run whenever the colorscheme is changed. They serve as
" hooks to further customize colors on top of the current colorschem.
"
" Hooks:
" 1. Italicize comments.
" 2. Highlight characters in column 81+ with a red background.
" TODO Add a toggle for this
" augroup ColorSchemeMods
"   autocmd!
"   autocmd ColorScheme *
"         \ highlight Comment cterm=italic
"         \ | call s:HighlightCharactersOver80()
" augroup END



let g:colorscheme_mode = v:null

" Dark {{{1
function! s:DarkMode() abort
  let g:colorscheme_mode = 'dark'

  " let g:airline_theme='onehalfdark'
  " let g:lightline.colorscheme = 'onehalfdark'
  " colorscheme onehalfdark

  " let ayucolor="mirage" " for mirage version of theme
  " let ayucolor="dark"   " for dark version of theme
  " colorscheme ayu

  colorscheme OceanicNext

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
endfunction
command! ToggleColorschemeMode call s:ToggleColorschemeMode()
nnoremap <leader>M :ToggleColorschemeMode<CR>

" Start in dark mode
call s:DarkMode()

