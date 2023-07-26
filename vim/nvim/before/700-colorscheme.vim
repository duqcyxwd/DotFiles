let g:colorschema_loaded = 1

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

  " Visual Selection highlight
  " highlight Visual  guifg=White
  " highlight Visual  guibg=Grey50
  let g:airline_theme='dracula'

endfunction

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

endfunction


" VIM Multi colr
fun! s:MultiThemesDark()  "{{{1
  " Modified from
  " https://github.com/mg979/vim-visual-multi/blob/master/autoload/vm/themes.vim
  hi! VM_Extend ctermbg=24                   guibg=#005f87
  hi! VM_Cursor ctermbg=31    ctermfg=237    guibg=#0087af    guifg=#87dfff
  hi! VM_Insert ctermbg=239                  guibg=#4c4e50
  hi! VM_Mono   ctermbg=167   ctermfg=253    guibg=#df5f5f    guifg=#dadada cterm=bold term=bold gui=bold

endfun

fun! s:MultiThemesLight() "{{{1
  hi! VM_Extend ctermbg=143   ctermfg=0      guibg=darkkhaki  guifg=black
  hi! VM_Cursor ctermbg=64    ctermfg=186    guibg=olivedrab  guifg=khaki
  hi! VM_Insert ctermbg=239                  guibg=#4c4e50
  hi! VM_Mono   ctermbg=131   ctermfg=235    guibg=#AF5F5F    guifg=#262626

endfun

" VM Theme Toggle {{{1
function! s:ToggleColorschemeMode() abort
  if g:colorscheme_mode ==# 'dark'
    call s:LightMode()
    call s:MultiThemesDark()
  else
    call s:DarkMode()
    call s:MultiThemesLight()
  endif
 syntax enable               " Enable syntax highlighting
endfunction

command! LightMode             call s:LightMode()
command! DarkMode              call s:DarkMode()
command! ToggleColorschemeMode call s:ToggleColorschemeMode()
command! MultiThemeDark        call s:MultiThemesDark()

" One time setup {{{1
"--------------------------------------------------------------------------------
" Start in dark mode
let g:colorscheme_mode = 'dark'
call s:DarkMode()
call s:MultiThemesDark()


set termguicolors


" Color setting for better white space
" I am using better-whitespace
" but the color setting in that plugin is not working
" let g:better_whitespace_ctermcolor='LightYellow'
" let g:better_whitespace_guicolor='#6272a4'
highlight ExtraWhitespace ctermbg=LightYellow guibg=#6272a4

syntax enable               " Enable syntax highlighting
syntax on                   " syntax for a lot of stuff
