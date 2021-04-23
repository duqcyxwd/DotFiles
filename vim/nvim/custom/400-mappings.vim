" Chuan's Mapping {{{1
" -----------------------------------------------------------------------------------------------------

" Debug keys
" verbose map <Space>s

" SpaceMapping: Section 0 {{{2
" -----------------------------------------------------------------------------------------------------

nnoremap <silent> <leader>      :<c-u>WhichKey ','<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey '\'<CR>
nnoremap <silent> <Space>       :<c-u>WhichKey '<Space>'<CR>
vnoremap <silent> <leader>      :<c-u>WhichKeyVisual ','<CR>
vnoremap <silent> <localleader> :<c-u>WhichKeyVisual '\'<CR>
vnoremap <silent> <Space>       :<c-u>WhichKeyVisual '<Space>'<CR>

call which_key#register(',', "g:which_key_map_leader")
call which_key#register('\', "g:which_key_map_localleader")
call which_key#register('<Space>', "g:which_key_map_space")

let g:which_key_map_leader = {}
let g:which_key_map_localleader = {}
let g:which_key_map_space = {}

let g:which_key_map_space.b = { 'name' : '+buffers' }
let g:which_key_map_space.f = { 'name' : '+file/format' }
let g:which_key_map_space.h = { 'name' : '+Help' }
let g:which_key_map_space.p = { 'name' : '+Projects/Packages' }
let g:which_key_map_space.q = { 'name' : '+Quit' }
let g:which_key_map_space.s = { 'name' : '+Search' }
let g:which_key_map_space.w = { 'name' : '+Windows' }

" SpaceMapping: Section f: File {{{2
" -----------------------------------------------------------------------------------------------------
let which_key_map_space.f.s = "Save current file"
nnoremap <Space>fs :w!<CR>

let which_key_map_space.f.S = "Save all files"
nnoremap <Space>fs :wa!<CR>

let g:which_key_map_space.f.a = "[operator] vim-easy-align {motion}"
map <Space>fs <Plug>(EasyAlign)


nnoremap <Space>fr :History<CR>
let which_key_map_space.f.r = "Open Recent files"

" SpaceMapping: Section q: Quit {{{2
" -----------------------------------------------------------------------------------------------------
let which_key_map_space.q.q = "Quit"
nnoremap <Space>qq :q<CR>
let which_key_map_space.q.a = "Force Quit"
nnoremap <Space>qa :q!<CR>

" SpaceMapping: Section p: Project {{{2
" -----------------------------------------------------------------------------------------------------
let which_key_map_space.p.f = "Project files"
nnoremap <Space>pf :Files<CR>

let which_key_map_space.b.d = "delete this buffer"
nnoremap <Space>bd :bdelete<CR>
let which_key_map_space.b.b = "List all buffers"
nnoremap <Space>bb :Buffers<CR>

" SpaceMapping: Section h: Help {{{2
" -----------------------------------------------------------------------------------------------------
"['FzfHelpTags SpaceVim', 'find-SpaceVim-help']

let g:which_key_map_space.h['<Space>'] = "Fzf Help tag"
nnoremap <silent> <Space>h<Space> :Helptags<CR>

let g:which_key_map_space.h.m = "FZF Key Maps"
nnoremap <silent> <Space>hm :Maps<CR>


" SpaceMapping: Section s: Search {{{2
" -----------------------------------------------------------------------------------------------------
let which_key_map_space.s.b = "FZF Search Current Files"
nnoremap <Space>sb :lines<CR>
let which_key_map_space.s.l = "FZF <Rg> Search Current Project"
nnoremap <Space>sl :Rg<CR>
let which_key_map_space.s.p = "FZF <ag> Search Current Project"
nnoremap <Space>sp :Ag<CR>
let which_key_map_space.s.s = "FZF Search All Open Files"
nnoremap <Space>ss :Blines<CR>

" SpaceMapping: Section +: Misc {{{2
" -----------------------------------------------------------------------------------------------------
" Search all open buffer
let which_key_map_space['Space'] = "FZF Command Search"
nnoremap <Space><Space> :Commands<CR>

let which_key_map_space['\'] = "FZF Command History Search"
nnoremap <silent> <Space>\ :History:<CR>

let which_key_map_space['<Tab>'] = "last buffer"
nnoremap <silent> <Space><Tab> :e#<cr>




" Section: Slash Key mappings {{{2
"--------------------------------------------------------------------------

nmap \w :setlocal wrap!<CR>:setlocal wrap?<CR>
nmap \l :set foldcolumn=2<CR>
nmap \L :set foldcolumn=0<CR>
nmap \s :set foldmethod=syntax<CR>
nmap \S :set foldmethod=marker<CR>


" Section: Other Key mappings {{{2
"--------------------------------------------------------------------------


" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)


nnoremap gj <C-W>j
nnoremap gk <C-W>k
nnoremap gh <C-W>h
nnoremap gl <C-W>l


nnoremap <silent><buffer> <C-j>           i<CR><Esc>
map <Leader>ew :e <C-R>=expand("%:p:h") . "/" <CR>


" Enter for following
nnoremap <CR> za                 

" Default Mapping {{{1
" -----------------------------------------------------------------------------------------------------
" Treat long lines as break lines (useful when moving around in them)
" map j gj
" map k gk

" Remap VIM 0 to first non-blank character
map 0 ^

" Bash like keys for the command line
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-K> <C-U>

cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

" Fast saving
" nmap <C-s> :w!<CR>
" nmap <C-x> :x!<CR>
" imap <C-s> <Esc><C-s>
" imap <C-x> <Esc><C-x>

" FIXME: <C-X> is actually the same key code as <C-x>! Apparently the terminal
" sends the same key code, so there is no way to differentiate. I'm just
" commenting this out for now.
"
" TODO: Either get rid of this (maybe I don't want it after all) or come up with
" a different keybinding.
"
" For when I'm feeling extra-confident: save commit and immediately git push
" nmap <silent> <C-X> :call SaveCommitAndPush()<CR>
" function! SaveCommitAndPush() abort
"   x!
"   Gina push
"   Gina status
" endfunction

" disable shortcut to go into Ex mode, which I never use
nnoremap Q <nop>

" the nuclear option - close everything without saving and quit
nnoremap <leader>Q :qa!<CR>

" i'm constantly accidentally typing :Q instead of :q when I want to quit
" let's just make :Q quit too
command! Q q

" Disable highlight
map <silent> <leader><CR> :noh<CR>

nnoremap <leader>ec :Defx ~/.vim/custom/<CR>
nnoremap <leader>em :e! ~/.vim/custom/400-mappings.vim<CR>
nnoremap <leader>ep :e! ~/.vim/custom/100-plugins.vim<CR>
nnoremap <leader>eP :e! ~/.vim/custom/500-plugins-config.vim<CR>
nnoremap <leader>ev :e! ~/.vimrc<CR>
nnoremap <leader>sv :source ~/.vimrc<CR>

" 'skip a line' variants of o/O
nnoremap <leader>o o<CR>
nnoremap <leader>O O<Esc>O
" append & prepend blank lines and go into insert mode
nnoremap <C-o> o<Esc>O

" like J (which appends next line to current line),
" but works the other way (appends current line to previous line)
nnoremap <Space>j kJ

