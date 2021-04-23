" vim:fdm=marker:ts=4:sw=4:et:
"  __  __                   _
" |  \/  | __ _ _ __  _ __ (_)_ __   __ _
" | |\/| |/ _` | '_ \| '_ \| | '_ \ / _` |
" | |  | | (_| | |_) | |_) | | | | | (_| |
" |_|  |_|\__,_| .__/| .__/|_|_| |_|\__, |
"              |_|   |_|            |___/
" Mapping

" Debug keys
" verbose map <Space>s

" SpaceMapping: Section 0 {{{1
" ------------------------------------------------------------------------------

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
let g:which_key_map_space.t = { 'name' : '+Togglers' }

" SpaceMapping: Section f+: File {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.f.s = "Save current file"
nnoremap <Space>fs :w!<CR>

let which_key_map_space.f.S = "Save all files"
nnoremap <Space>fS :wa!<CR>

let g:which_key_map_space.f.a = "[operator] vim-easy-align {motion}"
map <Space>fa <Plug>(EasyAlign)


let g:which_key_map_space.f.e = "Reopen current file"
nnoremap <Space>fS :e!<CR>


let which_key_map_space.f.r = "Open Recent files"
nnoremap <Space>fr :History<CR>

let which_key_map_space.f.r = "Open History files"
nnoremap <Space>fh :History<CR>

" SpaceMapping: Section h+: Help {{{1
" ------------------------------------------------------------------------------
"['FzfHelpTags SpaceVim', 'find-SpaceVim-help']

let g:which_key_map_space.h.t = "Fzf Help tag"
nnoremap <silent> <Space>ht :Helptags<CR>

let g:which_key_map_space.h.m = "FZF Key Maps"
nnoremap <silent> <Space>hm :Maps<CR>


" SpaceMapping: Section q+: Quit {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.q.q = "Quit"
nnoremap <Space>qq :q<CR>
let which_key_map_space.q.a = "Force Quit"
nnoremap <Space>qa :q!<CR>

" SpaceMapping: Section p+: Project {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.p.f = "Project files"
nnoremap <Space>pf :Files<CR>

" SpaceMapping: Section b+: Buffers {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.b.d = "delete this buffer"
nnoremap <Space>bd :bdelete<CR>

let which_key_map_space.b.D = "Force delete this buffer"
nnoremap <Space>bD :bdelete!<CR>
let which_key_map_space.b.b = "List all buffers"
nnoremap <Space>bb :Buffers<CR>

" SpaceMapping: Section s+: Search {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.s.b = "FZF Search Current Files"
nnoremap <Space>sb :lines<CR>
let which_key_map_space.s.l = "FZF <Rg> Search Current Project"
nnoremap <Space>sl :Rg<CR>
let which_key_map_space.s.p = "FZF <ag> Search Current Project"
nnoremap <Space>sp :Ag<CR>
let which_key_map_space.s.s = "FZF Search All Open Files"
nnoremap <Space>ss :Blines<CR>

" SpaceMapping: Section t+: Toggler {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.t.n = "Toggle line number"
nnoremap <Space>tn :setlocal number!<CR>:setlocal number?<CR>
let which_key_map_space.t.r = "Toggle relative line number"
nnoremap <Space>tr :setlocal rnu!<CR>:setlocal rnu?<CR>


" SpaceMapping: Section w+: Window {{{1
" ------------------------------------------------------------------------------
"
let which_key_map_space.w.c = "Window Close"
nnoremap <Space>wc <C-w><C-c>

let which_key_map_space.w.d = "Window Delete"
nnoremap <Space>wd <C-w><C-c>

" SpaceMapping: Section +: Misc {{{1
" ------------------------------------------------------------------------------
" Search all open buffer
let which_key_map_space['Space'] = "FZF Command Search"
nnoremap <Space><Space> :Commands<CR>

let which_key_map_space['\'] = "FZF Command History Search"
nnoremap <silent> <Space>\ :History:<CR>


let which_key_map_space["'"] = "Terminal"
nnoremap <silent> <Space>' :sp term://zsh<CR>

let which_key_map_space['<Tab>'] = "last buffer"
nnoremap <silent> <Space><Tab> :e#<cr>

let which_key_map_space.s.c = "Search highlight Clean"
nnoremap <Space>sc :noh<CR>



" Section: Slash Key mappings {{{1
"--------------------------------------------------------------------------

nmap \w :setlocal wrap!<CR>:setlocal wrap?<CR>
nmap \l :set foldcolumn = 2<CR>
nmap \L :set foldcolumn = 0<CR>
nmap \s :set foldmethod = syntax<CR>
nmap \S :set foldmethod = marker<CR>


" Section: Other Key mappings {{{1
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

" Treat long lines as break lines (useful when moving around in them)
" map j gj
" map k gk

" Bash like keys for the command line
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-K> <C-U>

cnoremap <C-P> <Up>
cnoremap <C-N> <Down>

nnoremap <leader>ec :Defx ~/.nvim/custom/<CR>
nnoremap <leader>em :e! ~/.nvim/custom/400-mappings.vim<CR>
nnoremap <leader>ep :e! ~/.nvim/custom/100-plugins.vim<CR>
nnoremap <leader>eP :e! ~/.nvim/custom/500-plugins-config.vim<CR>
nnoremap <leader>ev :e! ~/.nvim/vimrc<CR>
nnoremap <leader>sv :source ~/.vimrc<CR>
