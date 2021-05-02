" vim:fdm=marker:ts=4:sw=4:et:
"  __  __                   _
" |  \/  | __ _ _ __  _ __ (_)_ __   __ _
" | |\/| |/ _` | '_ \| '_ \| | '_ \ / _` |
" | |  | | (_| | |_) | |_) | | | | | (_| |
" |_|  |_|\__,_| .__/| .__/|_|_| |_|\__, |
"              |_|   |_|            |___/
" Mapping

" Debug keys
noremap <Space>hk :verbose map

" [SpaceMapping] 00: Leader keymap setting {{{1
" ------------------------------------------------------------------------------

" remap leader key to ,
let mapleader = ","
let g:mapleader = ","
let g:maplocalleader = ","
let maplocalleader = ","

nnoremap <silent> <leader>      :<c-u>WhichKey ','<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey '\'<CR>
nnoremap <silent> <Space>       :<c-u>WhichKey '<Space>'<CR>
vnoremap <silent> <leader>      :<c-u>WhichKeyVisual ','<CR>
vnoremap <silent> <localleader> :<c-u>WhichKeyVisual '\'<CR>
vnoremap <silent> <Space>       :<c-u>WhichKeyVisual '<Space>'<CR>

" WIP
vnoremap <silent> g             :<c-u>WhichKeyVisual 'g'<CR>

call which_key#register(',', "g:which_key_map_leader")
call which_key#register('\', "g:which_key_map_localleader")
call which_key#register('<Space>', "g:which_key_map_space")
call which_key#register('g', "g:which_key_map_g")

let g:which_key_map_leader = {}
let g:which_key_map_localleader = {}
let g:which_key_map_space = {}
let g:which_key_map_g = {}


let g:which_key_map_space.b = { 'name' : '+buffers' }
let g:which_key_map_space.f = { 'name' : '+file/format' }
let g:which_key_map_space.g = { 'name' : '+Git' }
let g:which_key_map_space.h = { 'name' : '+Help' }
let g:which_key_map_space.p = { 'name' : '+Projects/Packages' }
let g:which_key_map_space.q = { 'name' : '+Quit' }
let g:which_key_map_space.s = { 'name' : '+Search' }
let g:which_key_map_space.t = { 'name' : '+Togglers' }
let g:which_key_map_space.w = { 'name' : '+Windows' }

" [SpaceMapping] b+: Buffers {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.b.d = "delete this buffer"
nnoremap <Space>bd :call undoquit#SaveWindowQuitHistory()<CR>:bd<CR>

let which_key_map_space.b.D = "Force delete this buffer"
nnoremap <Space>bD :bp<bar>sp<bar>bn<bar>bd!<CR>

let which_key_map_space.b.b = "List all buffers"
nnoremap <Space>bb :Buffers<CR>

let which_key_map_space.b.h = "home"
nnoremap <Space>bh :Startify<CR>

" [SpaceMapping] f+: File/Format {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.f.s = "Save current file"
nnoremap <Space>fs :w!<CR>

let which_key_map_space.f.S = "Save all files"
nnoremap <Space>fS :wa!<CR>

let g:which_key_map_space.f.a = "[operator] vim-easy-align {motion}"
map <Space>fa <Plug>(EasyAlign)


let g:which_key_map_space.f.e = "Reopen current file"
nnoremap <Space>fe :e!<CR>


let which_key_map_space.f.r = "Open Recent files"
nnoremap <Space>fr :History<CR>

let which_key_map_space.f.h = "Open History files"
nnoremap <Space>fh :History<CR>

let which_key_map_space.f.t = "[format] Clean trailing space"
nnoremap <Space>ft :call StripWhitespace()<CR>

" [SpaceMapping] g+: Git/Go {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.g.M = "WIP Vimagit"
let g:magit_show_magit_mapping='<Space>gM'

let which_key_map_space.g.b = "Git Blame"
nnoremap <Space>gb :Gblame<CR>


let which_key_map_space.g.o = "Git Open Link (GBrwose)"
nnoremap <Space>go :GBrowse<CR>


" [SpaceMapping] h+: Help {{{1
" ------------------------------------------------------------------------------
"['FzfHelpTags SpaceVim', 'find-SpaceVim-help']

let g:which_key_map_space.h.t = "Fzf Help tag"
nnoremap <silent> <Space>ht :Helptags<CR>

let g:which_key_map_space.h.m = "[normal] Key Maps"
nnoremap <silent> <Space>hm :Maps<CR>

command! -bar -bang IMaps  call fzf#vim#maps("i", <bang>0)',
command! -bar -bang VMaps  call fzf#vim#maps("v", <bang>0)',

" Mapping selecting mappings
" nmap <leader><tab> <plug>(fzf-maps-n)
" xmap <leader><tab> <plug>(fzf-maps-x)
" omap <leader><tab> <plug>(fzf-maps-o)

let g:which_key_map_space.h.i = "[insert] Key Maps"
nnoremap <silent> <Space>hi :IMaps<CR>

let g:which_key_map_space.h.v = "[visual] Key Maps"
nnoremap <silent> <Space>hv :VMaps<CR>



" [SpaceMapping] p+: Project/Install {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.p.f = "Project files"
nnoremap <Space>pf :Files<CR>

let which_key_map_space.p.i = "Plug Install"
nnoremap <Space>pi :execute "tabnew\| PlugInstall"<CR>

let which_key_map_space.p.u = "Plug Update"
nnoremap <Space>pu :execute "tabnew\| PlugUpdate"<CR>

" [SpaceMapping] q+: Quit {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.q.q = "Quit"
nnoremap <Space>qq :q<CR>
let which_key_map_space.q.Q = "Force Quit"
nnoremap <Space>qQ :q!<CR>
let which_key_map_space.q.a = "Force Quit all"
nnoremap <Space>qa :qa!<CR>

" [SpaceMapping] s+: Search {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.s.b = "FZF Search Current Files"
nnoremap <Space>sb :lines<CR>
let which_key_map_space.s.l = "FZF <Rg> Search Current Project"
nnoremap <Space>sl :Rg<CR>
let which_key_map_space.s.p = "FZF <ag> Search Current Project"
nnoremap <Space>sp :Ag<CR>
let which_key_map_space.s.s = "FZF Search All Open Files"
nnoremap <Space>ss :BLines<CR>

" [SpaceMapping] t+: Toggler {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.t.n = "Toggle line number"
nnoremap <Space>tn :setlocal number!<CR>:setlocal number?<CR>

let which_key_map_space.t.r = "Toggle relative line number"
nnoremap <Space>tr :setlocal rnu!<CR>:setlocal rnu?<CR>

let which_key_map_space.t.s = "Toggle auto strip whitespace"
nnoremap <Space>ts :call ToggleAutoStripSpace()<CR>

let which_key_map_space.t.w = "Toggle word wrap"
nnoremap <Space>tw :setlocal wrap!<CR>:setlocal wrap?<CR>

let which_key_map_space.t.t = "Toggle 80 text width"
nnoremap <Space>tt :call ToggleTextWidth()<CR>
" nnoremap <Space>tt :call ToggleHighlightCharacterOver80()<CR>


let which_key_map_space.t.l = "Toggle list for special char "
nnoremap <Space>tl :set list!<CR>
" nnoremap <Space>tt :call ToggleHighlightCharacterOver80()<CR>

" WIP
let g:which_key_map_space.t.f = { 'name' : 'Fold+' }

let which_key_map_space.t.f.l = "[loop] foldmethod"
nnoremap <Space>tfl :call LoopFoldMethod()<CR>:set foldmethod<CR>zv

let which_key_map_space.t.f.m = "change foldmethod to marker"
nnoremap <Space>tfm :set foldmethod=marker<CR>zv

let which_key_map_space.t.f.s = "change foldmethod to syntax"
nnoremap <Space>tfs :set foldmethod=syntax<CR>zv

let which_key_map_space.t.f.c = "Toggle Fold Column"
nnoremap <Space>tfc :call ToggleFoldColumn()<CR>


nmap \s :call LoopFoldMethod()<CR>

" [SpaceMapping] w+: Window {{{1
" ------------------------------------------------------------------------------
" TODO Make this more powerful
let which_key_map_space.w.c = "Window Close"
nnoremap <Space>wc :call undoquit#SaveWindowQuitHistory()<CR>:close<CR>

let which_key_map_space.w.d = "Window Delete"
nnoremap <Space>wd :call undoquit#SaveWindowQuitHistory()<CR>:close<CR>

let which_key_map_space.w.u = "Undo quit window"
nnoremap <Space>wu :Undoquit<CR>

" [SpaceMapping] z+: WIP {{{1
" ------------------------------------------------------------------------------
nnoremap <silent> <Space>zz :Goyo<cr>


" [SpaceMapping] +: Misc {{{1
" ------------------------------------------------------------------------------
" Search all open buffer
let which_key_map_space['Space'] = "FZF Command Search"
nnoremap <Space><Space> :Commands<CR>

let which_key_map_space['\'] = "FZF Command History Search"
nnoremap <silent> <Space>\ :History:<CR>

" https://stackoverflow.com/questions/51644477/reuse-vim-terminal-buffer
let which_key_map_space["'"] = "Terminal"
nnoremap <silent> <Space>' :sp term://zsh<CR>

let which_key_map_space['<Tab>'] = "last buffer"
nnoremap <silent> <Space><Tab> :e#<cr>

let which_key_map_space.s.c = "Search highlight Clean"
nnoremap <Space>sc :noh<CR>


" [Slash Key] mappings {{{1
"--------------------------------------------------------------------------

"   call SpaceVim#mapping#space#def( 'nnoremap', ['t', 'T', 's'], "set foldmethod=syntax", 'Set foldmethod syntax', 1)
"   call SpaceVim#mapping#space#def( 'nnoremap', ['t', 'T', 'm'], "set foldmethod=marker", 'Set foldmethod marker', 1)
"   call SpaceVim#mapping#space#def( 'nnoremap', ['t', 'T', 'c'], "set foldcolumn=2", 'Set column to 2', 1)
"   call SpaceVim#mapping#space#def( 'nnoremap', ['t', 'T', 'C'], "set foldcolumn=0", 'Set column to 0', 1)

" [Other Keys] mappings {{{1
"--------------------------------------------------------------------------

" easy align: ga <Left>, Indent keep
" https://stackoverflow.com/a/24717020/2727296
" :help cmdline-special.

nnoremap <Space>es :so %<CR>|                   " Srouce current file
nnoremap <Space>sv :so ~/.config/vim/vimrc<CR>| " Srouce .vimrc

" Map Enter to folding
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

nnoremap <Space>ec :Defx ~/.config/vim/custom/<CR>
nnoremap <Space>em :e! ~/.config/vim/custom/400-mappings.vim<CR>
nnoremap <Space>er :e! ~/.config/vim/custom/999-random.vim<CR>
nnoremap <Space>ep :e! ~/.config/vim/custom/100-plugins.vim<CR>
nnoremap <Space>eP :e! ~/.config/vim/custom/500-plugins-config.vim<CR>
nnoremap <Space>ev :e! ~/.config/vim/vimrc<CR>
nnoremap <silent><buffer> <C-j> i<CR><Esc>

nnoremap gj <C-W>j
nnoremap gk <C-W>k
nnoremap gh <C-W>h
nnoremap gl <C-W>l

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" }}}1

" Other random / WIP
" --------------------------------------------------------------------------------
"
"  Comments
nnoremap <Space>cl i#_<esc>

" Some of them are used as object motions
" Bad idea, " is used for register
" vnoremap " c""<Esc>hp
" vnoremap ' c''<Esc>hp
" vnoremap ( c()<Esc>hp
vnoremap 9 c()<Esc>hp
" vnoremap [ c[]<Esc>hp
" vnoremap { c{}<Esc>hp


" Not vim stype but I am used to it. Similar to vscode, atom and sublime
" nnoremap <C-P> :Files<CR>
" https://www.javaer101.com/en/article/16363554.html
" nnoremap J :s/\s*$//<cr>J
" vnoremap J :s/\s*$//<cr>gvJ


" map <Leader>ew :e <C-R>=expand("%:p:h") . "/" <CR>


" nnoremap <leader>gb :Git blame<CR>
" nnoremap <leader>gc :Gina commit -v<CR>gg0i
" nnoremap <leader>gd :Gina diff<CR>
" nnoremap <leader>gD :Gina diff -w<CR>
" nnoremap <leader>gg :Gina grep<space>
" nnoremap <leader>gi :Gina init<CR>
" nnoremap <leader>gl :Gina log --graph --pretty=format:"%C(yellow)%h %ad%Cred%d %Creset%s%Cblue [%cn]" --decorate --all --date=short<CR><CR>
" nnoremap <leader>gp :Gina push<CR>
" nnoremap <leader>gs :Gina status<CR>
" nnoremap <leader>gS :Gina show<CR>
"
"
" let g:magit_show_help = 0
" let g:magit_toggle_help_mapping = '?'
" let g:magit_discard_untracked_do_delete=1
" let g:magit_show_magit_mapping='<leader>G'
"
"
" nnoremap <leader>wi :e ~/Sync/vimwiki/diary/<CR>
"
