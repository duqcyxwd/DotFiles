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

" Random notes: <c-u> will clean command line
" <C-R><C-W> inserts the word object.
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
let g:which_key_map_space.j = { 'name' : '+Jump' }
let g:which_key_map_space.p = { 'name' : '+Projects/Packages' }
let g:which_key_map_space.q = { 'name' : '+Quit' }
let g:which_key_map_space.r = { 'name' : '+Run' }
let g:which_key_map_space.s = { 'name' : '+Search' }
let g:which_key_map_space.t = { 'name' : '+Togglers' }
let g:which_key_map_space.w = { 'name' : '+Windows' }

" [SpaceMapping] b+: Buffers {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.b.d = "Delete this buffer"
nnoremap <Space>bd :call undoquit#SaveWindowQuitHistory()<CR>:Bclose<CR>

let which_key_map_space.b.D = "Force delete this buffer"
nnoremap <Space>bD :bp<bar>sp<bar>bn<bar>bd!<CR>

let which_key_map_space.b.b = "List all buffers"
nnoremap <Space>bb :BuffergatorOpen<CR>

let which_key_map_space.b.h = "home"
nnoremap <Space>bh :Startify<CR>

let which_key_map_space.b.c = ":BufOnly Close other buffers"
nnoremap <Space>bc :BufOnly<CR>
nnoremap <Space>bC :BufOnly<CR>

" [SpaceMapping] f+: File/Format {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.f.s = "Save current file"
nnoremap <Space>fs :mkview<CR>:w<CR>

let which_key_map_space.f.S = "Save all files"
nnoremap <Space>fS :wa!<CR>

let g:which_key_map_space.f.a = "[operator] vim-easy-align {motion}"
map <Space>fa <Plug>(EasyAlign)

let g:which_key_map_space.f.e = "Reopen current file"
nnoremap <Space>fe :mkview<CR>:e!<CR>:loadview<CR>

let which_key_map_space.f.r = "Open Recent files"
nnoremap <Space>fr :FFHistory<CR>

let which_key_map_space.f.h = "Open History files"
nnoremap <Space>fh :FFHistory<CR>

let which_key_map_space.f.t = "[format] Clean trailing space"
nnoremap <Space>ft :StripWhitespace<CR>

" [SpaceMapping] g+: Git/Go {{{1
" ------------------------------------------------------------------------------

let which_key_map_space.g = {
      \ 'name': '+git',
      \ 'A': 'git add --all',
      \ 'b': 'git blame',
      \ 'c': 'git commit',
      \ 'd': 'git diff',
      \ 'h': 'git history (GV)',
      \ 'l': 'git open link (GBrwose)',
      \ 'm': '[WIP]Vimagit',
      \ 'o': 'git open ** under cursor',
      \ 'p': 'git gina Patch',
      \ 'P': 'git push',
      \ 's': 'git status',
      \ 'S': 'Search ** in Github',
      \ 'v': 'browse git commits',
      \ }

      " \ 'g': 'git grep',
      " \ 'i': 'git init',
      " \ 'd': 'git diff',
      " \ 's': 'git status',

" let g:magit_show_magit_mapping='<Space>gM'
nnoremap <Space>gA :Git add --all<CR>
nnoremap <Space>gm :MagitOnly<CR>
nnoremap <Space>gb :Gblame<CR>
nnoremap <Space>gd :Gdiffsplit<CR>
nnoremap <Space>gc :Git commit -v<CR>
nnoremap <Space>gs :Gina status<CR>
nnoremap <Space>gP :Gina push<CR>


nnoremap <Space>gl :GBrowse<CR>
nnoremap <Space>gh :GV<CR>

nmap <Space>go <Plug>(openbrowser-smart-search)
vmap <Space>go <Plug>(openbrowser-smart-search)

nmap <Space>gS :OpenBrowserSmartSearch -github <c-r><c-w><CR>
vmap <Space>gS y:OpenBrowserSmartSearch -github <c-r>0<CR>


" [SpaceMapping] h+: Help {{{1
" ------------------------------------------------------------------------------

let g:which_key_map_space.h.t = "Fzf Help tag"
let g:which_key_map_space.h.h = "Fzf Help tag"
nnoremap <silent> <Space>ht <CMD>Telescope help_tags<CR>
nnoremap <silent> <Space>hh :FFHelptags<CR>

let g:which_key_map_space.h.m = "[normal] Key Maps"
nnoremap <silent> <Space>hm :FFMaps<CR>

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



" [SpaceMapping] j+: Jump {{{1
" ------------------------------------------------------------------------------

let g:which_key_map_space.j.i = "Fzf Jump def"
nnoremap <silent> <Space>ji :FFLines (def<CR>
" nnoremap <silent> <Space>ji :FFBLines (def<CR>

let g:which_key_map_space.j.I = "Fzf Jump def in project"
nnoremap <silent> <Space>jI :MyFzfAg \(def[n]? <CR>

let g:which_key_map_space.j.t = "Fzf Jump Tags"
nnoremap <silent> <Space>jt :FFTags<CR>


let g:which_key_map_space.b.t = "Fzf BTags"
nnoremap <silent> <Space>bt :FFBTags<CR>




" [SpaceMapping] p+: Project/Install {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.p.f = "Project files"
nnoremap <Space>pf :FFFiles<CR>

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
let which_key_map_space.q.a = "Quit all"
nnoremap <Space>qa :qa<CR>
let which_key_map_space.q.A = "Force Quit all"
nnoremap <Space>qA :qa!<CR>

" [SpaceMapping] r+: Run {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.r.r = "Run Current file"
nnoremap <Space>rr :call RunUsingCurrentFiletype()<CR>

let which_key_map_space.r.l = "Run Current line in neoterm"
nmap <Space>rl <Plug>(neoterm-repl-send-line)

let which_key_map_space.r.t = "Run in neoterm"
nmap <Space>rt <Plug>(neoterm-repl-send)
xmap <Space>rt <Plug>(neoterm-repl-send)

" nmap gt <Plug>(neoterm-repl-send)
" xmap gt <Plug>(neoterm-repl-send)
" nmap gtt <Plug>(neoterm-repl-send-line)

" [SpaceMapping] s+: Search {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.s.b = "FZF Search Current Files"
nnoremap <Space>sb :FFBLines<CR>

let which_key_map_space.s.l = "FZF <Rg> Search Current Project"
nnoremap <Space>sl :FFRg<CR>

let which_key_map_space.s.p = "FZF <Ag> Search Current Project"
" nnoremap <Space>sp :Ag '--hidden'<CR>
nnoremap <Space>sp :MyFzfAg <CR>

let which_key_map_space.s.s = "FZF Search All Open Files"
nnoremap <Space>ss :Telescope current_buffer_fuzzy_find<CR>

" [SpaceMapping] t+: Toggler {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.t.n = "Toggle line number"
nnoremap <Space>tn :setlocal number!<CR>:setlocal number?<CR>

let which_key_map_space.t.r = "Toggle relative line number"
nnoremap <Space>tr :setlocal rnu!<CR>:setlocal rnu?<CR>

let which_key_map_space.t.s = "Toggle auto strip whitespace"
nnoremap <Space>ts :ToggleStripWhitespaceOnSave<CR>


let which_key_map_space.t.w = "Toggle word wrap"
nnoremap <Space>tw :setlocal wrap!<CR>:setlocal wrap?<CR>

let which_key_map_space.t.t = "Toggle 80 text width"
nnoremap <Space>tt :call ToggleTextWidth()<CR>
" nnoremap <Space>tt :call ToggleHighlightCharacterOver80()<CR>


let which_key_map_space.t.T = "Toggle TagBar"
nnoremap <Space>tT :TagbarToggle<CR>


let which_key_map_space.t.l = "Toggle list for special char "
nnoremap <Space>tl :set list!<CR>
" nnoremap <Space>tt :call ToggleHighlightCharacterOver80()<CR>

let which_key_map_space.t.M = "Toggle ColorScheme"
nnoremap <Space>tM :ToggleColorschemeMode<CR>

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

let which_key_map_space.w.C = "Window Close all except current one"
nnoremap <Space>wC :call undoquit#SaveWindowQuitHistory()<CR>:only<CR>
" wo is used by wiki open
" nnoremap <Space>wo :call undoquit#SaveWindowQuitHistory()<CR>:only<CR>

let which_key_map_space.w.h = "Window Hide"
nnoremap <Space>wh :call undoquit#SaveWindowQuitHistory()<CR>:close<CR>
let which_key_map_space.w.d = "Window Delete and Close"
nnoremap <Space>wd :call undoquit#SaveWindowQuitHistory()<CR>:bd!<CR>

let which_key_map_space.w.u = "Undo quit window"
nnoremap <Space>wu :Undoquit<CR>

let which_key_map_space.w.w = "VimWiki Index Page"
nmap <Space>ww <Plug>VimwikiIndex

" [SpaceMapping] z+: WIP {{{1
" ------------------------------------------------------------------------------
nnoremap <silent> <Space>zz :Goyo<cr>
nnoremap <silent> <Space>zm :ZoomToggle<cr>
nnoremap <silent> <Space>zo :ZoomToggle<cr>

nnoremap <silent> <Space>zl <Plug>(Limelight)

" [SpaceMapping] +: Misc {{{1
" ------------------------------------------------------------------------------
" Search all open buffer
let which_key_map_space['Space'] = "FZF Command Search"
nnoremap <Space><Space> :FFCommands<CR>

let which_key_map_space['\'] = "FZF Command History Search"
nnoremap <silent> <Space>\ :FFHistory:<CR>

" https://stackoverflow.com/questions/51644477/reuse-vim-terminal-buffer
" Toggles the neoterm
let which_key_map_space["'"] = "Toggles the neoterm"
nnoremap <silent> <Space>' :above Ttoggle<CR>

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
nnoremap <Space>ep :e!   ~/.config/vim/custom/100-plugins.vim<CR>
nnoremap <Space>ef :e!   ~/.config/vim/custom/300-filetypes.vim<CR>
nnoremap <Space>eP :e!   ~/.config/vim/custom/500-plugins-config.vim<CR>
nnoremap <Space>em :e!   ~/.config/vim/custom/400-mappings.vim<CR>
nnoremap <Space>er :e!   ~/.config/vim/custom/999-playground.vim<CR>
nnoremap <Space>ev :e!   ~/.config/vim/vimrc<CR>
nnoremap <Space>et :e!   ~/vimwiki/TODO.md<CR>
" nnoremap <silent><buffer> <C-j> i<CR><Esc>
" unmap <C-j>
" nunmap <C-j>

nnoremap gj <C-W>j
nnoremap gk <C-W>k
nnoremap gh <C-W>h
nnoremap gl <C-W>l

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" vim-visual-multi, return to last selection
nmap gV <Plug>(VM-Reselect-Last)

" Useful thing from vim-sexp
" Safe full line delete
"
" nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
" omap <silent><buffer> $     <Plug>(sexp_move_to_end_of_line_with_form)
" nmap <silent><buffer> <C-k> d$

" }}}1

" Other random / WIP
" --------------------------------------------------------------------------------
"
"  Comments
" nnoremap gca i#_<esc>

" Fix paste
" p will not overwrite register
" https://vim.fandom.com/wiki/Replace_a_word_with_yanked_text
" xnoremap p "_dP
xnoremap <silent> p p:let @+=@0<CR>:let @"=@0<CR>

function! OpenGithubPlugin()
  " let s:uri = matchstr(getline("."), "'[a-z]*'")
  let s:uri = matchstr(getline("."), "[0-9a-z\-\_\.]*\/[0-9a-z\-\_\.]*")
  let s:uri = "https://www.github.com/".s:uri
  if s:uri != ""
    silent exec "!open '".s:uri."'"
    :redraw!
  else
    echo "no packages found"
  endif
endfunction

" Plugin open
nnoremap <Space>po :call OpenGithubPlugin()<CR>

nnoremap j gj
nnoremap k gk

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
