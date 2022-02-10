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
let g:which_key_map_space.c = { 'name' : '+COC' }
let g:which_key_map_space.f = { 'name' : '+file/format' }
let g:which_key_map_space.g = { 'name' : '+Git' }
let g:which_key_map_space.h = { 'name' : '+Help' }
let g:which_key_map_space.j = { 'name' : '+Jump' }
let g:which_key_map_space.p = { 'name' : '+Projects/Packages' }
let g:which_key_map_space.q = { 'name' : '+Quit' }
let g:which_key_map_space.r = { 'name' : '+Run' }
let g:which_key_map_space.s = { 'name' : '+Search' }
let g:which_key_map_space.S = { 'name' : '+Source' }
let g:which_key_map_space.t = { 'name' : '+Togglers' }
let g:which_key_map_space.w = { 'name' : '+Windows' }


function! s:key_map_cmd(mode, l1, l2, doc, cmd) abort
   echom 'let g:which_key_map_space.' .a:l1. '.' .a:l2. ' = "' .a:doc. '"'
   echom ''.a:mode.' <Space>'.a:l1.''.a:l2.' '.a:cmd
endfunction

function! s:nmap_cmdl(l1, l2, doc, cmd) abort
   call s:key_map_cmd('nmap', a:l1, a:l2, a:doc, ':'.a:cmd.'<CR>')
endfunction


" call s:nmap_cmdl('z', 'z', 'docs for zzzz', 'CocExplore')


function! s:map_cmd(mode, l1, l2, doc, cmd) abort
   exec 'let g:which_key_map_space.' .a:l1. '.' .a:l2. ' = "' .a:doc. '"'
   exec ''.a:mode.' <Space>'.a:l1.''.a:l2.' '.a:cmd
endfunction

function! s:nmap_cmd(l1, l2, doc, cmd) abort
   call s:map_cmd('nnoremap', a:l1, a:l2, a:doc, ':'.a:cmd.'<CR>')
endfunction



" [SpaceMapping] b+: Buffers {{{1
" ------------------------------------------------------------------------------

call s:nmap_cmd('b', 'D', 'Force delete this buffer',     ':bp<bar>sp<bar>bn<bar>bd!')
" call s:nmap_cmd('b', 'b', 'List all buffers', ':BuffergatorOpen ')
call s:nmap_cmd('b', 'b', 'List all buffers',             ':FFBuffers ')
call s:nmap_cmd('b', 'c', ':BufOnly Close other buffers', ':BufOnly')
call s:nmap_cmd('b', 'd', 'Delete this buffer',           ':call undoquit#SaveWindowQuitHistory()<CR>:Bclose')
call s:nmap_cmd('b', 'h', 'home',                         ':Startify')


" [SpaceMapping] c+: COC {{{1
" ------------------------------------------------------------------------------

if exists('g:plugs["coc.nvim"]')
    let which_key_map_space.c.e = 'Coc Explorer'
    nnoremap <silent><nowait> <Space>ce :CocCommand explorer<CR>

    " Show COC commands.
    nnoremap <silent><nowait> <space>cc  :<C-u>CocList commands<cr>

    " Show COC commands.
    nnoremap <silent><nowait> <space>cl  :CocList<cr>
    nnoremap <silent><nowait> <space>ci  :CocInfo<cr>

    " Show all diagnostics.
    nnoremap <silent><nowait> <space>ca  :<C-u>CocList diagnostics<cr>
    " Manage extensions.
    nnoremap <silent><nowait> <space>cE  :<C-u>CocList extensions<cr>


    " Find symbol of current document.
    nnoremap <silent><nowait> <space>co  :<C-u>CocList outline<cr>
    " Search workspace symbols.
    nnoremap <silent><nowait> <space>cs  :<C-u>CocList -I symbols<cr>


    " Do default action for next item.
    nnoremap <silent><nowait> <space>cj  :<C-u>CocNext<CR>
    " Do default action for previous item.
    nnoremap <silent><nowait> <space>ck  :<C-u>CocPrev<CR>
    " Resume latest coc list.
    nnoremap <silent><nowait> <space>cp  :<C-u>CocListResume<CR>
endif


" [SpaceMapping] f+: File/Format {{{1
" ------------------------------------------------------------------------------

call s:nmap_cmd('f', 'S', 'Save all files',                     ':wa!')
call s:nmap_cmd('f', 'e', 'Reopen current file',                ':mkview<CR>:e!<CR>:loadview')
call s:nmap_cmd('f', 'h', 'Open History files',                 ':FFHistory')
call s:nmap_cmd('f', 'r', 'Open Recent files',                  ':FFHistory')
call s:nmap_cmd('f', 't', '[format] Clean trailing space',      ':StripWhitespace')
call s:nmap_cmd('f', 's', 'Save current file',                  ':mkview<CR>:w')

" [SpaceMapping] g+: Git/Go {{{1
" ------------------------------------------------------------------------------

let which_key_map_space.g = { 'name': '+git' }

call s:nmap_cmd('g', 'a', 'Git action',        ':FzfPreviewGitActions')
call s:nmap_cmd('g', 'b', 'Git blame',         ':Git blame')
" call s:nmap_cmd('g', 'b', 'Git bale', ':Gina blame')
call s:nmap_cmd('g', 'c', 'Git commit',        ':Git commit -v')
call s:nmap_cmd('g', 'd', 'Git diff',          ':Gdiffsplit')
call s:nmap_cmd('g', 'm', 'Git Magit',         ':MagitOnly')
call s:nmap_cmd('g', 's', 'Git status',        ':FzfPreviewGitStatus')
call s:nmap_cmd('g', 'p', 'Git push',          ':Gina push')
call s:nmap_cmd('g', 'l', 'Git link open',     ':GBrowse')
call s:nmap_cmd('g', 'h', 'Git history view',  ':GV')
call s:nmap_cmd('g', 'v', 'Git history view ', ':GV')

nmap <Space>go <Plug>(openbrowser-smart-search)
vmap <Space>go <Plug>(openbrowser-smart-search)

nmap <Space>gS :OpenBrowserSmartSearch -github <c-r><c-w><CR>
vmap <Space>gS y:OpenBrowserSmartSearch -github <c-r>0<CR>


" [SpaceMapping] h+: Help {{{1
" ------------------------------------------------------------------------------

command! -bar -bang IMaps  call fzf#vim#maps("i", <bang>0)',
command! -bar -bang VMaps  call fzf#vim#maps("v", <bang>0)',

call s:nmap_cmd('h', 'h', 'Fzf Help tag',       ':FFHelptags')
call s:nmap_cmd('h', 'm', '[normal] Key Maps',  ':FFMaps')
call s:nmap_cmd('h', 't', 'Telescope Help tag', ':Telescope help_tags')
call s:nmap_cmd('h', 'i', '[insert] Key Maps',  ':IMaps')
call s:nmap_cmd('h', 'v', '[visual] Key Maps',  ':VMaps')



" [SpaceMapping] j+: Jump {{{1
" ------------------------------------------------------------------------------

call s:nmap_cmd('j', 'i', 'Fzf Jump def', 'FFLines (def')
call s:nmap_cmd('j', 'I', 'Fzf Jump def in project', 'MyFzfAg \(def[n]? ')
call s:nmap_cmd('j', 't', 'Fzf Jump Tags', 'FFTags')
call s:nmap_cmd('b', 't', 'Fzf BTags', 'FFBTags')


" [SpaceMapping] p+: Project/Install {{{1
" ------------------------------------------------------------------------------
call s:nmap_cmd('p', 'f', 'Project files', ':FFFiles')
call s:nmap_cmd('p', 'i', 'Plug Install',  ':execute "tabnew\| PlugInstall"')
call s:nmap_cmd('p', 'u', 'Plug Update',   ':execute "tabnew\| PlugUpdate"')

" [SpaceMapping] q+: Quit {{{1
" ------------------------------------------------------------------------------
call s:nmap_cmd('q', 'q', 'Quit',             ':q')
call s:nmap_cmd('q', 'Q', 'Force Quit',       ':q')
call s:nmap_cmd('q', 'a', 'Quit all',         ':qa')
call s:nmap_cmd('q', 'A', 'Quit all (Force)', ':qa!')

" [SpaceMapping] r+: Run {{{1
" ------------------------------------------------------------------------------
call s:map_cmd('nnoremap', 'r', 'r', 'Run Current file',            ':call RunUsingCurrentFiletype()')
call s:map_cmd('nmap',     'r', 'l', 'Run Current line in neoterm', '<Plug>(neoterm-repl-send-line)<CR>')
call s:map_cmd('nmap',     'r', 't', 'Run in neoterm',              '<Plug>(neoterm-repl-send)<CR>')



" [SpaceMapping] s+: Search {{{1
" ------------------------------------------------------------------------------
let which_key_map_space.s.s = 'FZF Search Current Files'
nnoremap <Space>ss :Telescope current_buffer_fuzzy_find<CR>
" nnoremap <Space>ss :FFBLines<CR>
nnoremap <Space>sS :execute ':FFBLines ' . expand('<cWORD>')<CR>
vnoremap <Space>ss y:FFBLines <C-R>=escape(@",'/\()')<CR><CR>

let which_key_map_space.s.r = "FZF <Rg> Search Current Project"
nnoremap <Space>sr :FFRg<CR>
nnoremap <Space>sR :execute ':FFRg ' . expand('<cWORD>')<CR>
vnoremap <Space>sr y:FFRg <C-R>=escape(@",'/\()')<CR><CR>

let which_key_map_space.s.p = 'FZF <Ag> Search Current Project'
" nnoremap <Space>sp :Ag '--hidden'<CR>
nnoremap <Space>sp :MyFzfAg <CR>
nnoremap <Space>sP :execute ':MyFzfAg ' . expand('<cWORD>')<CR>
vnoremap <Space>sp y:MyFzfAg <C-R>=escape(@",'/\()')<CR><CR>

let which_key_map_space.s.b = 'FZF Search All Opened Buffers'
nnoremap <Space>sb :FFBLines<CR>
nnoremap <Space>sB :execute ':FFBLines ' . expand('<cWORD>')<CR>
vnoremap <Space>sb y:FFBLines <C-R>=escape(@",'/\()')<CR><CR>

call s:nmap_cmd('s', 'c', 'Search highlight clean', ':nohlsearch')


" [SpaceMapping] S+: Source {{{1
" ------------------------------------------------------------------------------
call s:nmap_cmd('s', 'v', 'Source vimrc',         ':so $XDG_CONFIG_HOME/nvim/init.vim')
call s:nmap_cmd('s', 'e', 'Source current file!', ':so %')

" [SpaceMapping] t+: Toggler {{{1
" ------------------------------------------------------------------------------

function! s:map_toggle_opt(letter, option, mode, doc) abort
   " Example:
   " let which_key_map_space.t.c = "Toggle line cursorcolumn"
   " nnoremap <Space>tc :setlocal cursorcolumn!<CR>:setlocal cursorcolumn?<CR>
   "
   exe 'let g:which_key_map_space.t.' .a:letter ' = "' .a:doc '"'
   exe 'nnoremap <Space>t'.a:letter.' :'.a:mode.' '.a:option.'!<CR>:'.a:mode.' '.a:option.'?<CR>'
endfunction

call s:map_toggle_opt('l', 'cursorline',     'setlocal', 'Toggle line cursorline')
call s:map_toggle_opt('c', 'cursorcolumn',   'setlocal', 'Toggle line cursorcolumn')
call s:map_toggle_opt('n', 'number',         'set',      'Toggle line number ')
call s:map_toggle_opt('r', 'relativenumber', 'set',      'Toggle relative line number')
call s:map_toggle_opt('w', 'wrap',           'set',      'Toggle line wrap')
call s:map_toggle_opt('h', 'hlsearch',       'set',      'Toggle highlight matches')
call s:map_toggle_opt('i', 'list',           'set',      'Toggle invisible char (set list)')


call s:nmap_cmd('t', 's', 'Toggle Trailling whitespace indicator', ':ToggleWhitespace')
call s:nmap_cmd('t', 'z', 'Toggle Trailling whitespace indicator', ':ToggleWhitespace')
call s:nmap_cmd('t', 'S', 'Toggle Strip Whitespace On Save',       ':ToggleStripWhitespaceOnSave')
call s:nmap_cmd('t', 'M', 'ToggleColorschemeMode',                 ':ToggleColorschemeMode')
call s:nmap_cmd('t', 'm', 'ToggleColorschemeMode',                 ':ToggleColorschemeMode')
call s:nmap_cmd('t', 't', 'call ToggleTextWidth()',                ':Toggle 80 text width')
call s:nmap_cmd('t', 'T', 'TagbarToggle',                          ':TagbarToggle')
" call s:nmap_cmd('t', 't', 'call ToggleHighlightCharacterOver80()',       ':Toggle 80 text width')

" Fold method {{{2
let g:which_key_map_space.t.f = { 'name' : 'Fold+' }

let which_key_map_space.t.f.l = '[loop] foldmethod'
nnoremap <Space>tfl :call LoopFoldMethod()<CR>:set foldmethod<CR>zv

let which_key_map_space.t.f.m = 'change foldmethod to marker'
nnoremap <Space>tfm :set foldmethod=marker<CR>zv

let which_key_map_space.t.f.s = 'change foldmethod to syntax'
nnoremap <Space>tfs :set foldmethod=syntax<CR>zv

let which_key_map_space.t.f.i = 'change foldmethod to indent'
nnoremap <Space>tfi :set foldmethod=indent<CR>zv


let which_key_map_space.t.f.c = 'Toggle Fold Column'
nnoremap <Space>tfc :call ToggleFoldColumn()<CR>

nmap \s :call LoopFoldMethod()<CR>:set foldmethod<CR>zv
"}}}2


" [SpaceMapping] w+: Window {{{1
" ------------------------------------------------------------------------------


call s:nmap_cmd('w', 'c', 'Window Close',            ':call undoquit#SaveWindowQuitHistory()<CR>:close')
call s:nmap_cmd('w', 'h', 'Window Hide',             ':call undoquit#SaveWindowQuitHistory()<CR>:close')
call s:nmap_cmd('w', 'd', 'Window Close and Delete', ':call undoquit#SaveWindowQuitHistory()<CR>:bd!')
call s:nmap_cmd('w', 'u', 'Undoquit Window',         ':Undoquit')
call s:nmap_cmd('w', 'q', 'Write and quit',          ':wq')


let which_key_map_space.w.w = 'VimWiki Index Page'
" nmap <Space>ww <Plug>VimwikiIndex
nnoremap <Space>ww :e ~/vimwiki/index.md<CR>


let which_key_map_space.w.o = 'FZF search current file'
nnoremap <silent> <Space>wo :execute ':MyFzfFiles ' . SafeFzfQuery(expand('<cfile>'))<CR>
vnoremap <silent> <Space>wo y:execute ':MyFzfFiles ' . SafeFzfQuery(@*)<CR>



" [SpaceMapping] z+: WIP {{{1
" ------------------------------------------------------------------------------
nnoremap <silent> <Space>zz :Goyo<CR>
nnoremap <silent> <Space>zm :ZoomToggle<CR>
nnoremap <silent> <Space>zo :ZoomToggle<CR>

nnoremap <silent> <Space>zl :Limelight!!<CR>

" [SpaceMapping] +: Misc {{{1
" ------------------------------------------------------------------------------
" Search all open buffer
let which_key_map_space['Space'] = 'FZF Command Search'
nnoremap <Space><Space> :FFCommands<CR>

let which_key_map_space['\'] = 'FZF Command History Search'
nnoremap <silent> <Space>\ :FFHistory:<CR>

" https://stackoverflow.com/questions/51644477/reuse-vim-terminal-buffer
" Toggles the neoterm
let which_key_map_space["'"] = 'Toggles the neoterm'
nnoremap <silent> <Space>' :above Ttoggle<CR>

let which_key_map_space['<Tab>'] = 'last buffer'
nnoremap <silent> <Space><Tab> :e#<cr>


" [Other Keys] mappings {{{1
"--------------------------------------------------------------------------

" https://stackoverflow.com/a/24717020/2727296
" :help cmdline-special.

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

" Other random / WIP {{{1
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
"
