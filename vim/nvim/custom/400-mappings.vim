" vim:fdm=marker:ts=4:sw=4:et:
"  __  __                   _
" |  \/  | __ _ _ __  _ __ (_)_ __   __ _
" | |\/| |/ _` | '_ \| '_ \| | '_ \ / _` |
" | |  | | (_| | |_) | |_) | | | | | (_| |
" |_|  |_|\__,_| .__/| .__/|_|_| |_|\__, |
"              |_|   |_|            |___/
" Mapping

" [SpaceMapping] 00: Leader keymap setting {{{1
" ------------------------------------------------------------------------------

" remap leader key to ,
let g:mapleader = '\'
let g:maplocalleader = ','

"Random notes: <c-u> will clean command line
" <C-R><C-W> inserts the word object.
nnoremap <silent> <leader>      :<c-u>WhichKey '\'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey ','<CR>
nnoremap <silent> <Space>       :<c-u>WhichKey '<Space>'<CR>

vnoremap <silent> <leader>      :<c-u>WhichKeyVisual '\'<CR>
vnoremap <silent> <localleader> :<c-u>WhichKeyVisual ','<CR>
vnoremap <silent> <Space>       :<c-u>WhichKeyVisual '<Space>'<CR>

let g:which_key_vertical = 1

call which_key#register('\', "g:which_key_map_leader")
call which_key#register(',', "g:which_key_map_localleader")
call which_key#register('<Space>', "g:which_key_map_space")

let g:which_key_map_leader = {}
let g:which_key_map_localleader = {}
let g:which_key_map_space = {}

" call which_key#register('g', "g:which_key_map_g")
" let g:which_key_map_g = {}
" vnoremap <silent> g             :<c-u>WhichKeyVisual 'g'<CR>


let g:which_key_map_space.b = { 'name' : '+buffers' }
let g:which_key_map_space.c = { 'name' : '+COC' }
let g:which_key_map_space.e = { 'name' : '+Edit' }
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
let g:which_key_map_space.z = { 'name' : '+Mix' }


function! s:map_cmd_debug(mode, l1, l2, doc, cmd) abort "{{{2
   " echom 'let g:which_key_map_space.' .a:l1. '.' .a:l2. ' = "' .a:doc. '"'
   echom ''.a:mode.' <Space>'.a:l1.''.a:l2.' '.a:cmd
   map_cmd_2(mode, l1, l2, doc, cmd)
endfunction

function! s:nmap_cmd_debug(l1, l2, doc, cmd) abort "{{{2
   call s:map_cmd_debug('nmap', a:l1, a:l2, a:doc, ':'.a:cmd.'<CR>')
endfunction

function! s:map_cmd_2_debug(mode, l1, l2, doc, cmd) abort "{{{2
   echom 'let g:which_key_map_space.' .a:l1. '.' .a:l2. ' = "' .a:doc. '"'
   echom ''.a:mode.' <Space>'.a:l1.''.a:l2.' '.a:cmd
   call s:map_cmd_2(a:mode, a:l1, a:l2, a:doc, a:cmd)
endfunction


function! s:map_cmd_3_debug(mode, l1, l2, l3, doc, cmd) abort "{{{2
   echom 'let g:which_key_map_space.' .a:l1. '.' .a:l2. '.' .a:l3.' = "' .a:doc. '"'
   echom ''.a:mode.' <Space>'.a:l1.''.a:l2.''.a:l3.' '.a:cmd
   call s:map_cmd_3(a:mode, a:l1, a:l2, a:l3, a:doc, a:cmd)
endfunction


function! s:map_cmd_2(mode, l1, l2, doc, cmd) abort "{{{2
   exec 'let g:which_key_map_space.' .a:l1. '.' .a:l2. ' = "' .a:doc. '"'
   exec ''.a:mode.' <Space>'.a:l1.''.a:l2.' '.a:cmd
endfunction


function! s:map_cmd_3(mode, l1, l2, l3, doc, cmd) abort "{{{2
   exec 'let g:which_key_map_space.' .a:l1. '.' .a:l2. '.' .a:l3.' = "' .a:doc. '"'
   exec ''.a:mode.' <Space>'.a:l1.''.a:l2.''.a:l3.' '.a:cmd
endfunction

function! s:nmap_cmd_2(l1, l2, doc, cmd) abort "{{{2
   call s:map_cmd_2('nnoremap', a:l1, a:l2, a:doc, ''.a:cmd.'<CR>')
endfunction

function! s:vmap_cmd_2(l1, l2, doc, cmd) abort "{{{2
   call s:map_cmd_2('vnoremap', a:l1, a:l2, a:doc, ''.a:cmd.'<CR>')
endfunction


" [SpaceMapping] b+: Buffers {{{1
" ------------------------------------------------------------------------------

call s:nmap_cmd_2('b', 'D', 'Force delete this buffer', ':bp<bar>sp<bar>bn<bar>bd!')
" call s:nmap_cmd_2('b', 'b', 'List all buffers', ':BuffergatorOpen ')
call s:nmap_cmd_2('b', 'b', 'List all buffers',         ':FFBuffers ')
call s:nmap_cmd_2('b', 'c', 'Close other buffers',      ':BufOnly')
call s:nmap_cmd_2('b', 'd', 'Delete this buffer',       ':call undoquit#SaveWindowQuitHistory()<CR>:Bclose')
call s:nmap_cmd_2('b', 'h', 'home',                     ':Startify')
call s:nmap_cmd_2('b', 'n', 'Next buffer',              ':bnext')
call s:nmap_cmd_2('b', 'p', 'Previous buffer',          ':bprev')

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

call s:nmap_cmd_2('f', 'S', 'Save all files',                     ':wa!')
call s:nmap_cmd_2('f', 'h', 'Open History files',                 ':FFHistory')
call s:nmap_cmd_2('f', 'r', 'Open Recent files',                  ':FFHistory')
call s:nmap_cmd_2('f', 't', '[format] Clean trailing space',      ':StripWhitespace')
call s:nmap_cmd_2('f', 's', 'Save current file',                  ':mkview<CR>:w')


call s:map_cmd_2('nnoremap', 'w', 'o', 'FZF Open Files', ":<C-U>execute ':MyFzfFiles' SafeFzfQuery(GetCurrentWord('n'))<CR>")
call s:map_cmd_2('vnoremap', 'w', 'o', 'FZF Open Files', ":<C-U>execute ':MyFzfFiles' SafeFzfQuery(GetCurrentWord('v'))<CR>")

" Check whne to use SafeFzfQuery
" let which_key_map_space.w.o = 'FZF search current file'
" nnoremap <silent> <Space>wo :execute  ':MyFzfFiles ' . SafeFzfQuery(expand('<cfile>'))<CR>
" vnoremap <silent> <Space>wo y:execute ':MyFzfFiles ' . SafeFzfQuery(@*)<CR>


" [SpaceMapping] g+: Git/Go {{{1
" ------------------------------------------------------------------------------

call s:nmap_cmd_2('g', 'a', 'Git action',        ':FzfPreviewGitActions')
call s:nmap_cmd_2('g', 'b', 'Git blame',         ':Git blame')
" call s:nmap_cmd_2('g', 'b', 'Git bale', ':Gina blame')
call s:nmap_cmd_2('g', 'c', 'Git commit',        ':Git commit -v')
call s:nmap_cmd_2('g', 'd', 'Git diff',          ':Gdiffsplit')
call s:nmap_cmd_2('g', 'm', 'Git Magit',         ':MagitOnly')
call s:nmap_cmd_2('g', 's', 'Git status',        ':FzfPreviewGitStatus')
call s:nmap_cmd_2('g', 'p', 'Git push',          ':Gina push')
call s:nmap_cmd_2('g', 'l', 'Git link open',     ':GBrowse')
call s:nmap_cmd_2('g', 'h', 'Git history view',  ':GV')
call s:nmap_cmd_2('g', 'v', 'Git history view ', ':GV')


call s:map_cmd_2('nmap', 'g', 'o', 'Search and open in browser ', '<Plug>(openbrowser-smart-search)')
call s:map_cmd_2('vmap', 'g', 'o', 'Search and open in browser ', '<Plug>(openbrowser-smart-search)')
call s:map_cmd_2('nmap', 'g', 'x', 'Search and open in browser ', '<Plug>(openbrowser-smart-search)')
call s:map_cmd_2('vmap', 'g', 'x', 'Search and open in browser ', '<Plug>(openbrowser-smart-search)')
call s:map_cmd_2('nmap', 'g', 'S', 'Search and open in Github ',  ':OpenBrowserSmartSearch -github <C-R><C-W><CR>')
call s:map_cmd_2('vmap', 'g', 'S', 'Search and open in Github ',  'y:OpenBrowserSmartSearch -github <C-R>0<CR>')


" [SpaceMapping] h+: Help {{{1
" ------------------------------------------------------------------------------

command! -bar -bang IMaps  call fzf#vim#maps("i", <bang>0)',
command! -bar -bang VMaps  call fzf#vim#maps("v", <bang>0)',

" call s:nmap_cmd_2('h', 't', 'Help tag', ':FFHelptags')
call s:nmap_cmd_2('h', 't', 'Help tag', ':FzfLua help_tags ')
call s:nmap_cmd_2('h', 'm', 'Man Page', ':FzfLua man_pages')


" Debug keys
" noremap <Space>hk :verbose map

let g:which_key_map_space.h.k = { 'name' : '+Kemap' }

call s:map_cmd_3('nnoremap', 'h', 'k', 'v', '[visual] Key Maps', ':VMaps<CR>')
call s:map_cmd_3('nnoremap', 'h', 'k', 'i', '[insert] Key Maps', ':IMaps<CR>')
call s:map_cmd_3('nnoremap', 'h', 'k', 'm', '[normal] Key Maps', ':FFMaps<CR>')
call s:map_cmd_3('nnoremap', 'h', 'k', 'd', 'Debug Key Maps',    ':verbose map ')
nnoremap <Space>hkD :execute "enew\| pu=execute('verbos map')"


call s:nmap_cmd_2('p', 'u', 'Plug Update',   ':execute "tabnew\| PlugUpdate"')

" :enew|pu=execute('verbose map')


" [SpaceMapping] j+: Jump {{{1
" ------------------------------------------------------------------------------

call s:nmap_cmd_2('j', 'i', 'Fzf Jump def',            ':FFLines (def')
call s:nmap_cmd_2('j', 'I', 'Fzf Jump def in project', ':MyFzfAg \(def[n]? ')
call s:nmap_cmd_2('j', 't', 'Fzf Jump Tags',           ':FFTags')
call s:nmap_cmd_2('b', 't', 'Fzf BTags',               ':FFBTags')
call s:nmap_cmd_2('j', 'w', 'Hop Jump Word',           ':HopWord')
call s:nmap_cmd_2('j', 'w', 'Hop Jump Word',           ':HopWord')
call s:nmap_cmd_2('j', 'c', 'Hop Jump Character',      ':HopChar2')
call s:nmap_cmd_2('j', 'l', 'Hop Jump Line',           ':HopLineStart')


call s:map_cmd_2('nnoremap', 'j', 'j', 'FZF Jump jumps', ":FzfLua jumps<CR>")
call s:map_cmd_2('nnoremap', 'j', 'm', 'FZF Jump marks', ":FzfLua marks<CR>")

" [SpaceMapping] p+: Project/Install {{{1
" ------------------------------------------------------------------------------

call s:nmap_cmd_2('p', 'f', 'Project files',                   ':FFFiles')
call s:nmap_cmd_2('p', 'i', 'Plug Install',                    ':execute "tabnew\| PlugInstall"')
call s:nmap_cmd_2('p', 'u', 'Plug Update',                     ':execute "tabnew\| PlugUpdate"')
call s:nmap_cmd_2('p', 'o', 'Plugin main page open in Github', ':call OpenGithubPlugin()')

" [SpaceMapping] q+: Quit {{{1
" ------------------------------------------------------------------------------
call s:nmap_cmd_2('q', 'q', 'Quit',             ':q')
call s:nmap_cmd_2('q', 'Q', 'Force Quit',       ':q')
call s:nmap_cmd_2('q', 'a', 'Quit all',         ':qa')
call s:nmap_cmd_2('q', 'A', 'Quit all (Force)', ':qa!')

" [SpaceMapping] r+: Run {{{1
" ------------------------------------------------------------------------------
call s:map_cmd_2('nnoremap', 'r', 'r', 'Run Current file',            ':call RunUsingCurrentFiletype()')
call s:map_cmd_2('nnoremap', 'r', 'f', 'Run FzfLua',                  ':FzfLua<CR>')
call s:map_cmd_2('nmap',     'r', 'l', 'Run Current line in neoterm', '<Plug>(neoterm-repl-send-line)<CR>')
call s:map_cmd_2('nmap',     'r', 'n', 'Run in neoterm',              '<Plug>(neoterm-repl-send)<CR>')
call s:map_cmd_2('nmap',     'r', 't', 'Run test fn',                 '<Plug>(mytestFn)')
call s:map_cmd_2('vmap',     'r', 't', 'Run test fn',                 '<Plug>(mytestFn)')



" [SpaceMapping] s+: Search/Source {{{1
" ------------------------------------------------------------------------------

function! s:map_args_cmd(l1, l2, doc, cmd) abort
   call s:map_cmd_2('nnoremap', a:l1, a:l2,          a:doc, ':'.a:cmd.'<CR>')
   call s:map_cmd_2('nnoremap', a:l1, toupper(a:l2), a:doc, ":<C-U>execute ':" . a:cmd . "'.GetCurrentWord('n')<CR>")
   call s:map_cmd_2('vnoremap', a:l1, a:l2,          a:doc, ":<C-U>execute ':" . a:cmd . "'.GetCurrentWord('v')<CR>")
endfunctio

let s:TYPE = {'dict': type({}), 'funcref': type(function('call')), 'string': type(''), 'list': type([])}
function! s:LuaBlines(query, ...) abort  "{{{2
  if empty(a:query)
      lua require('fzf-lua').lines({ current_buffer_only = true })
  else
      execute "lua require('fzf-lua').lines({ search = '" . a:query . " ', current_buffer_only = true })"
  endif
endfunction

function! s:LuaLines(query, ...) abort  "{{{2
  if empty(a:query)
      lua require('fzf-lua').lines()
  else
      execute "lua require('fzf-lua').lines({ search = '" . a:query . " '})"
  endif
endfunction

function! s:LuaGrep(query, ...) abort  "{{{2
  if empty(a:query)
      lua require('fzf-lua').grep()
  else
      execute "lua require('fzf-lua').grep({ search = '" . a:query . " '})"
  endif
endfunction
" }}}2

" Writing above functions to handle optional parameter. There is a easy way if I don't want to worry about optional parameter
" For example:  command! -nargs=1 MyFzfLuaGrep lua require('fzf-lua').grep({ search = <f-args> })

command! -nargs=* MyFzfLuaLines  call s:LuaLines(<q-args>)
command! -nargs=* MyFzfLuaBlines call s:LuaBlines(<q-args>)
command! -nargs=* MyFzfLuaGrep   call s:LuaGrep(<q-args>)


call s:map_args_cmd('s', 's', 'FZF Search Lines in Current Files', 'MyFzfLuaBlines ')
call s:map_args_cmd('s', 'b', 'FZF Search All Opened Buffers',     'MyFzfLuaLines ')
call s:map_args_cmd('s', 'g', 'FZF <Rg> Search Current Project',   'MyFzfLuaGrep ')
call s:map_args_cmd('s', 'r', 'FZF <Rg> Search Current Project',   'FFRg ')
call s:map_args_cmd('s', 'p', 'FZF <Ag> Search Current Project',   'MyFzfAg ')

call s:nmap_cmd_2('s', 'c', 'Search highlight clean',            ':nohlsearch')
call s:nmap_cmd_2('s', 'e', 'Source current file!',              ':so %')
call s:nmap_cmd_2('s', 'v', 'Source vimrc',                      ':so $XDG_CONFIG_HOME/nvim/init.vim')
call s:nmap_cmd_2('s', 'h', 'Search history',                    ':FzfLua search_history')
call s:nmap_cmd_2('c', 's', 'FZF Schema',                        ':FzfLua colorschemes')

" [SpaceMapping] t+: Toggle {{{1
" ------------------------------------------------------------------------------

function! s:map_toggle_opt(letter, option, mode, doc) abort
   " Example:
   " let which_key_map_space.t.c = "Toggle line cursorcolumn"
   " nnoremap <Space>tc :setlocal cursorcolumn!<CR>:setlocal cursorcolumn?<CR>
   " echom 'let g:which_key_map_space.t.' .a:letter ' = "' .a:doc '"'
   " echom 'nnoremap <Space>t'.a:letter.' :'.a:mode.' '.a:option.'!<CR>:'.a:mode.' '.a:option.'?<CR>'
   "
   call s:map_cmd_2('nnoremap', 't', a:letter, a:doc, ':'.a:mode.' '.a:option.'!<CR>:'.a:mode.' '.a:option.'?<CR>')
endfunction

call s:map_toggle_opt('l', 'cursorline',     'setlocal', 'Toggle line cursorline')
call s:map_toggle_opt('c', 'cursorcolumn',   'setlocal', 'Toggle line cursorcolumn')
call s:map_toggle_opt('n', 'number',         'set',      'Toggle line number ')
call s:map_toggle_opt('r', 'relativenumber', 'set',      'Toggle relative line number')
call s:map_toggle_opt('w', 'wrap',           'set',      'Toggle line wrap')
call s:map_toggle_opt('h', 'hlsearch',       'set',      'Toggle highlight matches')
call s:map_toggle_opt('i', 'list',           'set',      'Toggle invisible char (set list)')


call s:nmap_cmd_2('t', 's', 'Toggle Trailling whitespace indicator', ':ToggleWhitespace')
call s:nmap_cmd_2('t', 'S', 'Toggle Strip Whitespace On Save',       ':ToggleStripWhitespaceOnSave')
call s:nmap_cmd_2('t', 'm', 'Color: Dark/Light Mode',                ':ToggleColorschemeMode')
call s:nmap_cmd_2('t', 'M', 'Color: FZF Schema',                     ':FzfLua colorschemes')
call s:nmap_cmd_2('t', 't', 'Toggle 80 text width',                  ':call ToggleTextWidth()')
call s:nmap_cmd_2('t', 'T', 'TagbarToggle',                          ':TagbarToggle')
" call s:nmap_cmd_2('t', 't', 'call ToggleHighlightCharacterOver80()',       ':Toggle 80 text width')

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

nnoremap \s :call LoopFoldMethod()<CR>:set foldmethod<CR>zv
"}}}2


" [SpaceMapping] w+: Window {{{1
" ------------------------------------------------------------------------------


call s:nmap_cmd_2('w', 'c', 'Window Close',            ':call undoquit#SaveWindowQuitHistory()<CR>:close')
call s:nmap_cmd_2('w', 'h', 'Window Hide',             ':call undoquit#SaveWindowQuitHistory()<CR>:close')
call s:nmap_cmd_2('w', 'd', 'Window Close and Delete', ':call undoquit#SaveWindowQuitHistory()<CR>:bd!')
call s:nmap_cmd_2('w', 'u', 'Undoquit Window',         ':Undoquit')
call s:nmap_cmd_2('w', 'q', 'Write and quit',          ':wq')
call s:nmap_cmd_2('w', 'w', 'VimWiki Index Page',      ':e ~/vimwiki/index.md')
call s:nmap_cmd_2('w', 's', 'Window Swap',             ':call WindowSwap#EasyWindowSwap()')





" [SpaceMapping] z+: WIP {{{1
" ------------------------------------------------------------------------------
" nnoremap <silent> <Space>zz :Goyo<CR>
" nnoremap <silent> <Space>zm :ZoomToggle<CR>
" nnoremap <silent> <Space>zo :ZoomToggle<CR>
" nnoremap <silent> <Space>zl :Limelight!!<CR>

call s:nmap_cmd_2('z', 'z', 'Goyo', ':Goyo')
call s:nmap_cmd_2('z', 'm', 'Zoom', ':ZoomToggle')
call s:nmap_cmd_2('z', 'o', 'Zoom', ':ZoomToggle')
call s:nmap_cmd_2('z', 'l', 'Limelight', ':Limelight!!')
call s:nmap_cmd_2('z', 'u', 'Undo tree', ':UndotreeToggle')

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


let which_key_map_space.1 = 'which_key_ignore'
let which_key_map_space.2 = 'which_key_ignore'
let which_key_map_space.3 = 'which_key_ignore'
let which_key_map_space.4 = 'which_key_ignore'
let which_key_map_space.5 = 'which_key_ignore'
let which_key_map_space.6 = 'which_key_ignore'
let which_key_map_space.7 = 'which_key_ignore'
let which_key_map_space.8 = 'which_key_ignore'
let which_key_map_space.9 = 'which_key_ignore'

nmap <Space>1 <Plug>AirlineSelectTab1
nmap <Space>2 <Plug>AirlineSelectTab2
nmap <Space>3 <Plug>AirlineSelectTab3
nmap <Space>4 <Plug>AirlineSelectTab4
nmap <Space>5 <Plug>AirlineSelectTab5
nmap <Space>6 <Plug>AirlineSelectTab6
nmap <Space>7 <Plug>AirlineSelectTab7
nmap <Space>8 <Plug>AirlineSelectTab8
nmap <Space>9 <Plug>AirlineSelectTab9



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


" call s:nmap_cmd_2('f', 'e', 'Reopen current file',                ':mkview<CR>:e!<CR>:loadview')
nnoremap <Space>fee :mkview<CR>:e!<CR>:loadview<CR>
nnoremap <Space>fec :Defx ~/.config/vim/custom/<CR>
nnoremap <Space>fep :e!   ~/.config/vim/custom/100-plugins.vim<CR>
nnoremap <Space>fef :e!   ~/.config/vim/custom/300-filetypes.vim<CR>
nnoremap <Space>feP :e!   ~/.config/vim/custom/500-plugins-config.vim<CR>
nnoremap <Space>fem :e!   ~/.config/vim/custom/400-mappings.vim<CR>
nnoremap <Space>fer :e!   ~/.config/vim/custom/999-playground.vim<CR>
nnoremap <Space>fev :e!   ~/.config/vim/vimrc<CR>
nnoremap <Space>fet :e!   ~/vimwiki/TODO.md<CR>

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

" }}}1

" Other random / WIP {{{1
" --------------------------------------------------------------------------------

" Fix paste
" p will not overwrite register
" https://vim.fandom.com/wiki/Replace_a_word_with_yanked_text
" xnoremap p "_dP
xnoremap <silent> p p:let @+=@0<CR>:let @"=@0<CR>

" WIP
" nnoremap <silent> gh <Cmd>Lspsaga lsp_finder<CR>
" inoremap <silent> <C-k> <Cmd>Lspsaga signature_help<CR>
" nnoremap <silent> <C-j> :Lspsaga diagnostic_jump_next<CR>


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

" autocmd! CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false})
