" /****************************************************************/
" /*  ____  _             _          ____             __ _        */
" /* |  _ \| |_   _  __ _(_)_ __    / ___|___  _ __  / _(_) __ _  */
" /* | |_) | | | | |/ _` | | '_ \  | |   / _ \| '_ \| |_| |/ _` | */
" /* |  __/| | |_| | (_| | | | | | | |__| (_) | | | |  _| | (_| | */
" /* |_|   |_|\__,_|\__, |_|_| |_|  \____\___/|_| |_|_| |_|\__, | */
" /*                |___/                                  |___/  */
" /****************************************************************/

" => acid.nvim {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:acid_auto_require = 1
let g:acid_alt_paths = ['src/backend']

" nnoremap <leader>r :AcidRequire<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => ALE (asynchronous lint engine) {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ale_fix_on_save = 1
let g:ale_linters = {'clojure': [], 'sh': []}
" I've read that goimports is basically just like gofmt, plus it organizes your
" imports and removes ones you aren't using. So, I prefer goimports over gofmt.
let g:ale_fixers = {'go': ['goimports']}


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => asciidoctor {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:asciidoctor_fenced_languages = ['python', 'clojure']

" Function to create buffer local mappings
fun! AsciidoctorMappings()
  " nnoremap <buffer> <localleader>oo :AsciidoctorOpenRAW<CR>
  " nnoremap <buffer> <localleader>op :AsciidoctorOpenPDF<CR>
  " nnoremap <buffer> <localleader>oh :AsciidoctorOpenHTML<CR>
  " nnoremap <buffer> <localleader>ox :AsciidoctorOpenDOCX<CR>
  " nnoremap <buffer> <localleader>ch :Asciidoctor2HTML<CR>
  " nnoremap <buffer> <localleader>cp :Asciidoctor2PDF<CR>
  " nnoremap <buffer> <localleader>cx :Asciidoctor2DOCX<CR>

  " nnoremap <buffer> <localleader>H
  "       \ :silent! Asciidoctor2HTML<CR>
  "       \ :silent! AsciidoctorOpenHTML<CR>
  " nnoremap <buffer> <localleader>P
  "       \ :silent! Asciidoctor2PDF<CR>
  "       \ :silent! AsciidoctorOpenPDF<CR>
  " nnoremap <buffer> <localleader>D
  "       \ :silent! Asciidoctor2DOCX<CR>
  "       \ :silent! AsciidoctorOpenDOCX<CR>
endfun

" Call AsciidoctorMappings for all `*.adoc` and `*.asciidoc` files
augroup asciidoctor
  autocmd!
  autocmd BufEnter *.adoc,*.asciidoc call AsciidoctorMappings()
augroup END


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => calendar {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:calendar_google_calendar = 1
let g:calendar_clock_12hour = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => clj-refactor {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:clj_refactor_prefix_rewriting = 0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => clojure-static {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let', '-tpl$']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$']
let g:clojure_special_indent_words = 'deftype,defrecord,reify,proxy,extend-type,extend-protocol,letfn'
let g:clojure_align_multiline_strings = 1
let g:clojure_maxlines = 0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => colorizer {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:colorizer_auto_color = 1
" Clojure is included because Conjure's log buffer is a Clojure buffer, and
" sometimes I want to use a Clojure REPL to print things that include ANSI
" escape codes for pretty colors.
let g:colorizer_auto_filetype='scss,css,clojure'

" Disable Colorizer's strange default behavior where it de-colorizes whenever
" you leave the buffer.
let g:colorizer_disable_bufleave = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => ctrlp {{{1
" """"""""""""""""""""""""""""""
" let g:ctrlp_working_path_mode = 'ra'
"
" let g:ctrlp_map = '<c-f>'
" " map <leader>j :CtrlP<cr>
" " map <c-b> :CtrlPBuffer<cr>
"
" let g:ctrlp_max_height = 20
" let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|^.git$\|_site'
" let g:ctrlp_root_markers = ['pom.xml', '.p4ignore', 'project.clj']
"
" let g:ctrlp_use_caching = 0
" if executable('ag')
"     set grepprg=ag\ --nogroup\ --nocolor
"
"     let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" else
"   let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
"   let g:ctrlp_prompt_mappings = {
"     \ 'AcceptSelection("e")': ['<space>', '<cr>', '<2-LeftMouse>'],
"     \ }
" endif


""""""""""""""""""""""""""""""
" => ctrlsf {{{1
""""""""""""""""""""""""""""""
let g:ctrlsf_auto_focus = {"at": "start"}
let g:ctrlsf_winsize = '100%'
" Ignore ctags files. I have this in my global gitignore, which rg respects
" when you run rg at the command-line, but for some reason, it isn't respected
" when using rg via ctrlsf. Weird.
"
" As a workaround, this rg option has the same desired effect.
let g:ctrlsf_extra_backend_args = {'rg': '--glob !tags'}
" nmap <leader>f <Plug>CtrlSFPrompt
" vmap <leader>f <Plug>CtrlSFVwordExec
" nmap <leader>F :CtrlSFOpen<CR>:CtrlSFUpdate<CR>
" nmap <leader>td :CtrlSF -R TODO<bar>FIXME<CR>
" nmap <leader>8 :CtrlSF -R '.{81,}'<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => deoplete {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:deoplete#enable_at_startup = 1

let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.clojure = '[\w!$%&*+/:<=>?@\^_~\-\.#]*'

" Use TAB and SHIFT-TAB to cycle through completions
" inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
" inoremap <expr><S-tab> pumvisible() ? "\<c-p>" : "\<tab>"

" Close the popup window when I accept a completion
" autocmd CompleteDone * pclose!

" Enable integration with the OCAML tool Merlin
if !exists('g:deoplete#omni_patterns')
  let g:deoplete#omni#input_patterns = {}
endif
let g:deoplete#omni#input_patterns.ocaml = '[^. *\t]\.\w*|\s\w*|#'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => dispatch {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:dispatch_no_maps = 1


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => elm-vim {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:elm_format_autosave = 0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => fireplace {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" nnoremap <leader>r :Require!<CR>
" nnoremap <leader>t :RunTests<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => go {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1
" autocmd FileType go nnoremap <buffer> <leader>r  :GoRun<CR>
" autocmd FileType go nnoremap <buffer> <leader>b  :GoBuild<CR>
" autocmd FileType go nnoremap <buffer> <leader>t  :GoTest<CR>
" autocmd FileType go nnoremap <buffer> <leader>c  :GoCoverage<CR>
" autocmd FileType go nnoremap <buffer> <leader>gd :GoDoc<CR>
" autocmd FileType go nnoremap <buffer> <leader>gb :GoDocBrowser<CR>
" autocmd FileType go nnoremap <buffer> <leader>s  :GoImplements<CR>
" autocmd FileType go nnoremap <buffer> <leader>i  :GoInfo<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => gofmt {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" I've read that goimports is basically just like gofmt, plus it organizes your
" imports and removes ones you aren't using. So, I prefer goimports over gofmt.
let g:gofmt_exe = 'goimports'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => hexokinase {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:Hexokinase_highlighters = ['backgroundfull']

let g:Hexokinase_optInPatterns = [
      \ 'full_hex',
      \ 'triple_hex',
      \ 'rgb',
      \ 'rgba',
      \ 'hsl',
      \ 'hsla',
      \ 'colour_names'
      \ ]

let g:Hexokinase_refreshEvents = [
      \ 'BufWrite',
      \ 'BufRead',
      \ 'CursorHoldI',
      \ 'InsertLeave',
      \ 'TextChanged',
      \ ]


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => highlightedyank {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:highlightedyank_highlight_duration = 250


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" => jack-in {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" nnoremap <leader>L :Lein<CR>
"" nnoremap <leader>B :Boot<CR>
"" nnoremap <leader>C :Clj<CR>

"" Not really part of jack-in, but it's as if it were
"" nnoremap <leader>I :Start! iced repl<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" => LanguageClient {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" https://github.com/autozimu/LanguageClient-neovim
let g:LanguageClient_serverCommands = {
    \ 'clojure': ['bash', '/usr/local/bin/clojure-lsp'],
    \ 'sh': ['bash-language-server', 'start']
    \ }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => lightline {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:lightline = {
  \   'colorscheme': 'onehalfdark',
  \   'active': {
  \     'left':[ [ 'mode', 'paste' ],
  \              [ 'gitbranch', 'cocstatus', 'readonly', 'filename', 'modified' ]
  \     ],
  \     'right': [['lineinfo'],
  \               ['percent'],
  \               ['fileformat', 'fileencoding', 'filetype']]
  \   },
	\   'component': {
	\     'lineinfo': ' %3l:%-2v',
	\   },
  \   'component_function': {
  \     'gitbranch': 'fugitive#head',
  \     'cocstatus': 'coc#status',
  \   }
  \ }
let g:lightline.separator = {
	\   'left': '', 'right': ''
  \}
let g:lightline.subseparator = {
	\   'left': '', 'right': ''
  \}

let g:lightline.tabline = {
  \   'left': [ ['tabs'] ],
  \   'right': [ ['close'] ]
  \ }


"""""""""""""""""""""""""""""""
" => sandwich {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let g:sandwich#recipes = deepcopy(g:sandwich#default_recipes)

" Add recipes for vim-surround-style buns with an extra space.
" let g:sandwich#recipes +=
"       \ [
"       \   {
"       \     'buns':         ['{ ', ' }'],
"       \     'nesting':      1,
"       \     'match_syntax': 1,
"       \     'kind':         ['add', 'replace'],
"       \     'action':       ['add'],
"       \     'input':        ['{']
"       \   },
"       \
"       \   {
"       \     'buns':         ['[ ', ' ]'],
"       \     'nesting':      1,
"       \     'match_syntax': 1,
"       \     'kind':         ['add', 'replace'],
"       \     'action':       ['add'],
"       \     'input':        ['[']
"       \   },
"       \
"       \   {
"       \     'buns':         ['( ', ' )'],
"       \     'nesting':      1,
"       \     'match_syntax': 1,
"       \     'kind':         ['add', 'replace'],
"       \     'action':       ['add'],
"       \     'input':        ['(']
"       \   },
"       \
"       \   {
"       \     'buns':         ['{\s*', '\s*}'],
"       \     'nesting':      1,
"       \     'regex':        1,
"       \     'match_syntax': 1,
"       \     'kind':         ['delete', 'replace', 'textobj'],
"       \     'action':       ['delete'],
"       \     'input':        ['{']
"       \   },
"       \
"       \   {
"       \     'buns':         ['\[\s*', '\s*\]'],
"       \     'nesting':      1,
"       \     'regex':        1,
"       \     'match_syntax': 1,
"       \     'kind':         ['delete', 'replace', 'textobj'],
"       \     'action':       ['delete'],
"       \     'input':        ['[']
"       \   },
"       \
"       \   {
"       \     'buns':         ['(\s*', '\s*)'],
"       \     'nesting':      1,
"       \     'regex':        1,
"       \     'match_syntax': 1,
"       \     'kind':         ['delete', 'replace', 'textobj'],
"       \     'action':       ['delete'],
"       \     'input':        ['(']
"       \   },
"       \ ]

" Make it so that when I visually select a line and add parens, brackets or
" curly braces, it indents the line that I'm wrapping.
"
" source: https://github.com/machakann/vim-sandwich/tree/a0cde9cfb20d9aff10a09b7fc776231f7856abef#linewise-and-blockwise-operations
let s:command = "'[+1,']-1normal! >>"
" let g:sandwich#recipes +=
"       \ [
"       \   {
"       \     'buns':       ['{', '}'],
"       \     'motionwise': ['line'],
"       \     'kind':       ['add'],
"       \     'linewise':   1,
"       \     'command':    [s:command]
"       \   },
"       \
"       \   {
"       \     'buns':       ['[', ']'],
"       \     'motionwise': ['line'],
"       \     'kind':       ['add'],
"       \     'linewise':   1,
"       \     'command':    [s:command]
"       \   },
"       \   {
"       \     'buns':       ['(', ')'],
"       \     'motionwise': ['line'],
"       \     'kind':       ['add'],
"       \     'linewise':   1,
"       \     'command':    [s:command]
"       \   },
"       \ ]

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => syntastic {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:syntastic_python_checkers=['pyflakes']

" fixes lag related to having both vim-go and syntastic installed
let g:syntastic_go_checkers = ['golint', 'govet', 'errcheck']
let g:syntastic_mode_map = { 'mode': 'active', 'passive_filetypes': ['go'] }

" enable syntastic integration with the OCAML tool Merlin
let g:syntastic_ocaml_checkers = ['merlin']

" enable syntastic integration with swiftpm and swiftlint
let g:syntastic_swift_checkers = ['swiftpm', 'swiftlint']


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => unicode {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" nnoremap ga :UnicodeName<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vimagit {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let g:magit_show_help = 0
" let g:magit_toggle_help_mapping = '?'
" let g:magit_discard_untracked_do_delete=1
" WIP Update this
" let g:magit_show_magit_mapping='<Space>gM'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vimfiler {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" let g:vimfiler_as_default_explorer = 1
" let g:vimfiler_time_format = "%b %d %Y %H:%M"

" call vimfiler#custom#profile('default', 'context', {
"       \   'safe': 0,
"       \   'force_quit': 1
"       \ })


""""""""""""""""""""""""""""""
" => vim grep {{{1
""""""""""""""""""""""""""""""
let Grep_Skip_Dirs = 'RCS CVS SCCS .svn generated'
set grepprg=/bin/grep\ -nH


""""""""""""""""""""""""""""""
" => vista {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Ensure you have installed some decent font to show these pretty symbols, then you can enable icon for the kind.
let g:vista#renderer#enable_icon = 0
let g:vista_sidebar_width = 40
" The default icons can't be suitable for all the filetypes, you can extend it as you wish.
let g:vista#renderer#icons = {
\   "function": "\uf794",
\   "variable": "\uf71b",
\  }
let g:vista_icon_indent = ["▸ ", ""]
" nnoremap <leader>T :Vista!!<CR>

" Don't blink the cursor after jumping to definition.
let g:vista_top_level_blink = [0, 0]

" Jump to the definition the cursor is over for easy previewing.
let g:vista_echo_cursor_strategy = 'scroll'

" Close the Vista window once I choose something.
let g:vista_close_on_jump = 1

" Instead of the nested structure used by default, list all tags by kind, e.g.
" all methods, then all namespaces, etc. This display looks cleaner to me.
let g:vista#renderer#ctags = 'kind'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vlime {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" A handful of vlime mappings conflict with vim-sexp mappings.
" vlime courteously does not overwrite them, so I can redefine the conflicting
" mappings to different keys here.
" augroup LocalVlimeKeys
"   autocmd!
"   " mnemonic: Send File
"   autocmd FileType lisp nnoremap <silent> <buffer> <LocalLeader>sf
"         \ :call vlime#plugin#LoadFile(expand("%:p"))<cr>
"   " mnemonic: Vlime Interactive mode
"   autocmd FileType lisp nnoremap <silent> <buffer> <LocalLeader>vi
"         \ :call vlime#plugin#InteractionMode()<cr>
" augroup end


""""""""""""""""""""""""""""""
" => YankRing {{{1
""""""""""""""""""""""""""""""
if has("win16") || has("win32")
    " Don't do anything
else
    let g:yankring_history_dir = '~/.vim_runtime/temp_dirs/'
endif


" Chuan's Config: ===================================================================================={{{1
"
" => airline {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:airline_theme = 'luna'                         " airline colorscheme
let g:airline_powerline_fonts = 1                    " Fancy symbols
let g:airline#extensions#tabline#enabled = 1         " Enable the list of buffers
" let g:airline#extensions#tabline#fnamemod = ':t'     " Show just the filename
let g:airline#extensions#tabline#tab_nr_type = 1     " Show buffer #, not # of splits
let g:airline#extensions#tabline#show_tab_nr = 1     " Show buffer # in tabline
let g:airline#extensions#tabline#show_tab_type = 1   " Show the tab type
let g:airline#extensions#tabline#buffer_idx_mode = 1 " Show buffer index

" let spc = g:airline_symbols.space
call airline#parts#define('cust_line_col', {
      \ 'raw': '%l/%L',
      \ 'accent': 'bold'})

call airline#parts#define('cust_line_col_nr', {
      \ 'raw': '%l:%v/%L',
      \ 'accent': 'bold'})

let g:airline_section_b = airline#section#create(['hunks', 'file'])
let g:airline_section_c = airline#section#create(['%<', 'readonly', 'coc_status', 'lsp_progress', 'scrollbar'])
let g:airline_section_x = airline#section#create_right(['coc_current_function', 'bookmark', 'tagbar', 'vista', 'gutentags', 'gen_tags', 'omnisharp', 'grepper', 'filetype'])
" let g:airline_section_z = airline#section#create(['windowswap', 'obsession', '%p%%', 'linenr', 'maxlinenr', 'colnr'])
let g:airline_section_z = airline#section#create(['%p%% ', 'cust_line_col'])



let g:airline_symbols.linenr = ''
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.colnr = ''

nmap <Space>1 <Plug>AirlineSelectTab1
nmap <Space>2 <Plug>AirlineSelectTab2
nmap <Space>3 <Plug>AirlineSelectTab3
nmap <Space>4 <Plug>AirlineSelectTab4
nmap <Space>5 <Plug>AirlineSelectTab5
nmap <Space>6 <Plug>AirlineSelectTab6
nmap <Space>7 <Plug>AirlineSelectTab7
nmap <Space>8 <Plug>AirlineSelectTab8
nmap <Space>9 <Plug>AirlineSelectTab9


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => anyfold {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plug 'pseewald/vim-anyfold'

" autocmd Filetype * AnyFoldActivate               " activate for all filetypes
" or
" autocmd Filetype <your-filetype> AnyFoldActivate " activate for a specific filetype

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" => auto-pairs {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:AutoPairsMapSpace=0

augroup autopairs_config
  autocmd!
  " autocmd Filetype clojure let g:AutoPairsFlyMode = 1

  " don't pair single quotes or backticks when editing lisp code
  " autocmd Filetype lisp,scheme,clojure,lfe let b:AutoPairs = {'(':')', '[':']', '{':'}','"':'"'}

  " disable auto-pairs for lisp -- it interferes with parinfer
  autocmd Filetype lisp,scheme,clojure,lfe let b:AutoPairs = {}


  " Default
  " let g:AutoPairs={'(':')', '[':']', '{':'}',"'":"'",'"':'"', "`":"`", '```':'```', '"""':'"""', "'''":"'''"}

  " Don't auto pair " and { when it is vim mode
  " It pairs for """
  " autocmd Filetype vim let b:AutoPairs = {'<':'>', '(':')', '[':']', "'":"'",'"':'', "`":"`", '```':'```', '""':'"', "'''":"'''"}
  autocmd Filetype vim let b:AutoPairs = {'<':'>', '(':')', '[':']', "'":"'",'"':'', "`":"`", '```':'```', "'''":"'''"}

augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => better-whitespace {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:better_whitespace_filetypes_blacklist = ['diff', 'gitcommit', 'unite', 'qf', 'help', 'ctrlsf']

" function! ReloadWithoutScrolling() abort
"   let l:currentview = winsaveview()
"   edit
"   call winrestview(l:currentview)
" endfunction

let g:better_whitespace_enabled=1
let g:strip_whitespace_on_save=1
let g:strip_whitespace_confirm=0

let g:better_whitespace_ctermcolor='LightYellow'
let g:better_whitespace_guicolor='#6272a4'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => buffergator {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" horizontal bottom (full screen width)
let g:buffergator_viewport_split_policy = "B"
let g:buffergator_split_size = 10
let g:buffergator_suppress_keymaps = 1

" press ESC to close the buffergator buffer
augroup buffergator_buffer
  autocmd!
  autocmd BufEnter \[\[buffergator-buffers\]\] nnoremap <buffer> <ESC> :bd<CR>
augroup END


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => coc  {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let g:coc_config_home = '$XDG_CONFIG_HOME/vim'
let g:coc_data_home = '$XDG_DATA_HOME/coc'


" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

if exists('g:plugs["coc.nvim"]')

  function! ToggleCoc() abort
    if len(coc#status()) == 0
      execute 'CocEnable'
    else
      execute 'CocDisable'
    endif
  endfunction

  " WIP
  " Use tab for trigger completion with characters ahead and navigate.
  " NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
  " other plugin before putting this into your config.
  inoremap <silent><expr> <TAB>
	\ pumvisible() ? "\<C-n>" :
	\ <SID>check_back_space() ? "\<TAB>" :
	\ coc#refresh()
  inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

  function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
  endfunction

  " Use <c-space> to trigger completion.
  if has('nvim')
    inoremap <silent><expr> <c-space> coc#refresh()
  else
    inoremap <silent><expr> <c-@> coc#refresh()
  endif

  function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    elseif (coc#rpc#ready())
      call CocActionAsync('doHover')
    else
      execute '!' . &keywordprg . " " . expand('<cword>')
    endif
  endfunction

  " NOT WORKing
  " " Make <CR> auto-select the first completion item and notify coc.nvim to
  " " format on enter, <cr> could be remapped by other vim plugin
  " inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
  "                               \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"


  " Use `[g` and `]g` to navigate diagnostics
  " Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
  nmap <silent> [g <Plug>(coc-diagnostic-prev)
  nmap <silent> ]g <Plug>(coc-diagnostic-next)

  " GoTo code navigation.
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)

  " Use K to show documentation in preview window.
  nnoremap <silent> K :call <SID>show_documentation()<CR>
  nnoremap <silent> <Space>cd :call <SID>show_documentation()<CR>

  " Highlight the symbol and its references when holding the cursor.
  " TODO Enable this
  " autocmd CursorHold * silent call CocActionAsync('highlight')


  " Formatting selected code.
  xmap <Space>cf  <Plug>(coc-format-selected)
  nmap <Space>cf  <Plug>(coc-format-selected)

  " augroup mygroup
  "   autocmd!
  "   " Setup formatexpr specified filetype(s).
  "   autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  "   " Update signature help on jump placeholder.
  "   autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
  " augroup end

  " Applying codeAction to the selected region.
  " Example: `<leader>aap` for current paragraph
  xmap <Space>ca  <Plug>(coc-codeaction-selected)
  nmap <Space>ca  <Plug>(coc-codeaction-selected)

  " Remap keys for applying codeAction to the current buffer.
  nmap <Space>cac  <Plug>(coc-codeaction)
  " Apply AutoFix to problem on the current line.
  nmap <Space>cqf  <Plug>(coc-fix-current)

  " Map function and class text objects
  " NOTE: Requires 'textDocument.documentSymbol' support from the language server.
  " xmap if <Plug>(coc-funcobj-i)
  " omap if <Plug>(coc-funcobj-i)
  " xmap af <Plug>(coc-funcobj-a)
  " omap af <Plug>(coc-funcobj-a)
  " xmap ic <Plug>(coc-classobj-i)
  " omap ic <Plug>(coc-classobj-i)
  " xmap ac <Plug>(coc-classobj-a)
  " omap ac <Plug>(coc-classobj-a)

  " Remap <C-f> and <C-b> for scroll float windows/popups.
  " WIP C-j and C-k
  if has('nvim-0.4.0') || has('patch-8.2.0750')
    nnoremap <silent><nowait><expr> <C-j> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-j>"
    nnoremap <silent><nowait><expr> <C-k> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-k>"
    inoremap <silent><nowait><expr> <C-j> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
    inoremap <silent><nowait><expr> <C-k> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
    vnoremap <silent><nowait><expr> <C-j> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-j>"
    vnoremap <silent><nowait><expr> <C-k> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-k>"
  endif

  " Use CTRL-S for selections ranges.
  " Requires 'textDocument/selectionRange' support of language server.
  nmap <silent> <C-s> <Plug>(coc-range-select)
  xmap <silent> <C-s> <Plug>(coc-range-select)

  " " Add `:Format` command to format current buffer.
  " command! -nargs=0 Format :call CocAction('format')
  "
  " " Add `:Fold` command to fold current buffer.
  " command! -nargs=? Fold :call     CocAction('fold', <f-args>)
  "
  " " Add `:OR` command for organize imports of the current buffer.
  " command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

  " Mappings for CoCList


  " COC Actions
  " nnoremap <silent><nowait> <space>c:  :CocAction<cr>
  " xnoremap <silent><nowait> <space>c:  :CocAction<cr>
  nnoremap <silent><nowait> <space>;  :CocAction<cr>
  xnoremap <silent><nowait> <space>;  :CocAction<cr>

  " nnoremap <silent><nowait> <space>:  <Plug>(coc-codeaction-cursor)
  nmap <silent> <space>:            <Plug>(coc-codeaction-line)
  vmap <silent> <space>:            <Plug>(coc-codeaction-selected)
  " nnoremap <leader>ci :CocInfo<CR>

  " Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
  " vmap <leader>a  <Plug>(coc-codeaction-selected)
  " nmap <leader>a  <Plug>(coc-codeaction-selected)

  " Remap for do codeAction of current line
  " nmap <space>c;  <Plug>(coc-codeaction)
  " Fix autofix problem of current line
  " nmap <leader>qf  <Plug>(coc-fix-current)


  " vmap <leader>cf <Plug>(coc-format-selected)
  " nmap <leader>cf <Plug>(coc-format-selected)


  " WIP
  " COC with Clojure
  " https://clojure-lsp.github.io/clojure-lsp/clients/
  function! Expand(exp) abort
    let l:result = expand(a:exp)
    return l:result ==# '' ? '' : "file://" . l:result
  endfunction

  autocmd CursorHold * silent call CocActionAsync('highlight')

  " TODO add mapping for cojure only
  nnoremap <silent> <leader>rcc :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'cycle-coll', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
  nnoremap <silent> <leader>rth :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-first', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
  nnoremap <silent> <leader>rtt :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-last', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
  nnoremap <silent> <leader>rtf :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-first-all', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
  nnoremap <silent> <leader>rtl :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'thread-last-all', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
  nnoremap <silent> <leader>ruw :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'unwind-thread', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
  nnoremap <silent> <leader>rua :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'unwind-all', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
  nnoremap <silent> <leader>rml :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'move-to-let', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')]})<CR>
  nnoremap <silent> <leader>ril :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'introduce-let', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1, input('Binding name: ')]})<CR>
  nnoremap <silent> <leader>rel :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'expand-let', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
  nnoremap <silent> <leader>ram :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'add-missing-libspec', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
  nnoremap <silent> <leader>rcn :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'clean-ns', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1]})<CR>
  nnoremap <silent> <leader>ref :call CocRequest('clojure-lsp', 'workspace/executeCommand', {'command': 'extract-function', 'arguments': [Expand('%:p'), line('.') - 1, col('.') - 1, input('Function name: ')]})<CR>

  " End coc config
endif

" augroup coc_load_clojure_content
"   autocmd BufReadCmd,FileReadCmd,SourceCmd jar:file://*
"         \ call s:LoadClojureContent(expand("<amatch>"))
" augroup END

" function! s:LoadClojureContent(uri) abort
"   setfiletype clojure
"   let content = CocRequest('clojure-lsp', 'clojure/dependencyContents', {'uri': a:uri})
"   call setline(1, split(content, "\n"))
"   setl nomodified
"   setl readonly
" endfunction
" Clojure Notes:
" https://github.com/dense-analysis/ale
" ale is conflict with clojure-lsp
" https://clojure-lsp.github.io/clojure-lsp/settings/#clj-kondo

" => commentary {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup commentary_config
  autocmd!
  autocmd FileType cs,kotlin,adoc setlocal commentstring=//\ %s
  " autocmd FileType lisp,clojure,racket setlocal commentstring=;;\ %s
  autocmd FileType lisp,racket setlocal commentstring=;;\ %s
  autocmd FileType clojure setlocal commentstring=;;\ %s
  " TODO Add syntax aware comment #_
  autocmd FileType sml,ocaml setlocal commentstring=(*\ %s\ *)
  autocmd FileType resolv,crontab setlocal commentstring=#\ %s
  autocmd FileType sql setlocal commentstring=--\ %s
  autocmd FileType robot setlocal commentstring=#\ %s
augroup END


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => conjure {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" NOTE: I'd like to have a good setup where ANSI escape codes are interpreted in
" the Conjure log buffer. Currently, neither Colorizer nor Olical's fork of
" AnsiEsc are doing this as well as I'd like. I think Olical/AnsiEsc is more
" likely to get there with time (Colorizer seems to be under-maintained), but
" for now, Colorizer is marginally better, so I'm sticking with Colorizer for
" now. I'm leaving AnsiEsc config intact below, ready to be un-commented when I
" inevitably switch back over to Olical/AnsiEsc.

" Disable the default ANSI escape code stripping so that I can use a separate
" plugin to interpret them and display the colors.
" let g:conjure#log#strip_ansi_escape_sequences_line_limit = 0

" let g:conjure#log#hud#passive_close_delay = 1000
"
" let g:conjure#highlight#enabled = v:true

" let g:conjure#filetype#sicp = "conjure.client.racket.stdio"

" function! ToggleConjureLog() abort
"   if expand('%:t') =~ ".*conjure-log-.*"
"     execute 'Bclose'
"   else
"     " Ideally I could call some function provided by Conjure directly to do
"     " this, but I wasn't able to figure out how to do that. This mapping will
"     " need to be adjusted if I ever configure Conjure to use a different mapping
"     " to open the log in a tab, or if Conjure ever changes the default mapping.
"     " I think those two things are both pretty unlikely to happen, so meh.
"     "
"     " Another thing worth noting: normal apparently doesn't work with <leader>
"     " and <localleader>, so you have to do some hackery like what's going on
"     " here (https://vi.stackexchange.com/a/7780/25687) or just give up and type
"     " your actual (local)leader key in the mapping. I'm doing the second one.
"     normal \lt
"   endif
" endfunction

" augroup additional_conjure_bindings
"   autocmd!
"
"   autocmd FileType clojure,fennel,janet,racket
"         \ nnoremap <buffer>
"         \ <localleader>cc :call ToggleConjureLog()<CR>
"   autocmd FileType clojure,fennel,janet,racket
"         \ nnoremap <buffer>
"         \ <localleader>cl :call ToggleConjureLog()<CR>
"
"   " mnemonic: eval prompt
"   " (like how <localleader>ee is eval expression)
"   autocmd FileType clojure,fennel,janet,racket
"         \ nnoremap <buffer>
"         \ <localleader>ep :ConjureEval<space>
"
"   " press q to close the log buffer
"   autocmd BufEnter conjure-log-* nnoremap <buffer> q :Bclose<CR>
"
"   " Automatically enable AnsiEsc (interpret ANSI escape codes) for the Conjure
"   " log buffer.
"   " autocmd BufEnter conjure-log-* AnsiEsc
" augroup END


" """"""""""""""""""""""""""""""
"
" => defx {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" defx somewhat annoyingly doesn't provide any default mappings. I copy-pasted
" this example config from :help defx and modified it.
function! s:defx_my_settings() abort
  " Define mappings
  nnoremap <silent><buffer><expr> <CR>
	\ defx#do_action('open')
  nnoremap <silent><buffer><expr> c
	\ defx#do_action('copy')
  nnoremap <silent><buffer><expr> m
	\ defx#do_action('move')
  nnoremap <silent><buffer><expr> p
	\ defx#do_action('paste')
  nnoremap <silent><buffer><expr> l
	\ defx#do_action('open')
  nnoremap <silent><buffer><expr> E
	\ defx#do_action('open', 'vsplit')
  nnoremap <silent><buffer><expr> P
	\ defx#do_action('open', 'pedit')
  nnoremap <silent><buffer><expr> o
	\ defx#do_action('open_or_close_tree')
  nnoremap <silent><buffer><expr> K
	\ defx#do_action('new_directory')
  nnoremap <silent><buffer><expr> N
	\ defx#do_action('new_file')
  nnoremap <silent><buffer><expr> M
	\ defx#do_action('new_multiple_files')
  nnoremap <silent><buffer><expr> C
	\ defx#do_action('toggle_columns',
	\                'mark:indent:icon:filename:type:size:time')
  nnoremap <silent><buffer><expr> S
	\ defx#do_action('toggle_sort', 'time')
  nnoremap <silent><buffer><expr> d
	\ defx#do_action('remove')
  nnoremap <silent><buffer><expr> r
	\ defx#do_action('rename')
  nnoremap <silent><buffer><expr> !
	\ defx#do_action('execute_command')
  nnoremap <silent><buffer><expr> x
	\ defx#do_action('execute_system')
  nnoremap <silent><buffer><expr> yy
	\ defx#do_action('yank_path')
  nnoremap <silent><buffer><expr> .
	\ defx#do_action('toggle_ignored_files')
  nnoremap <silent><buffer><expr> ;
	\ defx#do_action('repeat')
  nnoremap <silent><buffer><expr> h
	\ defx#do_action('cd', ['..'])
  nnoremap <silent><buffer><expr> ~
	\ defx#do_action('cd')
  nnoremap <silent><buffer><expr> q
	\ defx#do_action('quit')
  nnoremap <silent><buffer><expr> <Space>
	\ defx#do_action('toggle_select') . 'j'
  nnoremap <silent><buffer><expr> *
	\ defx#do_action('toggle_select_all')
  nnoremap <silent><buffer><expr> j
	\ line('.') == line('$') ? 'gg' : 'j'
  nnoremap <silent><buffer><expr> k
	\ line('.') == 1 ? 'G' : 'k'
  nnoremap <silent><buffer><expr> <C-l>
	\ defx#do_action('redraw')
  nnoremap <silent><buffer><expr> <C-g>
	\ defx#do_action('print')
  nnoremap <silent><buffer><expr> cd
	\ defx#do_action('change_vim_cwd')
  call defx#custom#option('_', {
	\ 'columns': 'mark:indent:icon:filename:type:size:time',
	\ })
  call defx#custom#column('time', {
	\ 'format': '%Y-%m-%d %I:%M %p',
	\ })
endfunction

function! s:open_defx_if_directory() abort
  " This throws an error if the buffer name contains unusual characters like
  " [[buffergator]]. Desired behavior in those scenarios is to consider the
  " buffer not to be a directory.
  try
    let l:full_path = expand(expand('%:p'))
  catch
    return
  endtry

  " If the path is a directory, delete the (useless) buffer and open defx for
  " that directory instead.
  if isdirectory(l:full_path)
    execute "Defx `expand('%:p')` | bd " . expand('%:r')
  endif
endfunction

augroup defx_config
  autocmd!
  autocmd FileType defx call s:defx_my_settings()

  " It seems like BufReadPost should work for this, but for some reason, I can't
  " get it to fire. BufEnter seems to be more reliable.
  autocmd BufEnter * call s:open_defx_if_directory()
augroup END

" netrw apparently has a similar callback, because sometimes netrw opens instead
" of defx! According to https://stackoverflow.com/a/21687112/2338327, this
" disables netrw.
let loaded_netrwPlugin = 1

nnoremap <silent> -
      \ :Defx `expand('%:p:h')`
      \ -search=`expand('%:p')`<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" => findroot {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" If you allow changing directory that goes up from sub-directory:
let g:findroot_not_for_subdir = 0
let g:findroot_patterns = [
      \  '.git/',
      \  '.svn/',
      \  '.hg/',
      \  '.bzr/',
      \  '.gitignore',
      \  'Rakefile',
      \  'pom.xml',
      \  'project.clj',
      \  '*.csproj',
      \  '*.sln',
      \  '.vimroot',
      \]
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => fzf {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" nnoremap <C-f> :FZF<CR>
" nnoremap <C-g> :GFiles<CR>
" nnoremap <C-g> :GFiles<CR>

let g:fzf_command_prefix="FF"

" Update so it will search hiddent file
command! -bang -nargs=* MyFzfAg
  \ call fzf#vim#ag(<q-args>,
  \                 '--ignore "node_modules" --hidden',
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)

" command!      -bang -nargs=* LL                        call fzf#vim#lines(<q-args>, <bang>0),

" command!      -bang -nargs=* Lines                     call fzf#vim#lines(<q-args>, <bang>0),
" command!      -bang -nargs=* BLines                    call fzf#vim#buffer_lines(<q-args>, <bang>0),

function! FloatingFZF() abort "{{{2
  let buf = nvim_create_buf(v:false, v:true)
  call setbufvar(buf, '&signcolumn', 'no')

  " let height = float2nr(15)
  " let width = float2nr(80)
  let width = float2nr(&columns * 0.9)
  let height = float2nr(&lines * 0.6)
  let col = float2nr((&columns - width) / 2)
  let row = float2nr((&lines - height) / 2)

  let opts = {
	\ 'relative': 'editor',
	\ 'row': row,
	\ 'col': col,
	\ 'width': width,
	\ 'height': height,
	\ 'style': 'minimal'
	\ }

  call nvim_open_win(buf, v:true, opts)
endfunction " }}}2

" source: https://www.reddit.com/r/neovim/comments/djmehv/im_probably_really_late_to_the_party_but_fzf_in_a/f463fxr/
" https://github.com/junegunn/fzf.vim/issues/664
" TODO Improve color
" https://www.erickpatrick.net/blog/adding-syntax-highlighting-to-fzf.vim-preview-window
let g:fzf_layout = { 'window': 'call FloatingFZF()' }
let g:fzf_files_options= '--preview "bat {} 2> /dev/null | head 100" --bind "?:toggle-preview"'
let g:fzf_history_dir = '~/.local/share/fzf-history'

let $FZF_DEFAULT_OPTS .= " --ansi --border --layout=reverse "
let $FZF_DEFAULT_OPTS .= " --bind 'up:previous-history' "
let $FZF_DEFAULT_OPTS .= " --bind 'down:next-history' "
let $FZF_DEFAULT_OPTS .= " --bind 'ctrl-p:up' --bind 'ctrl-n:down' "

" This is the default extra key bindings
" let g:fzf_action = { 'ctrl-t': 'tab split', 'ctrl-x': 'split', 'ctrl-v': 'vsplit' }

" Customize fzf colors to match your color scheme
" - fzf#wrap translates this to a set of `--color` options
let g:fzf_colors =
      \ { 'fg':      ['fg', 'Normal'],
      \ 'bg':      ['bg', 'Normal'],
      \ 'hl':      ['fg', 'Comment'],
      \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
      \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
      \ 'hl+':     ['fg', 'Statement'],
      \ 'info':    ['fg', 'PreProc'],
      \ 'border':  ['fg', 'Ignore'],
      \ 'prompt':  ['fg', 'Conditional'],
      \ 'pointer': ['fg', 'Exception'],
      \ 'marker':  ['fg', 'Keyword'],
      \ 'spinner': ['fg', 'Label'],
      \ 'header':  ['fg', 'Comment'] }


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => GV {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" A Better Git log viewer
" ### Commands
"
" - `:GV` to open commit browser
"     - You can pass `git log` options to the command, e.g. `:GV -S foobar -- plugins`.
" - `:GV!` will only list commits that affected the current file
" - `:GV?` fills the location list with the revisions of the current file
"
" `:GV` or `:GV?` can be used in visual mode to track the changes in the
" selected lines.
"
" ### Mappings
"
" - `o` or `<cr>` on a commit to display the content of it
" - `o` or `<cr>` on commits to display the diff in the range
" - `O` opens a new tab instead
" - `gb` for `:Gbrowse`
" - `]]` and `[[` to move between commits
" - `.` to start command-line with `:Git [CURSOR] SHA` à la fugitive
" - `q` or `gq` to close
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => guntentags {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" No tag generate for git commit. This will remove the error message in git
" commit: https://github.com/ludovicchabant/vim-gutentags/issues/269
let g:gutentags_exclude_filetypes = ['gitcommit', 'gitconfig', 'gitrebase', 'gitsendemail', 'git']
" WIP, not sure if it is working
let g:gutentags_project_root = ['pom.xml', '.p4ignore', 'project.clj', '.git']


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => gina {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gina#action#index#discard_directories = 1

" nnoremap <leader>g<space> :Gina<space>
" nnoremap <leader>gA :Gina add --all<CR>:Gina status<CR>
" this one actually comes from fugitive because I like its output better than
" :Gina blame. Putting it here so that I don't have to look in two places for
" all my git-related keybindings.
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

" press q to close gina buffers
augroup gina_buffers
  autocmd!
  autocmd BufEnter gina://* nnoremap <buffer> q :bd<CR>
augroup END


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => gitgutter {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gitgutter_map_keys = 0

" Update gitgutter on BufEnter instead of on FocusGained. Another approach would
" be to get vim/tmux focus events working, however I've observed issues with
" gitgutter not updating even while I'm staying inside Vim, e.g. after making a
" commit via `:Gina commit`, so reacting to BufEnter seems like a better way to
" get this working the way I want it to work.
"
" ref: https://github.com/airblade/vim-gitgutter#when-signs-dont-update-after-focusing-vim
let g:gitgutter_terminal_reports_focus=0


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => junegunn/limelight {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plug 'junegunn/limelight.vim'

" autocmd! User GoyoEnter Limelight
" autocmd! User GoyoLeave Limelight!

" " Color name (:help cterm-colors) or ANSI code
" let g:limelight_conceal_ctermfg = 'gray'
" let g:limelight_conceal_ctermfg = 240

" " Color name (:help gui-colors) or RGB color
" let g:limelight_conceal_guifg = 'DarkGray'
" let g:limelight_conceal_guifg = '#777777'

" Default: 0.5
let g:limelight_default_coefficient = 0.6

" Number of preceding/following paragraphs to include (default: 0)
let g:limelight_paragraph_span = 0

" Beginning/end of paragraph
"   When there's no empty line between the paragraphs
"   and each paragraph starts with indentation
let g:limelight_bop = '^\s'
let g:limelight_eop = '\ze\n^\s'

" To space?
nmap <Leader>l <Plug>(Limelight)
xmap <Leader>l <Plug>(Limelight)

" Highlighting priority (default: 10)
"   Set it to -1 not to overrule hlsearch
let g:limelight_priority = -1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" => junegunn/goyo {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" goyo_height: goyo is implimented with 4 invisiable window so it is impossible
" to have full height.
let g:goyo_width=90
let g:goyo_height=100
let g:goyo_margin_top = 0
let g:goyo_margin_bottom = 0

function! s:goyo_enter()
  if executable('tmux') && strlen($TMUX)
    silent !tmux set status off
    silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  endif
  set noshowmode
  set noshowcmd
  set scrolloff=999
  # Limelight
endfunction

function! s:goyo_leave()
  if executable('tmux') && strlen($TMUX)
    silent !tmux set status on
    silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  endif
  set showmode
  set showcmd
  set scrolloff=15
  syntax on
  # Limelight!
endfunction

augroup GoYo
  autocmd!
  autocmd User GoyoEnter nested call <SID>goyo_enter()
  autocmd User GoyoLeave nested call <SID>goyo_leave()
augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => neoterm {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:neoterm_shell = 'zsh'
let g:neoterm_autoscroll = 1
let g:neoterm_keep_term_open = 1
let g:neoterm_autoinsert = 1

augroup neoterm
  autocmd!
  au TermEnter * set scrolloff=0
augroup END

tnoremap <Esc> <C-\><C-n>
tnoremap <C-h> <C-\><C-N><C-w>h

" tnoremap <C-j> <C-\><C-N><C-w>j
" tnoremap <C-k> <C-\><C-N><C-w>k
" tnoremap <C-l> <C-\><C-N><C-w>l


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => open-browser {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" the gx mapping provided by netrw used to open the URL under the cursor in my
" browser, then it mysteriously stopped working. The new behavior is that it
" curls the URL into a tempfile and then open the file in my browser, and I
" would see the HTML.

let g:netrw_nogx = 1 " disable netrw's gx mapping.
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)

let g:openbrowser_search_engines = {
      \   'favorite': 'http://example.com/search?q={query}',
      \     'github': 'https://github.com/search?q={query}',
      \}
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => quick-scope {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Trigger a highlight in the appropriate direction when pressing these keys:
let g:qs_highlight_on_keys = ['f', 'F', 't', 'T']
augroup qs_colors
  autocmd!
  autocmd ColorScheme * highlight QuickScopePrimary guifg='#afff5f' gui=underline ctermfg=155 cterm=underline
  autocmd ColorScheme * highlight QuickScopeSecondary guifg='#5fffff' gui=underline ctermfg=81 cterm=underline
augroup END

" let g:qs_hi_priority = 2

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => rainbow {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plug 'luochen1990/rainbow'
let g:rainbow_active = 1

let g:rainbow_conf = {
      \	'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
      \	'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
      \	'guis': [''],
      \	'cterms': [''],
      \	'operators': '_,_',
      \	'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
      \	'separately': {
	\		'*': {},
	\		'markdown': 0,
	\		'lisp': {
	  \			'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'],
	  \		},
	  \		'haskell': {
	    \			'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/\v\{\ze[^-]/ end=/}/ fold']
	    \		},
	    \               'html': 0,
	    \		'vim': {
	      \			'parentheses_options': 'containedin=vimFuncBody',
	      \		},
	      \		'vimwiki': 0,
	      \		'perl': {
		\			'syn_name_prefix': 'perlBlockFoldRainbow',
		\		},
		\		'stylus': {
		  \			'parentheses': ['start=/{/ end=/}/ fold contains=@colorableGroup']
		  \		},
		  \		'css': 0
		  \	}
		  \}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => signature (Improve Marks) {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" mx           Toggle mark 'x' and display it in the leftmost column
" dmx          Remove mark 'x' where x is a-zA-Z

" m,           Place the next available mark
" m.           If no mark on line, place the next available mark. Otherwise, remove (first) existing mark.
" m-           Delete all marks from the current line
" m<Space>     Delete all marks from the current buffer
" ]`           Jump to next mark
" [`           Jump to prev mark
" ]'           Jump to start of next line containing a mark
" ['           Jump to start of prev line containing a mark
" `]           Jump by alphabetical order to next mark
" `[           Jump by alphabetical order to prev mark
" ']           Jump by alphabetical order to start of next line having a mark
" '[           Jump by alphabetical order to start of prev line having a mark
" m/           Open location list and display marks from current buffer

" m[0-9]       Toggle the corresponding marker !@#$%^&*()
" m<S-[0-9]>   Remove all markers of the same type
" ]-           Jump to next line having a marker of the same type
" [-           Jump to prev line having a marker of the same type
" ]=           Jump to next line having a marker of any type
" [=           Jump to prev line having a marker of any type
" m?           Open location list and display markers from current buffer
" m<BS>        Remove all markers

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => startify {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:startify_custom_header = [
      \ '',
      \' /**********************************************************************/',
      \' /*       ___          ___          ___          ___          ___      */',
      \' /*      /\  \        /\  \        /\  \        /\__\        /|  |     */',
      \' /*      \:\  \       \:\  \      /::\  \      /:/  /       |:|  |     */',
      \' /*       \:\  \       \:\  \    /:/\:\  \    /:/  /        |:|  |     */',
      \' /*   ___ /::\  \  ___ /::\  \  /:/ /::\  \  /:/  /  ___  __|:|  |     */',
      \' /*  /\  /:/\:\__\/\  /:/\:\__\/:/_/:/\:\__\/:/__/  /\__\/\ |:|__|____ */',
      \' /*  \:\/:/  \/__/\:\/:/  \/__/\:\/:/  \/__/\:\  \ /:/  /\:\/:::::/__/ */',
      \' /*   \::/__/      \::/__/      \::/__/      \:\  /:/  /  \::/~~/~     */',
      \' /*    \:\  \       \:\  \       \:\  \       \:\/:/  /    \:\~~\      */',
      \' /*     \:\__\       \:\__\       \:\__\       \::/  /      \:\__\     */',
      \' /*      \/__/        \/__/        \/__/        \/__/        \/__/     */',
      \" /* Happy Hacking. Chuan's new vim                                     */",
      \' /**********************************************************************/',
      \ ]

let g:startify_files_number = 6
let g:startify_enable_special = 0
let g:startify_session_persistence = 1
let g:startify_bookmarks = [{'zn': '~/.config/vim/nvimrc'},
      \ {'zz': '~/.config/zsh/.zshrc'},
      \ {'za': '~/.config/alacritty/alacritty.yml'},
      \ {'zt': '~/.tmux.conf'}
      \]

let g:startify_session_before_save = [ 'silent! tabdo NERDTreeClose' ]

let g:startify_lists = [
      \ { 'type': 'dir',       'header': ['   PWD '. getcwd()] },
      \ { 'type': 'sessions',  'header': ['   Sessions'], 'indices': ['a' ,'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j']},
      \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
      \ { 'type': 'commands',  'header': ['   Commands']       },
      \ { 'type': 'files',     'header': ['   MRU']            },
      \ ]


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => sexp {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Nice function from tpope/vim-sexp-mappings-for-regular-people,
" can't loaded as plugin since I have customized sexp mappings
function! s:map_sexp_wrap(type, target, left, right, pos)
  execute (a:type ==# 'v' ? 'x' : 'n').'noremap'
	\ '<buffer><silent>' a:target ':<C-U>let b:sexp_count = v:count<Bar>exe "normal! m`"<Bar>'
	\ . 'call sexp#wrap("'.a:type.'", "'.a:left.'", "'.a:right.'", '.a:pos.', 0)'
	\ . '<Bar>silent! call repeat#set("'.a:target.'", v:count)<CR>'
endfunction

function! g:VIM_lisp_mappings()  "{{{2
  echom "Meow!! config vim_lisp_mappings"

  " TODO Some of those binding can be used outside of lisp world
  " TODO Disable auto adding space

  xmap <silent><buffer> af              <Plug>(sexp_outer_list)
  omap <silent><buffer> af              <Plug>(sexp_outer_list)
  xmap <silent><buffer> if              <Plug>(sexp_inner_list)
  omap <silent><buffer> if              <Plug>(sexp_inner_list)
  xmap <silent><buffer> aF              <Plug>(sexp_outer_top_list)
  omap <silent><buffer> aF              <Plug>(sexp_outer_top_list)
  xmap <silent><buffer> iF              <Plug>(sexp_inner_top_list)
  omap <silent><buffer> iF              <Plug>(sexp_inner_top_list)
  xmap <silent><buffer> as              <Plug>(sexp_outer_string)
  omap <silent><buffer> as              <Plug>(sexp_outer_string)
  xmap <silent><buffer> is              <Plug>(sexp_inner_string)
  omap <silent><buffer> is              <Plug>(sexp_inner_string)
  xmap <silent><buffer> ae              <Plug>(sexp_outer_element)
  omap <silent><buffer> ae              <Plug>(sexp_outer_element)
  xmap <silent><buffer> ie              <Plug>(sexp_inner_element)
  omap <silent><buffer> ie              <Plug>(sexp_inner_element)
  nmap <silent><buffer> (               <Plug>(sexp_move_to_prev_bracket)
  xmap <silent><buffer> (               <Plug>(sexp_move_to_prev_bracket)
  omap <silent><buffer> (               <Plug>(sexp_move_to_prev_bracket)
  nmap <silent><buffer> )               <Plug>(sexp_move_to_next_bracket)
  xmap <silent><buffer> )               <Plug>(sexp_move_to_next_bracket)
  omap <silent><buffer> )               <Plug>(sexp_move_to_next_bracket)
  nmap <silent><buffer> [[              <Plug>(sexp_move_to_prev_top_element)
  xmap <silent><buffer> [[              <Plug>(sexp_move_to_prev_top_element)
  omap <silent><buffer> [[              <Plug>(sexp_move_to_prev_top_element)
  nmap <silent><buffer> ]]              <Plug>(sexp_move_to_next_top_element)
  xmap <silent><buffer> ]]              <Plug>(sexp_move_to_next_top_element)
  omap <silent><buffer> ]]              <Plug>(sexp_move_to_next_top_element)

  nmap <silent><buffer> ==              <Plug>(sexp_indent)
  nmap <silent><buffer> =-              <Plug>(sexp_indent_top)

  " Wrappping
  nmap <silent><buffer> <LocalLeader>i  <Plug>(sexp_round_head_wrap_list)
  xmap <silent><buffer> <LocalLeader>i  <Plug>(sexp_round_head_wrap_list)
  nmap <silent><buffer> <LocalLeader>I  <Plug>(sexp_round_tail_wrap_list)
  xmap <silent><buffer> <LocalLeader>I  <Plug>(sexp_round_tail_wrap_list)
  nmap <silent><buffer> <LocalLeader>[  <Plug>(sexp_square_head_wrap_list)
  xmap <silent><buffer> <LocalLeader>[  <Plug>(sexp_square_head_wrap_list)
  nmap <silent><buffer> <LocalLeader>]  <Plug>(sexp_square_tail_wrap_list)
  xmap <silent><buffer> <LocalLeader>]  <Plug>(sexp_square_tail_wrap_list)
  nmap <silent><buffer> <LocalLeader>{  <Plug>(sexp_curly_head_wrap_list)
  xmap <silent><buffer> <LocalLeader>{  <Plug>(sexp_curly_head_wrap_list)
  nmap <silent><buffer> <LocalLeader>}  <Plug>(sexp_curly_tail_wrap_list)
  xmap <silent><buffer> <LocalLeader>}  <Plug>(sexp_curly_tail_wrap_list)
  nmap <silent><buffer> <LocalLeader>w  <Plug>(sexp_round_head_wrap_element)
  xmap <silent><buffer> <LocalLeader>w  <Plug>(sexp_round_head_wrap_element)
  nmap <silent><buffer> <LocalLeader>W  <Plug>(sexp_round_tail_wrap_element)
  xmap <silent><buffer> <LocalLeader>W  <Plug>(sexp_round_tail_wrap_element)
  nmap <silent><buffer> <LocalLeader>e[ <Plug>(sexp_square_head_wrap_element)
  xmap <silent><buffer> <LocalLeader>e[ <Plug>(sexp_square_head_wrap_element)
  nmap <silent><buffer> <LocalLeader>e] <Plug>(sexp_square_tail_wrap_element)
  xmap <silent><buffer> <LocalLeader>e] <Plug>(sexp_square_tail_wrap_element)
  nmap <silent><buffer> <LocalLeader>e{ <Plug>(sexp_curly_head_wrap_element)
  xmap <silent><buffer> <LocalLeader>e{ <Plug>(sexp_curly_head_wrap_element)
  nmap <silent><buffer> <LocalLeader>e} <Plug>(sexp_curly_tail_wrap_element)
  xmap <silent><buffer> <LocalLeader>e} <Plug>(sexp_curly_tail_wrap_element)

  nmap <silent><buffer> <LocalLeader>@  <Plug>(sexp_splice_list)
  " nmap <silent><buffer> <LocalLeader>o  <Plug>(sexp_raise_list)
  " xmap <silent><buffer> <LocalLeader>o  <Plug>(sexp_raise_list)
  " nmap <silent><buffer> <LocalLeader>O  <Plug>(sexp_raise_element)
  " xmap <silent><buffer> <LocalLeader>O  <Plug>(sexp_raise_element)

  nmap <silent><buffer> <LocalLeader>o  <Plug>(sexp_raise_element)
  xmap <silent><buffer> <LocalLeader>o  <Plug>(sexp_raise_element)

  " Some nice keybinding from tpope/vim-sexp-mappings-for-regular-people
  nmap <buffer> B   <Plug>(sexp_move_to_prev_element_head)
  nmap <buffer> W   <Plug>(sexp_move_to_next_element_head)
  nmap <buffer> E   <Plug>(sexp_move_to_next_element_tail)
  xmap <buffer> B   <Plug>(sexp_move_to_prev_element_head)
  xmap <buffer> W   <Plug>(sexp_move_to_next_element_head)
  xmap <buffer> E   <Plug>(sexp_move_to_next_element_tail)
  omap <buffer> B   <Plug>(sexp_move_to_prev_element_head)
  omap <buffer> W   <Plug>(sexp_move_to_next_element_head)
  omap <buffer> E   <Plug>(sexp_move_to_next_element_tail)

  nmap <silent><buffer> $   <Plug>(sexp_move_to_end_of_line_with_form)
  xmap <silent><buffer> $   <Plug>(sexp_move_to_end_of_line_with_form)
  omap <silent><buffer> $   <Plug>(sexp_move_to_end_of_line_with_form)

  nmap <buffer> <I  <Plug>(sexp_insert_at_list_head)
  nmap <buffer> >I  <Plug>(sexp_insert_at_list_tail)
  nmap <buffer> <f  <Plug>(sexp_swap_list_backward)
  nmap <buffer> >f  <Plug>(sexp_swap_list_forward)
  nmap <buffer> <e  <Plug>(sexp_swap_element_backward)
  nmap <buffer> >e  <Plug>(sexp_swap_element_forward)
  nmap <buffer> >(  <Plug>(sexp_emit_head_element)
  nmap <buffer> <)  <Plug>(sexp_emit_tail_element)
  nmap <buffer> <(  <Plug>(sexp_capture_prev_element)
  nmap <buffer> >)  <Plug>(sexp_capture_next_element)

  " cs is not regular surround command
  " call s:map_sexp_wrap('e', 'cseb', '(', ')', 0)
  " call s:map_sexp_wrap('e', 'cse(', '(', ')', 0)
  " call s:map_sexp_wrap('e', 'cse)', '(', ')', 1)
  " call s:map_sexp_wrap('e', 'cse[', '[', ']', 0)
  " call s:map_sexp_wrap('e', 'cse]', '[', ']', 1)
  " call s:map_sexp_wrap('e', 'cse{', '{', '}', 0)
  " call s:map_sexp_wrap('e', 'cse}', '{', '}', 1)

  " Don't use auto pair plugins
  imap <silent><buffer> "               <Plug>(sexp_insert_double_quote)
  imap <silent><buffer> (               <Plug>(sexp_insert_opening_round)
  imap <silent><buffer> )               <Plug>(sexp_insert_closing_round)
  imap <silent><buffer> [               <Plug>(sexp_insert_opening_square)
  imap <silent><buffer> ]               <Plug>(sexp_insert_closing_square)
  imap <silent><buffer> {               <Plug>(sexp_insert_opening_curly)
  imap <silent><buffer> }               <Plug>(sexp_insert_closing_curly)

  " nmap <silent><buffer> H               <Plug>(sexp_swap_element_backward)
  " nmap <silent><buffer> L               <Plug>(sexp_swap_element_forward)

  nmap <silent><buffer> (               <Plug>(sexp_move_to_prev_bracket)
  xmap <silent><buffer> (               <Plug>(sexp_move_to_prev_bracket)
  omap <silent><buffer> (               <Plug>(sexp_move_to_prev_bracket)
  nmap <silent><buffer> )               <Plug>(sexp_move_to_next_bracket)
  xmap <silent><buffer> )               <Plug>(sexp_move_to_next_bracket)
  omap <silent><buffer> )               <Plug>(sexp_move_to_next_bracket)

  nmap <silent><buffer> H               <Plug>(sexp_move_to_list_head)
  nmap <silent><buffer> L               <Plug>(sexp_move_to_list_end)

  " nmap <silent><buffer> <LocalLeader>h  <Plug>(sexp_insert_at_list_head)
  " nmap <silent><buffer> <LocalLeader>l  <Plug>(sexp_insert_at_list_tail)
  " Using keybinding is faster than switch to insert in code
  nmap <silent><buffer> <LocalLeader>h  Ha
  nmap <silent><buffer> <LocalLeader>l  Li
  nmap <silent><buffer> I  Ha
  nmap <silent><buffer> A  Li
  nmap <silent><buffer> I  <Plug>(sexp_insert_at_list_head)
  nmap <silent><buffer> A  <Plug>(sexp_insert_at_list_tail)

  " WIP Better D
  nmap <silent><buffer> D               d)
  " nmap <silent><buffer> <C-k>           d$

  " nmap <silent><buffer> <C-]>           <Plug>(sexp_emit_head_element)
  " nmap <silent><buffer> <C-[>           <Plug>(sexp_capture_prev_element)
  nmap <silent><buffer> <C-left>        <Plug>(sexp_emit_tail_element)
  nmap <silent><buffer> <C-right>       <Plug>(sexp_capture_next_element)

  " WIP Need improve source code
  imap <silent><buffer> <C-right>       <esc><Plug>(sexp_capture_next_element)i


  nnoremap gca i#_<esc>


endfunction
"}}}2

let g:sexp_filetypes = ''
augroup VIM_SEXP_MAPPING
  autocmd!
  autocmd FileType clojure,scheme,lisp,timl,scheme call g:VIM_lisp_mappings()
  " if exists('g:sexp_loaded')
  "   autocmd FileType clojure,scheme,lisp,timl,scheme call g:VIM_lisp_mappings()
  " endif
augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" => tagbar {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" nnoremap <leader>T :TagbarToggle<CR>
let g:tagbar_position='left'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vim-tagquery {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plug 'matt-snider/vim-tagquery'

let g:tagquery_ctags_file = '~/vimwiki/.vimwiki_tags'


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" => vimux {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Prompt for a command to be run in a 20% lower tmux split, without losing
" focus on vim :)
" nnoremap <leader>vp :VimuxPromptCommand<CR>
" nnoremap <leader>v<space> :VimuxPromptCommand<CR>

" Opens a shell in the split.
nnoremap <leader>vo :call VimuxOpenRunner()<CR>

" Re-run the last command.
nnoremap <leader>vv :VimuxRunLastCommand<CR>

" Interrupt whatever process is running in the runner pane.
nnoremap <leader>vi :VimuxInterruptRunner<CR>

" Zoom the runner pane.
nnoremap <leader>vz :VimuxZoomRunner<CR>

" Clear the runner pane. (i.e. Ctrl-L)
" nnoremap <leader>vc :call VimuxSendKeys("C-l")<CR>

" Close vimux runner pane.
" nnoremap <leader>vC :VimuxCloseRunner<CR>

" An operator for sending text to Vimux.
function! VimuxOperator(type, ...) abort
  let previous = @n

  " yank target/selected text into "n
  if a:type ==# 'char' || a:type ==# 'line'
    silent! normal `[v`]"ny
  else "visual
    silent! normal gv"ny
  endif

  let input = @n

  " restore whatever was in "n before
  let @n = previous

  if exists('a:1')
    let input = substitute(input, '\n', ' ', 'g')
  endif

  " if input already ends with a newline, don't send an extra newline
  if input =~# '\n$'
    call VimuxRunCommand(input, 0)
  else
    call VimuxRunCommand(input)
  endif

endfunction

nnoremap <leader>vs :set operatorfunc=VimuxOperator<cr>g@
" nmap <leader>vss V<leader>vs
vnoremap <leader>vs :<c-u>call VimuxOperator(visualmode())<cr>
vnoremap <leader>vS :<c-u>call VimuxOperator(visualmode(), 0)<cr>

function! VimuxSendBuffer(...) abort
  let pos = winsaveview()
  let arg = exists('a:1') ? ", 0" : ""
  execute "normal! gg0vG$:\<c-u>call VimuxOperator(visualmode()".arg.")\<cr>"
  call winrestview(pos)
endfunction

command! VimuxSendBuffer
      \ call VimuxSendBuffer()

" nnoremap <buffer> <leader>vf
"   \ :call VimuxSendBuffer()<CR>
"
" nnoremap <buffer> <leader>vF
"   \ :call VimuxSendBuffer(0)<CR>


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vim-tmux {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! TmuxMappings()
  " echom "TmuxMapping"
  " setlocal comments=:#
  " setlocal commentstring=#\ %s

  nnoremap <silent><buffer> K :call tmux#man()<CR>

  " nnoremap <silent> <Plug>TmuxExec :<C-U>set opfunc=tmux#filterop<CR>g@
  " xnoremap <silent> <Plug>TmuxExec :<C-U>call tmux#filterop(visualmode())<CR>
  " nmap <buffer> g! <Plug>TmuxExec
  " nmap <buffer> g!! <Plug>TmuxExec_
  " xmap <buffer> g! <Plug>TmuxExec

  " let &cpo = s:cpo_save
  " unlet s:cpo_save
endfunction

augroup tmuxMapping
  autocmd!
  autocmd BufEnter .tmux.conf call TmuxMappings()
augroup END

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" => vim-tmux-navigator {{{1
""""""""""""""""""""""""""""""
" Quickly switch vim split with tmux panel. However, the C-\ is used for tmux
" prefix key, and <C-j> and <C-k> is used for scrolling in fzf preview panel or
" select thing still trying to figure out what is the best solution there

" Custom Key Bindings
" If you don't want the plugin to create any mappings, you can use the five
" provided functions to define your own custom maps. You will need to define
" custom mappings in your ~/.vimrc as well as update the bindings in tmux to match.

let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-j> :TmuxNavigateDown<cr>
nnoremap <silent> <C-k> :TmuxNavigateUp<cr>
nnoremap <silent> <C-l> :TmuxNavigateRight<cr>
" nnoremap <silent> <C-h> :TmuxNavigatePrevious<cr>

let g:tmux_navigator_save_on_switch = 1
let g:tmux_navigator_disable_when_zoomed = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vimwiki {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:automatic_nested_syntaxes=1
let g:vimwiki_auto_header=1
let g:vimwiki_conceal_pre=1
let g:vimwiki_conceallevel=3
let g:vimwiki_url_maxsave=15

let g:vimwiki_menu=''
let g:vimwiki_ext2syntax = {'.md': 'markdown'}
let g:vimwiki_global_ext = 0


let g:vimwiki_filetypes = ['markdown']

" :VimwikiToggleListItem
" Wiki Key Mapping {{{2
" Disable lists later after I familiar with vimwiki
let g:vimwiki_key_mappings =
      \ {
	\   'all_maps': 0,
	\   'global': 1,
	\   'headers': 1,
	\   'text_objs': 1,
	\   'table_format': 1,
	\   'table_mappings': 1,
	\   'lists': 1,
	\   'links': 0,
	\   'html': 1,
	\   'mouse': 0,
	\ }

" Wiki Config {{{2
let mainWiki = {
      \ 'path': '~/vimwiki',
      \ 'template_path': '~/vimwiki/templates/',
      \ 'template_default': 'default',
      \ 'syntax': 'markdown',
      \ 'ext': '.md',
      \ 'path_html': '~/vimwiki/html/',
      \ 'custom_wiki2html': 'vimwiki_markdown',
      \ 'template_ext': '.tpl'}
let mainWiki.nested_syntaxes = {
      \ 'c++': 'cpp',
      \ 'bash': 'sh',
      \ 'diff': 'diff',
      \ 'elixir': 'elixir',
      \ 'javascript': 'javascript',
      \ 'python': 'python',
      \ 'ruby': 'ruby',
      \ 'sh': 'sh',
      \ 'tmux': 'tmux',
      \ 'vim': 'vim',
      \ 'yaml': 'yaml',
      \ 'zsh': 'zsh'}

let g:vimwiki_list = [mainWiki]

" CreateSkeletonDiaryEntry WIP {{{2
function! CreateSkeletonDiaryEntry() abort
  if line('$') == 1 && getline(1) == ''
    read !cat ~/Sync/vimwiki/diary/template.wiki
    " The previous command pastes starting on the line _below_ the cursor (i.e.
    " line 2), so we have to delete the empty line at the top (line 1).
    execute "normal! ggdd"
    " Insert today's date
    execute "%s/DATE-GOES-HERE/" . strftime('%Y-%m-%d (%A)') . "/"
  endif
endfunction

" augroup CustomVimwikiMappings
"   autocmd!
"   " Use my own mappings for increment/decrement header level, ( and ) instead of
"   " - and =. This is because I have my own - mapping that I use to open the
"   "   current directory in defx.
"   autocmd FileType vimwiki nmap ( <Plug>VimwikiRemoveHeaderLevel
"   autocmd FileType vimwiki nmap ) <Plug>VimwikiAddHeaderLevel
"
"   " This adds some functionality that vimwiki is missing: if there is no diary
"   " entry for today yet, it creates a skeleton from a template.
"   autocmd BufEnter *vimwiki/diary/*.wiki call CreateSkeletonDiaryEntry()
" augroup end

" I don't use the diary.wiki diary index page. I prefer to use defx to browse
" the directory of diary entries. So, I'm remapping the keybinding that would
" otherwise open the diary index page. vimwiki is nice enough not to overwrite
" this when it loads.
" nnoremap <leader>wi :e ~/Sync/vimwiki/diary/<CR>

" ,w,w feels a bit awkward. I like ,wt (mnemonic: "wiki today") better.  Vimwiki
" has its own ,wt mapping, but I never use it (it opens the wiki index page in a
" tab, and I don't really use tabs).
" nmap <leader>wt <Plug>VimwikiMakeDiaryNote
"
" Wiki folding + Autogroup {{{2
" let g:vimwiki_folding = 'custom'
let g:vimwiki_folding = 'syntax'
let g:vimwiki_folding = 'list'
let g:vimwiki_folding = 'expr'
let g:vimwiki_folding = ''

function! g:CusVimWikiKeyMap() " {{{3
  echom "CusVimWikiKeyMap"

  " Links
  let g:which_key_map_space.w.o = "VimWiki Page open"
  call vimwiki#u#map_key('n', '<Space>wo', '<Plug>VimwikiFollowLink')
  call vimwiki#u#map_key('v', '<Space>wo', '<Plug>VimwikiNormalizeLinkVisualCR')

  call vimwiki#u#map_key('n', '<S-CR>', '<Plug>VimwikiSplitLink')
  call vimwiki#u#map_key('n', '<C-CR>', '<Plug>VimwikiVSplitLink')
  call vimwiki#u#map_key('n', '+', '<Plug>VimwikiNormalizeLink')
  call vimwiki#u#map_key('v', '+', '<Plug>VimwikiNormalizeLinkVisual')
  " call vimwiki#u#map_key('v', '<CR>', '<Plug>VimwikiNormalizeLinkVisualCR')
  call vimwiki#u#map_key('n', '<D-CR>', '<Plug>VimwikiTabnewLink')
  call vimwiki#u#map_key('n', '<C-S-CR>', '<Plug>VimwikiTabnewLink', 1)
  call vimwiki#u#map_key('n', '<BS>', '<Plug>VimwikiGoBackLink')
  " call vimwiki#u#map_key('n', '<TAB>', '<Plug>VimwikiNextLink')
  " call vimwiki#u#map_key('n', '<S-TAB>', '<Plug>VimwikiPrevLink')
  call vimwiki#u#map_key('n', vimwiki#vars#get_global('map_prefix').'n', '<Plug>VimwikiGoto')
  call vimwiki#u#map_key('n', vimwiki#vars#get_global('map_prefix').'d', '<Plug>VimwikiDeleteFile')
  call vimwiki#u#map_key('n', vimwiki#vars#get_global('map_prefix').'r', '<Plug>VimwikiRenameFile')
  call vimwiki#u#map_key('n', '<C-Down>', '<Plug>VimwikiDiaryNextDay')
  call vimwiki#u#map_key('n', '<C-Up>', '<Plug>VimwikiDiaryPrevDay')


  " List
  call vimwiki#u#map_key('n', 'gnt', '<Plug>VimwikiNextTask')
  call vimwiki#u#map_key('n', '<C-Space>', '<Plug>VimwikiToggleListItem')
  call vimwiki#u#map_key('n', '<C-Space>', '<Plug>VimwikiToggleListItem')
  call vimwiki#u#map_key('v', '<C-Space>', '<Plug>VimwikiToggleListItem', 1)
  if has('unix')
    call vimwiki#u#map_key('n', '<C-@>', '<Plug>VimwikiToggleListItem', 1)
    call vimwiki#u#map_key('v', '<C-@>', '<Plug>VimwikiToggleListItem', 1)
  endif
  call vimwiki#u#map_key('n', 'glx', '<Plug>VimwikiToggleRejectedListItem')
  call vimwiki#u#map_key('v', 'glx', '<Plug>VimwikiToggleRejectedListItem', 1)
  call vimwiki#u#map_key('n', 'gln', '<Plug>VimwikiIncrementListItem')
  call vimwiki#u#map_key('v', 'gln', '<Plug>VimwikiIncrementListItem', 1)
  call vimwiki#u#map_key('n', 'glp', '<Plug>VimwikiDecrementListItem')
  call vimwiki#u#map_key('v', 'glp', '<Plug>VimwikiDecrementListItem', 1)
  call vimwiki#u#map_key('i', '<C-D>', '<Plug>VimwikiDecreaseLvlSingleItem')
  call vimwiki#u#map_key('i', '<C-T>', '<Plug>VimwikiIncreaseLvlSingleItem')
  call vimwiki#u#map_key('n', 'glh', '<Plug>VimwikiDecreaseLvlSingleItem', 1)
  call vimwiki#u#map_key('n', 'gll', '<Plug>VimwikiIncreaseLvlSingleItem', 1)
  call vimwiki#u#map_key('n', 'gLh', '<Plug>VimwikiDecreaseLvlWholeItem')
  call vimwiki#u#map_key('n', 'gLH', '<Plug>VimwikiDecreaseLvlWholeItem', 1)
  call vimwiki#u#map_key('n', 'gLl', '<Plug>VimwikiIncreaseLvlWholeItem')
  call vimwiki#u#map_key('n', 'gLL', '<Plug>VimwikiIncreaseLvlWholeItem', 1)
  call vimwiki#u#map_key('i', '<C-L><C-J>', '<Plug>VimwikiListNextSymbol')
  call vimwiki#u#map_key('i', '<C-L><C-K>', '<Plug>VimwikiListPrevSymbol')
  call vimwiki#u#map_key('i', '<C-L><C-M>', '<Plug>VimwikiListToggle')
  call vimwiki#u#map_key('n', 'glr', '<Plug>VimwikiRenumberList')
  call vimwiki#u#map_key('n', 'gLr', '<Plug>VimwikiRenumberAllLists')
  call vimwiki#u#map_key('n', 'gLR', '<Plug>VimwikiRenumberAllLists', 1)
  call vimwiki#u#map_key('n', 'gl', '<Plug>VimwikiRemoveSingleCB')
  call vimwiki#u#map_key('n', 'gL', '<Plug>VimwikiRemoveCBInList')
  " Not sure if need this
  " call vimwiki#u#map_key('n', 'o', '<Plug>VimwikiListo')
  call vimwiki#u#map_key('n', 'O', '<Plug>VimwikiListO')
endfunction


function! g:MarkdownKeyMap() " {{{3

  " nunmap <Tab>
  " nunmap <S-Tab>

  nnoremap <buffer> <Tab> >>
  nnoremap <buffer> <S-Tab> <<
  vnoremap <buffer> <Tab> >
  vnoremap <buffer> <S-Tab> <
  " inoremap <Tab> >
  " inoremap <S-Tab> <
  imap <buffer> <Tab> <C-t>
  imap <buffer> <S-Tab> <C-d>
  " nnoremap <CR> <Plug>VimwikiFollowLink
  " xnoremap <CR> <Plug>VimwikiFollowLink
endfunction

function! VimwikiFoldLevelCustom(lnum) " {{{3
  let pounds = strlen(matchstr(getline(a:lnum), '^#\+'))
  if (pounds)
    return '>' . pounds  " start a fold level
  endif
  if getline(a:lnum) =~? '\v^\s*$'
    if (strlen(matchstr(getline(a:lnum + 1), '^#\+')))
      return '-1' " don't fold last blank line before header
    endif
  endif
  return '=' " return previous fold level
endfunction

" Autogroup {{{3
augroup vimWiki
  autocmd!
  " Fix wrong fold method from vim-markdown
  " autocmd FileType vimwiki set foldexpr=VimwikiFoldListLevel(v:lnum)
  " autocmd FileType vimwiki call g:CusVimWikiKeyMap()
  " autocmd FileType markdown call g:CusVimWikiKeyMap()
  " autocmd BufReadPost *.md,*.markdown call g:CusVimWikiKeyMap()

  " autocmd FileType vimwiki setlocal foldmethod=expr |
  " \ setlocal foldenable | set foldexpr=VimwikiFoldLevelCustom(v:lnum) |
  " \ call g:CusVimWikiKeyMap()

  " autocmd BufReadPost TODO.md setlocal foldmethod=indent | call g:CusVimWikiKeyMap()

augroup END


" augroup VimrcAuGroup
"   autocmd!
"   autocmd FileType vimwiki setlocal foldmethod=expr |
" 	\ setlocal foldenable | set foldexpr=VimwikiFoldLevelCustom(v:lnum)
" augroup END

""""""""""""""""""""""""""""""""""""""""""""
" => vim-markdown {{{1
" Config for vim-markdown
" Plug 'plasticboy/vim-markdown'
" Use vimWiki folding instead
let g:vim_markdown_folding_disabled = 1

" => markdown augroup {{{2
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup markdown
  autocmd!
  " autocmd BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
  " "for some reason, I have to set the filetype to ghmarkdown, and then set it to markdown, in order to get all of the syntax highlighting. cool!
  " autocmd BufNewFile,BufRead *.md,*.markdown setlocal syntax=markdown
  " autocmd BufReadPost *.md,*.markdown setlocal syntax=markdown
  " autocmd BufNewFile,BufRead *.md,*.markdown setlocal filetype=markdown
  " autocmd BufReadPost *.md,*.markdown :%foldopen!
  " autocmd FileType vimwiki set ft=markdown | set syntax=markdown | call g:CusVimWikiKeyMap()
  " autocmd BufReadPost *.md,*.markdown set ft=markdown | set syntax=markdown | call g:CusVimWikiKeyMap()
  " autocmd BufNewFile,BufRead *.md,*.markdown set syntax=markdown | call g:CusVimWikiKeyMap()
  " autocmd BufEnter,BufRead,BufNewFile *.md set filetype=markdown | call g:CusVimWikiKeyMap()




  " autocmd BufEnter,BufRead,BufNewFile *.md set filetype=markdown
  autocmd FileType markdown call g:CusVimWikiKeyMap() | call g:MarkdownKeyMap() |
	\ setlocal foldmethod=expr | set foldexpr=NestedMarkdownFolds() |
	\ echom 'fileType markdown'



  " Use expr for vim-getting-things-down folding. It is nice problem but I am
  " just using the folding syntax for now
  " foldexpr=getting_things_down#fold_expr(v:lnum)
  " autocmd BufEnter TODO.md setlocal foldmethod=expr

  " Plugin: masukomi/vim-markdown-folding
  " set foldexpr=NestedMarkdownFolds()


  autocmd BufEnter TODO.md setlocal foldmethod=expr | set foldexpr=getting_things_down#fold_expr(v:lnum)

augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => which-key {{{1
""""""""""""""""""""""""""""""
let g:which_key_centered = 0
" Don't put keygroup at start or at end
let g:which_key_group_dicts=''
let g:which_key_floating_opts = { 'row': '0', 'col': '-10', 'height': '+0', 'width': '+10'}
" See: https://github.com/liuchengxu/vim-which-key#configuration


""""""""""""""""""""""""""""""
" => yaml folds {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plug 'pedrohdz/vim-yaml-folds'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" => 000 Sample {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => WIP Tree sitter Sample {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if exists('g:plugs["nvim-treesitter"]')
  lua <<EOF
  require'nvim-treesitter.configs'.setup {
  ensure_installed = "maintained",                             -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  ignore_install = { "javascript", "zsh", "vim", "bash" },     -- List of parsers to ignore installing
  highlight = {
  enable = true,                                               -- false will disable the whole extension
  disable = { "c", "rust", "vim", "clojure", "bash", "zsh" },  -- list of language that will be disabled
  },
}
EOF

endif



" => LSP pyright {{{1
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" lua << EOF
" require'lspconfig'.pyright.setup{}
" EOF

" lua << EOF
" local nvim_lsp = require('lspconfig')
" -- Use an on_attach function to only map the following keys
" -- after the language server attaches to the current buffer
" local on_attach = function(client, bufnr)
"   local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
"   local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end
"   --Enable completion triggered by <c-x><c-o>
"   buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')
"   -- Mappings.
"   local opts = { noremap=true, silent=true }
"   -- See `:help vim.lsp.*` for documentation on any of the below functions
"   buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
"   buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
"   buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
"   buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
"   buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
"   buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
"   buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
"   buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
"   buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
"   buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
"   buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
"   buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
"   buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
"   buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
"   buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
"   buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
"   buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
" end
" -- Use a loop to conveniently call 'setup' on multiple servers and
" -- map buffer local keybindings when the language server attaches
" local servers = { "pyright", "rust_analyzer", "tsserver" }
" for _, lsp in ipairs(servers) do
"   nvim_lsp[lsp].setup { on_attach = on_attach }
" end
" EOF
