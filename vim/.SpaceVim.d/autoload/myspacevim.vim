
function! g:VIM_lisp_mappings()  "{{{1
  echom "Meow!! config vim_lisp_mappings"

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
  nmap <silent><buffer> [e              <Plug>(sexp_select_prev_element)
  xmap <silent><buffer> [e              <Plug>(sexp_select_prev_element)
  omap <silent><buffer> [e              <Plug>(sexp_select_prev_element)
  nmap <silent><buffer> ]e              <Plug>(sexp_select_next_element)
  xmap <silent><buffer> ]e              <Plug>(sexp_select_next_element)
  omap <silent><buffer> ]e              <Plug>(sexp_select_next_element)
  nmap <silent><buffer> ==              <Plug>(sexp_indent)
  nmap <silent><buffer> =-              <Plug>(sexp_indent_top)
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
  nmap <silent><buffer> <LocalLeader>h  <Plug>(sexp_insert_at_list_head)
  nmap <silent><buffer> <LocalLeader>l  <Plug>(sexp_insert_at_list_tail)
  nmap <silent><buffer> <LocalLeader>@  <Plug>(sexp_splice_list)
  nmap <silent><buffer> <LocalLeader>o  <Plug>(sexp_raise_list)
  xmap <silent><buffer> <LocalLeader>o  <Plug>(sexp_raise_list)
  nmap <silent><buffer> <LocalLeader>O  <Plug>(sexp_raise_element)
  xmap <silent><buffer> <LocalLeader>O  <Plug>(sexp_raise_element)

  " nmap <silent><buffer> <C-k>           <Plug>(sexp_swap_list_backward)
  " nmap <silent><buffer> <C-j>           <Plug>(sexp_swap_list_forward)
  nmap <silent><buffer> <C-h>           <Plug>(sexp_swap_element_backward)
  nmap <silent><buffer> <C-l>           <Plug>(sexp_swap_element_forward)

  nmap <silent><buffer> H          <Plug>(sexp_swap_element_backward)
  nmap <silent><buffer> L           <Plug>(sexp_swap_element_forward)

  " nmap <silent><buffer> <C-]>           <Plug>(sexp_emit_head_element)
  " nmap <silent><buffer> <C-[>           <Plug>(sexp_capture_prev_element)
  nmap <silent><buffer> <C-left>        <Plug>(sexp_emit_tail_element)
  nmap <silent><buffer> <C-right>       <Plug>(sexp_capture_next_element)

  "Beautiful thing in vim-sexp
  nmap <silent><buffer> <C-k> daf

  " nmap <C-right> ,>
  " nmap <C-left> ,<lt>
endfunction


function! g:ToggleVerbose() "{{{1
  if !&verbose
    set verbosefile=~/.log/vim/verbose.log
    set verbose=15
  else
    set verbose=0
    set verbosefile=
  endif
endfunction

function! g:UnmapDelimitMate() "{{{1
  inoremap <silent><buffer> " "
  inoremap <silent><buffer> { {
endfunction

function! g:myspacevim#before() abort "{{{1
  " echom "Meow!! myspacevim before"
  " map local leader is used by vim-sexp
  let g:maplocalleader  = ','
  let g:paredit_leader = ','


  " VIM Sexp Config {{{2
  " Disable mapping hooks
  let g:sexp_filetypes = ''

  " source /Users/EYONDUU/.cache/vimfiles/repos/github.com/guns/vim-sexp/autoload/sexp.vim

  " nmap <silent><buffer> ,o :call sexp#docount(v:count, 'sexp#raise', 'n', 'sexp#select_current_element', 'n', 1)<CR>
  " nmap <silent><buffer> <C-i> :call sexp#docount(v:count, 'sexp#raise', 'n', 'sexp#select_current_element', 'n', 1)<CR>



  " }}}2

  augroup VIM_SEXP_MAPPING
    " echom "Meow!! VIM_SEXP_MAPPING"
    autocmd!
    autocmd FileType clojure,scheme,lisp,timl,scheme call g:VIM_lisp_mappings()
  augroup END

  augroup VIM_VIM_FILE
    autocmd!
    autocmd FileType vim call g:UnmapDelimitMate()
  augroup END


endfunction

function! g:myspacevim#after() abort "{{{1
  echom "Meow!! myspacevim after"





  " Section: Slash Key mappings {{{2
  "--------------------------------------------------------------------------

  nmap \w :setlocal wrap!<CR>:setlocal wrap?<CR>
  nmap \l :set foldcolumn=2<CR>
  nmap \L :set foldcolumn=0<CR>
  nmap \s :set foldmethod=syntax<CR>
  nmap \S :set foldmethod=marker<CR>

  " Section: Vim options {{{2
  "--------------------------------------------------------------------------

  set timeoutlen=500               " By default timeoutlen is 1000 ms
  set nonumber                     " No line numbers to start
  set clipboard+=unnamed           " use system clipboard
  set foldmethod=marker            " Use braces by default
  set autochdir


  set scroll=4                     " Number of lines to scroll with ^U/^D
  set scrolloff=15                 " Keep cursor away from this many chars top/bot


  " Section: Space Key mappings {{{2
  "--------------------------------------------------------------------------

  " call SpaceVim#mapping#def('nmap', '<leader>-', '<Plug>AirlineSelectPrevTab', 'Switch to previous airline tag', '', 'window previous')
  " call SpaceVim#mapping#def('nmap', '<leader>+', '<Plug>AirlineSelectNextTab', 'Switch to next airline tag', '', 'window next')
  " call SpaceVim#mapping#space#def('nmap', ['-'], '<Plug>AirlineSelectPrevTab', 'window previous', 0)
  " call SpaceVim#mapping#space#def('nmap', ['+'], '<Plug>AirlineSelectNextTab', 'window next', 0)
  
  call SpaceVim#mapping#space#def( 'nnoremap', ['t', 'r'], 'setlocal rnu!',                                   'toggle relative line number', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['b', 'd'], 'bdelete',                                         'delete current buffer', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['q', 'w'], 'wq',                                              'Save file and quit', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['c', 'c'], 'lcd %:p:h<CR>:pwd<CR>',                           'cd into directory of current file', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['c', 'r'], 'lcd `=system("git root")` <CR>:pwd<CR>',          'cd into project root directory', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['c', 'd'], "call fzf#run({'source': 'fd -t d -d 4', 'sink': 'lcd', 'window': 'below 40new' })", 'cd into by using fd search', 1)

  call SpaceVim#mapping#space#def( 'vnoremap', ['g', 'o'], "'<,'>GBrowse", 'Open current line in remote repo', 1)
  " WIP
  call SpaceVim#mapping#space#def( 'nnoremap', ['g', 'o'], "GBrowse", 'Open current file in remote repo', 1)
  " vnoremap <SPC>go :'<,'>Gbrowse<CR>

  let g:_spacevim_mappings_space.y = {'name' : '+Yank'}
  call SpaceVim#mapping#space#def( 'nnoremap', ['y', 'f'], "let @+ = expand('%:p')", 'Copy current file path', 1)


  let g:_spacevim_mappings_space.t.T = {'name' : '+General Toggle'}
  let g:_spacevim_mappings_space.T.T = {'name' : '+General Toggle'}

  call SpaceVim#mapping#space#def( 'nnoremap', ['t', 'T', 's'], "set foldmethod=syntax", 'Set foldmethod syntax', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['t', 'T', 'm'], "set foldmethod=marker", 'Set foldmethod marker', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['t', 'T', 'c'], "set foldcolumn=2", 'Set column to 2', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['t', 'T', 'C'], "set foldcolumn=0", 'Set column to 0', 1)


  call SpaceVim#mapping#space#def( 'nnoremap', ['T', 'T', 's'], "set foldmethod=syntax", 'Set foldmethod syntax', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['T', 'T', 'm'], "set foldmethod=marker", 'Set foldmethod marker', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['T', 'T', 'c'], "set foldcolumn=2", 'Set column to 2', 1)
  call SpaceVim#mapping#space#def( 'nnoremap', ['T', 'T', 'C'], "set foldcolumn=0", 'Set column to 0', 1)


  call SpaceVim#mapping#space#def( 'nnoremap', ['\'], "FzfMenu CustomKeyMaps", 'fuzzy find custom key bindings', 1)

  " Section: fzf related settings {{{2

  call SpaceVim#mapping#space#def('nnoremap', ['s', 's'],  'BLines',      'FZF Lines in the current buffer', 1)
  call SpaceVim#mapping#space#def('nnoremap', ['s', 'b'],  'Lines',       'FZF Lines in all open buffer', 1)
  call SpaceVim#mapping#space#def('nnoremap', ['f', 'r'],  'History',     'FZF Open recent file', 1)
  call SpaceVim#mapping#space#def('nnoremap', ['p', 'f'],  'Files',       'FZF Open project file', 1)
  call SpaceVim#mapping#space#def('nnoremap', ['<Space>'], 'Commands',    'FZF Commands', 1)


  " nnoremap <C-p> :Files<cr>

  " This is the default extra key bindings
  let g:fzf_action = {
        \ 'ctrl-t': 'tab split',
        \ 'ctrl-x': 'split',
        \ 'ctrl-v': 'vsplit' }

  " Default fzf layout
  " - down / up / left / right
  let g:fzf_layout = { 'down': '~40%' }


  let g:fzf_files_options= '--preview "bat {} 2> /dev/null | head 100" --bind "?:toggle-preview"'
  let g:fzf_history_dir = '~/.local/share/fzf-history'

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


  tnoremap <expr> <Esc> (&filetype == "fzf") ? "<Esc>" : "<c-\><c-n>"

  if has("nvim")
    " au TermOpen * tnoremap <buffer> <Esc> <c-\><c-n>
    " au FileType fzf tunmap <buffer> <Esc>
    " au TermOpen * tnoremap <Esc> <c-\><c-n>
    " au FileType fzf tunmap <Esc>
    tnoremap <expr> <Esc> (&filetype == "fzf") ? "<Esc>" : "<c-\><c-n>"

  endif


  " ^^^^^FZF option

  " Section: Other Key mappings {{{2
  "--------------------------------------------------------------------------
  nnoremap gj <C-W>j
  nnoremap gk <C-W>k
  nnoremap gh <C-W>h
  nnoremap gl <C-W>l

  nnoremap <silent><buffer> <C-j>           i<CR><Esc>

  map <Leader>ew :e <C-R>=expand("%:p:h") . "/" <CR>


  " Enter for following
  nnoremap <CR> za                 

  " Section: Plugins settings {{{2
  "--------------------------------------------------------------------------
  

  let g:spacevim_relativenumber = 1
  let g:startify_session_persistence = 1
  let g:startify_session_autoload    = 1
  

  let g:airline_theme='simple'
  let ayucolor="dark"


  " Don't commit this line
  let g:fugitive_gitlab_domains = ['https://git.rosetta.ericssondevops.com']

  " vim sexp vs paredit
  let g:sexp_enable_insert_mode_mappings = 0

  " cd `=system("git root")`<CR>:pwd<CR>
  command CDC cd %:p:h             " CDC = Change to Directory of Current file


endfunction



function g:Meow() "{{{1
  echom "Meow!"
endfunction
