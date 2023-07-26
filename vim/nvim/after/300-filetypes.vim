""""""""""""""""""""""""""""""
" => Alda {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Replaces each part call in the score (e.g. `piano:`) with a random instrument.
" This is useful for when I'm writing demo scores and I want to keep things
" interesting instead of just using the same instruments over and over again.
"
" NB: This isn't perfect. It doesn't properly handle:
" * Colons within comments or inline Clojure expressions.
" * Instrument groups, e.g. clarinet/flute/oboe:
" * Part aliases, e.g. piano "foo":, trumpet/trombone "bar":
function! RandomizeAldaInstruments() abort
  let instruments = trim(system("mktemp"), "\n")
  exe "!alda instruments > " . instruments
  exe '%s/[[:alnum:]-]\+:/\=trim(system("shuf -n1 '
        \ . escape(instruments, "/")
        \ . '"), "\n") . ":"/gc'
endfunction

augroup alda
  autocmd!
  autocmd FileType alda
        \ nnoremap <buffer> <silent> <localleader>ri
        \ :call RandomizeAldaInstruments()<cr>
augroup END



" => C# / Spark MVC {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup spark_mvc
  autocmd!
  autocmd BufNewFile,BufRead *.spark set filetype=html
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => CoffeeScript {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! CoffeeScriptFold() abort
  setl foldmethod=indent
  setl foldlevelstart=1
endfunction

augroup coffeescript_filetype_settings
  autocmd!
  autocmd FileType coffee call CoffeeScriptFold()
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Clojure {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Paul copy-pasted me this from his own vimrc on 2021-04-21.
function! ClojureTagLookup()
  let word = expand('<cword>')
  if word =~ ".*/.*"
    let sym_words = split(word, "/")
    let first_tag = taglist(sym_words[0])
    if len(first_tag) == 1
      let first_tag = first_tag[0]
      if first_tag['kind'] == 'a'
        let nsname = matchlist(first_tag['cmd'], '[\(.*\) :as .*')[1]
        let second_tag = taglist("^".sym_words[1]."$")
        let criteria = "has_key(v:val, 'namespace') && v:val['namespace'] == '".nsname."'"
        let x = filter(second_tag, criteria)
        if len(x) == 1
          let t = x[0]
          " Drop the trailing '/' from the tag's command/regex entry
          let cmd = strpart((t.cmd),0,strlen((t.cmd))-1)
          " Are we on a version of Vim that has gettagstack and
          " settagstack?
          if exists('*gettagstack')
            "TODO: Update the tagstack
          endif
          "exe 'tag '.(t.name) " This doesn't work as expected, because it doesn't jump to the tag that was just found
          " So let's 'edit' the file from the tag, and use the regex
          " to jump to the line
          exe 'e '. (t.filename)
          exe ':'. cmd
          return
        endif
      endif
    endif
    let word = sym_words[1]
  endif

  let first_tag = taglist("^".word."$")
  if len(first_tag) == 1
    exe 'tag' word
  else
    exe 'tselect' word
  endif
endfunction

augroup clojure_and_hoplon
  autocmd!
  autocmd BufNewFile,BufRead  *.cljs.hl,*.boot set filetype=clojure
  autocmd BufNewFile,BufRead  *.html.hl        set filetype=html

  " autocmd FileType clojure setlocal foldmethod=indent
  " autocmd FileType clojure setlocal foldmethod=syntax

  " Lookup the tag under cursor, accounting for ns
  " autocmd FileType clojure nnoremap <buffer> <C-]> :call ClojureTagLookup()<CR>
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => fish {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup fish_filetype_settings
  autocmd!
  autocmd Filetype fish compiler fish
  autocmd Filetype fish setlocal textwidth=80
  autocmd Filetype fish setlocal foldmethod=expr
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Go {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup golang
  autocmd!
  " that thing you type every 2 seconds when you're programming in go
  " mnemonic: error propagate (ep)
  autocmd FileType go
        \ nnoremap <buffer> <localleader>ep
        \ :exe 'normal!' "oif err != nil {\<lt>cr>return err\<lt>cr>}"<cr>

  " 2-arity version of the above
  autocmd FileType go
        \ nnoremap <buffer> <localleader>eP
        \ :exe 'normal!' "oif err != nil {\<lt>cr>return nil, err\<lt>cr>}"<cr>
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => git {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup git_buffer_settings
  autocmd!
  autocmd FileType gitcommit call setpos('.', [0, 1, 1, 0])
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => help text {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup help_text
  autocmd!
  autocmd BufEnter *.txt call s:at_help()
augroup END

function! s:at_help() abort
  if &buftype == 'help'
    nnoremap <buffer> q :q<CR>
  endif
endfunction



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => JavaScript {{{1
"""""""""""""""""""""""""""""""
function! JavaScriptFold() abort
  setl foldmethod=syntax
  setl foldlevelstart=1
  syn region foldBraces start=/{/ end=/}/ transparent fold keepend extend

  function! FoldText() abort
    return substitute(getline(v:foldstart), '{.*', '{...}', '')
  endfunction
  setl foldtext=FoldText()
endfunction

augroup javascript_filetype_settings
  autocmd!
  autocmd FileType javascript call JavaScriptFold()
  autocmd FileType javascript setl fen
  autocmd FileType javascript setl nocindent
augroup END


""""""""""""""""""""""""""""""
" => JSON {{{1
"""""""""""""""""""""""""""""""
" magically format/minify json in the current buffer
nnoremap <leader>jf :%!jq -S '.'<CR>
nnoremap <leader>jm :%!jq -c '.'<CR>
" or just the current visual selection
vnoremap <leader>jf :!jq -S '.'<CR>
vnoremap <leader>jm :!jq -c '.'<CR>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => lisp {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup lisp
  autocmd!
  autocmd BufNewFile,BufRead *.asd setlocal filetype=lisp
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Markdown: vimwiki {{{1
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

" Wiki folding + Autogroup {{{2
" let g:vimwiki_folding = 'custom'
let g:vimwiki_folding = 'syntax'
let g:vimwiki_folding = 'list'
let g:vimwiki_folding = 'expr'
let g:vimwiki_folding = ''

function! g:CusVimWikiKeyMap() " {{{3
  if exists('g:plugs["vimwiki.nvim"]')
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

  endif
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

" => Markdown {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! g:MarkdownKeyMap() " {{{2

  " nunmap <Tab>
  " nunmap <S-Tab>

  " Save tab for visual multi mode
  " nnoremap <buffer> <Tab> >>
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
" }}}2

" Config for vim-markdown
" Use masukomi/vim-markdown-folding for folding
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_conceal = 1

augroup markdownConfig
  autocmd!
  autocmd FileType markdown call g:MarkdownKeyMap() |
	\ setlocal foldmethod=expr |
  \ setlocal conceallevel=2
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Python {{{1 {{{1
""""""""""""""""""""""""""""""
let python_highlight_all = 1

augroup python_filetype_settings
  autocmd!
  autocmd FileType python syn keyword pythonDecorator True None False self
  autocmd BufNewFile,BufRead *.jinja set syntax=htmljinja
  autocmd BufNewFile,BufRead *.mako set ft=mako
augroup END


""""""""""""""""""""""""""""""
" => Prolog {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup prolog
  autocmd!
  autocmd BufNewFile,BufRead *.plt set syntax=prolog
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => quickfix {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Press q to close quickfix buffer.
augroup quickfix_settings
  autocmd!
  autocmd BufReadPost quickfix nnoremap <buffer> q :q!<CR>
augroup END




""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
