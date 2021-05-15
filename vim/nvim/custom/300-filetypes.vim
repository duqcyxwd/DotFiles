""""""""""""""""""""""""""""""
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
" => fish {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup fish_filetype_settings
  autocmd!
  autocmd Filetype fish compiler fish
  autocmd Filetype fish setlocal textwidth=80
  autocmd Filetype fish setlocal foldmethod=expr
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => git {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup git_buffer_settings
  autocmd!
  autocmd FileType gitcommit call setpos('.', [0, 1, 1, 0])
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => io {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup iolang
  autocmd!
  " I have the 'extras/SyntaxHighlighters/Vim' directory of the IoLanguage/io
  " installed as a plugin, but it's just the syntax definition and that's it.
  " This line is necessary in order to actually recognize *.io files as Io
  " source files.
  autocmd BufNewFile,BufRead *.io setlocal filetype=io
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => lisp {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup lisp
  autocmd!
  autocmd BufNewFile,BufRead *.asd setlocal filetype=lisp
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => markdown {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup markdown
  autocmd!
  " autocmd BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
  " "for some reason, I have to set the filetype to ghmarkdown, and then set it to markdown, in order to get all of the syntax highlighting. cool!
  " autocmd BufNewFile,BufRead *.md,*.markdown setlocal syntax=markdown
  " autocmd BufReadPost *.md,*.markdown setlocal syntax=markdown
  " autocmd BufNewFile,BufRead *.md,*.markdown setlocal filetype=markdown
  " autocmd BufReadPost *.md,*.markdown :%foldopen!
  " autocmd FileType vimwiki set ft=markdown
augroup END

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => vimWiki {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! s:vimWikiMap()

  let g:which_key_map_space.w.w = "VimWiki Index Page"
  " nmap <Space>ww <Plug>VimwikiIndex

  " call vimwiki#u#map_key('n', '<CR>', '<Plug>VimwikiFollowLink')
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

augroup vimWiki
  autocmd!
  " Fix wrong fold method from vim-markdown
  autocmd FileType vimwiki set foldexpr=VimwikiFoldListLevel(v:lnum)
  autocmd FileType vimwiki call s:vimWikiMap()
  autocmd FileType markdown call s:vimWikiMap()
  " autocmd BufReadPost *.md,*.markdown call s:vimWikiMap()
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
    " enable 'q' = quit
    nnoremap <buffer> q :q<CR>
  endif
endfunction



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => quickfix {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Press q to close quickfix buffer.
augroup quickfix_settings
  autocmd!
  autocmd BufReadPost quickfix nnoremap <buffer> q :q!<CR>
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

  " Lookup the tag under cursor, accounting for ns
  autocmd FileType clojure
        \ nnoremap <buffer> <C-]>
        \ :call ClojureTagLookup()<cr>
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Prolog {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup prolog
  autocmd!
  autocmd BufNewFile,BufRead *.plt set syntax=prolog
augroup END


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => C# / Spark MVC {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
augroup spark_mvc
  autocmd!
  autocmd BufNewFile,BufRead *.spark set filetype=html
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


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vim {{{1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup VimSource
  autocmd FileType vim
        \ autocmd! VimSource BufWritePost <buffer> set foldmethod=marker
augroup END

