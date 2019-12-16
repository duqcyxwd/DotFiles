function! myspacevim#before() abort
    " useful macros I use the most
    nmap \w :setlocal wrap!<CR>:setlocal wrap?<CR>
    nmap \I :SPUpdate <CR>

    nnoremap gj <C-W>j
    nnoremap gk <C-W>k
    nnoremap gh <C-W>h
    nnoremap gl <C-W>l

    " By default timeoutlen is 1000 ms
    set timeoutlen=500

    let g:spacevim_relativenumber = 1

    call SpaceVim#custom#SPCGroupName(['G'], '+TestGroup')
    call SpaceVim#custom#SPC('nore', ['G', 't'], 'echom 1', 'echomessage 1', 1)

    set clipboard+=unnamed      " use system clipboard


    set foldmethod=syntax
    silent! set foldmethod=marker " Use braces by default


    set scroll=4                " Number of lines to scroll with ^U/^D
    set scrolloff=15            " Keep cursor away from this many chars top/bot

    " Why not use the space or return keys to toggle folds?
    " nnoremap <space> za
    nnoremap <CR> za

endfunction

function! myspacevim#after() abort
endfunction



