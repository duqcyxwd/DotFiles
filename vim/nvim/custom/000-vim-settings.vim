"
" __     _____ __  __   ____       _   _   _
" \ \   / /_ _|  \/  | / ___|  ___| |_| |_(_)_ __   __ _ ___
"  \ \ / / | || |\/| | \___ \ / _ \ __| __| | '_ \ / _` / __|
"   \ V /  | || |  | |  ___) |  __/ |_| |_| | | | | (_| \__ \
"    \_/  |___|_|  |_| |____/ \___|\__|\__|_|_| |_|\__, |___/
"                                                  |___/

" ------------------------------------------------------------------------------

" Chuan's Settings =>  {{{1
" remap leader key to ,

filetype indent on          " Enable filetype plugins
filetype plugin on          " Enable filetype plugins

set autoread                    " Set to auto read when a file is changed from the outside
set clipboard+=unnamed          " use system clipboard
set cmdheight=1                 " Give more space for displaying messages.
set cursorline                  " highlights line numbers (vim-airline-colornum)
set encoding=utf8               " Set utf8 as standard encoding
set ffs=unix,dos,mac            " Use Unix as the standard file type
set foldcolumn=0                " Disable fold column by default, <Space>tfc to enable it
set foldmethod=marker           " Use braces by default
set hidden                      " A buffer becomes hidden when it is abandoned
set history=700                 " Sets how many lines of history VIM has to remember
set hlsearch                    " Highlight search results
set ignorecase                  " Ignore case when searching
set incsearch                   " Makes search act like search in modern browsers
set langmenu=en
set laststatus=2                " Always show the status line
set lazyredraw                  " Don't redraw while executing macros (good performance config)
set matchtime=2                 " How many tenths of a second to blink when matching brackets
set mouse=a
set noautochdir                 " no auto dir
set noerrorbells                " No annoying sound on errors
set nojoinspaces                " No extra space when join line
set nonumber                    " No line numbers to start
set number
set relativenumber
set ruler                       " Always show current position
set scroll=4                    " Number of lines to scroll with ^U/^D
set scrolloff=15                " Keep cursor away from this many chars top/bot
set shell=zsh                   " Default shell to zsh
set showcmd
set showmatch                   " Show matching brackets when text indicator is over them
set smartcase                   " When searching try to be smart about cases
set startofline                 " When "on" the commands listed below move the cursor to the first non-blank of the line.
set timeoutlen=250              " The default timeoutlen is 1000 ms.
set updatetime=250
" set viminfo^=%                  " Remember info about open buffers on close, will reopen buffer Replaced with shada
set visualbell

" Don't use this with tabline
" set showtabline=1               " turn on tabline

" Text Related
set autoindent
set expandtab                   " Use spaces instead of tabs
set nolinebreak                 " Automatically break lines at 80 characters.
set shiftwidth=2                " 1 tab == 2 spaces
set smarttab                    " Be smart when using tabs ;)
set tabstop=2
" set textwidth=80                " 80 chars <Space>tt to toggle it
set textwidth=0                " I hope this can disable auto break line

set nowrap                " Wrap lines

set foldlevel=2           " Set to 0 fold all by default
set foldlevelstart=1      " Set the start fold level


" Edit
set backspace=eol,start,indent " Configure backspace so it acts as it should act
set whichwrap+=<,>,h,l         " Allow specified keys that move the cursor left/right to move to the previous/next line

set diffopt=internal,filler,vertical

" https://stackoverflow.com/a/29787362
set listchars=eol:¬,tab:>·,trail:~,extends:>,precedes:<,space:␣
set nolist

" Update my session setting
" Don't reuse options, empty buffers
" Hope this can fix conjure session problem
set sessionoptions=curdir,folds,tabpages,winpos
set viewoptions=cursor,folds


" * Special *

" Fixes occasional issues where Vim disables syntax highlighting because
" some plugin takes more than the default of 2 seconds to redraw the screen.
"
" See: https://github.com/vim/vim/issues/2790
set redrawtime=10000

" Set extra options when running in GUI mode
if has("gui_running")
    set guioptions-=T
    set guioptions-=e
    set t_Co=256
    set guitablabel=%M\ %t
endif


augroup i_like_folding_lol
  autocmd!
  " autocmd BufWinEnter * silent! :%foldopen!
augroup END

" auto-Load fold views
augroup AutoSaveFolds
  autocmd!
  " autocmd BufWinLeave ?* mkview
  " autocmd BufWinEnter ?* silent! loadview
augroup END


" Save last session using startify
augroup save_last_session
  autocmd!
  autocmd VimLeave * SSave! last-open-session | echom 'Save last session'
augroup END

" Return to last edit position when opening files (You want this!)
" VIM info can do samething
augroup return_to_last_edit_position
  autocmd!
  autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal! g`\"" |
        \ endif
augroup END

augroup autoReloadFile
  " Triger `autoread` when files changes on disk
  " https://unix.stackexchange.com/questions/149209/refresh-changed-content-of-file-opened-in-vim/383044#383044
  " https://vi.stackexchange.com/questions/13692/prevent-focusgained-autocmd-running-in-command-line-editing-mode
  autocmd FocusGained,BufEnter,CursorHold,CursorHoldI *
        \ if mode() !~ '\v(c|r.?|!|t)' && getcmdwintype() == '' | checktime | endif

  " Notification after file change
  " https://vi.stackexchange.com/questions/13091/autocmd-event-for-autoread
  autocmd FileChangedShellPost *
        \ echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None
augroup END

" Default =>  {{{1
" ------------------------------------------------------------------------------

source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

" Turn on the WiLd menu
set wildmenu

" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
else
    set wildignore+=.git\*,.hg\*,.svn\*
endif


" Returns true if paste mode is enabled
function! HasPaste() abort
  if &paste
    return 'PASTE MODE  '
  en
  return ''
endfunction

" Format the status line
" set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l

" Go to last file(s) if Vim is started without arguments.
" augroup reopen_last_file
"   autocmd!
"   autocmd VimLeave * nested
"         \ if (!isdirectory($HOME . "/.vim")) |
"         \ call mkdir($HOME . "/.vim") |
"         \ endif |
"         \ execute "mksession! " . $HOME . "/.vim/Session.vim"
"   autocmd VimEnter * nested
"         \ if argc() == 0 && filereadable($HOME . "/.vim/Session.vim") |
"         \ execute "source " . $HOME . "/.vim/Session.vim"
" augroup END

" Turn persistent undo on (means that you can undo even when you close a
" buffer/VIM)
if has('persistent_undo')
  set undodir=~/.vim_runtime/temp_dirs/undodir
  set undofile
endif

