" /***********************************************/
" /*         ____  _             _               */
" /*        |  _ \| |_   _  __ _(_)_ __          */
" /*        | |_) | | | | |/ _` | | '_ \         */
" /*        |  __/| | |_| | (_| | | | | |        */
" /*        |_|   |_|\__,_|\__, |_|_| |_|        */
" /*                       |___/                 */
" /***********************************************/

"
call plug#begin('$XDG_DATA_HOME/nvim-plug')

" Aesthetic {{{1
" ------------------------------------------------------------------------------
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }
Plug 'dracula/vim', { 'as': 'dracula' }
" Plug 'vim-scripts/mayansmoke'

"theme
Plug 'mhartington/oceanic-next'
Plug 'ayu-theme/ayu-vim'
Plug 'sonph/onehalf', {'rtp': 'vim/'}
Plug 'cormacrelf/vim-colors-github'

" Clojure things {{{1
" ------------------------------------------------------------------------------
" Plug 'eraserhd/parinfer-rust'
" Plug 'bhurlow/vim-parinfer'
" Plug 'vim-scripts/paredit.vim'

" Plug 'clojure-vim/acid.nvim'|                        "Asynchronous Clojure Interactive Development
Plug 'Olical/conjure',  { 'for': 'clojure' }           " Add for lazy loading
" Plug '~/duqcyxwd/conjure',  { 'for': 'clojure' }     " Add for lazy loading

" Plug 'guns/vim-clojure-static'
Plug 'clojure-vim/clojure.vim'|                      " Replace guns/vim-clojure-static, code style
Plug '~/duqcyxwd/vim-sexp'                           " My version of vim-sexp

" Plug 'tpope/vim-sexp-mappings-for-regular-people'| " Tesing
" m, Make next available mark


" Interesting tag jumping!
Plug 'tpope/vim-fireplace',  { 'for': 'clojure' }
Plug 'venantius/vim-cljfmt', { 'for': 'clojure' }|   " uses a vim-fireplace REPL connection to only format the current file
Plug 'luochen1990/rainbow'

" VimWiki with Markdown {{{1
" ------------------------------------------------------------------------------
"  vim-markdown's color is better but it is slow
" Plug 'vimwiki/vimwiki'

" Markdown
" Plug 'jtratner/vim-flavored-markdown'
Plug 'plasticboy/vim-markdown'                      " A better markdown plugin
" I just need one for folding
" Plug 'rlue/vim-getting-things-down'
" Plug 'masukomi/vim-markdown-folding'


" Not working
"Plug 'matt-snider/vim-tagquery', { 'do': 'bash install.sh' }

" Git {{{1
" ------------------------------------------------------------------------------
Plug 'jreybert/vimagit'                                " ViMagit to stage changes
Plug 'lambdalisue/gina.vim'
Plug 'tpope/vim-fugitive'|                             " Raw git command, Gblame
Plug 'shumphrey/fugitive-gitlab.vim'|                  " Open gitlab link
Plug 'tpope/vim-rhubarb'                               " Open github link
Plug 'airblade/vim-gitgutter'|                         " shows a git diff in the sign column
Plug 'junegunn/gv.vim'|                                " A git commit browser.



" Editor enhancements {{{1
" ------------------------------------------------------------------------------


" Plug 'liuchengxu/vim-which-key'
Plug 'folke/which-key.nvim'

Plug 'AndrewRadev/undoquit.vim'
Plug 'jeetsukumaran/vim-buffergator'|                       " open a window listing all buffers
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug '~/duqcyxwd/fzf.vim'
" Plug 'junegunn/fzf.vim'
" Plug 'duqcyxwd/fzf.vim'|                                  " My quick fix for fzf vim
Plug 'ibhagwan/fzf-lua'
Plug 'kyazdani42/nvim-web-devicons'                         " optional for icon support

Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }|  " File browser
Plug 'junegunn/limelight.vim'
Plug 'kassio/neoterm'|                                      " Terminal
Plug 'kshenoy/vim-signature'|                               " A plugin to place, toggle and display marks.
Plug 'machakann/vim-highlightedyank'|                       " Show highlight for yank
Plug 'machakann/vim-sandwich'
Plug 'mbbill/undotree'
Plug 'mg979/vim-visual-multi'|                              " vim multi cursor
Plug 'ntpeters/vim-better-whitespace'
Plug 'osyo-manga/vim-over'|                                 " Preview replace chanage
Plug 'preservim/tagbar'|                                    " Create tag on fly, used by markdown
Plug 'schickling/vim-bufonly'|                              " Delete all the buffers except the current buffer.
Plug 'tpope/vim-commentary'|                                " Gcc
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'|                                    " Automatically adjusts 'shiftwidth' and 'expandtab'
Plug 'tpope/vim-surround'
Plug 'unblevable/quick-scope'|                              " Quick highlight for f/F
Plug 'phaazon/hop.nvim'|                                    " Jump Everywhere

Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'tpope/vim-endwise'|                                   " Add endif endfunction for some language
" Somehow it cause problem on exists
" Plug 'ludovicchabant/vim-gutentags'|               "Good way to generate tags
"
" Telescope
" Plug 'nvim-lua/popup.nvim'|                        "required by telescope
" Plug 'nvim-lua/plenary.nvim'|                      "required by telescope
" Plug 'nvim-telescope/telescope.nvim'
" Plug 'nvim-telescope/telescope-symbols.nvim'

" Align text
Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align'

" Completion {{{1
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-cmdline'
Plug 'hrsh7th/nvim-cmp'

" For vsnip users.
Plug 'hrsh7th/cmp-vsnip'
Plug 'hrsh7th/vim-vsnip'

" Languages/Filetypes {{{1
" ------------------------------------------------------------------------------

Plug 'OrangeT/vim-csharp'
Plug 'adimit/prolog.vim'
Plug 'cespare/vim-toml'
Plug 'christoomey/vim-tmux-navigator'
Plug 'evedovelli/rst-robotframework-syntax-vim'
Plug 'seeamkhan/robotframework-vim'
Plug 'kchmck/vim-coffee-script'
Plug 'leafgarland/typescript-vim'
Plug 'mattn/emmet-vim'
Plug 'pangloss/vim-javascript'
Plug 'pedrohdz/vim-yaml-folds'
Plug 'rust-lang/rust.vim'
Plug 'tfnico/vim-gradle'
Plug 'tmux-plugins/vim-tmux'
Plug 'tweekmonster/gofmt.vim'
Plug 'udalov/kotlin-vim'
Plug 'zah/nim.vim'
" Plug 'zinit-zsh/zinit-vim-syntax'
Plug 'zdharma-continuum/zinit-vim-syntax'

" Misc+ {{{1
" ------------------------------------------------------------------------------

Plug 'chrisbra/unicode.vim'
Plug 'junegunn/goyo.vim'
Plug 'mattn/vim-findroot'
Plug 'mhinz/vim-startify'
Plug 'szw/vim-dict'|                                   "A dict client
Plug 'tpope/vim-eunuch'|                               "Vim sugar from tpope
Plug 'tyru/open-browser.vim'

" WIP {{{1
" ------------------------------------------------------------------------------

Plug 'wesQ3/vim-windowswap'
Plug 'Olical/aniseed'


" https://medium.com/prodhacker/how-to-set-up-neovim-0-5-modern-plugins-lsp-treesitter-etc-542c3d9c9887
Plug 'neovim/nvim-lspconfig'
" Plug 'glepnir/lspsaga.nvim'
Plug 'tami5/lspsaga.nvim'
Plug 'nvim-lua/lsp-status.nvim'

Plug 'norcalli/nvim_utils'

" WIP
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " Better syntax highlight


" preview buffer tags, not working in clojure yet
Plug 'yuki-yano/fzf-preview.vim', { 'branch': 'release/remote', 'do': ':UpdateRemotePlugins' }

" Disable default highlight before use this
" Not working yet
" Plug 'folke/todo-comments.nvim'

" Plug 'daveyarwood/vim-alda'|                             "editing and playing Alda scores.
" Plug 'habamax/vim-asciidoctor'
" Plug 'junegunn/vader.vim'|                               "use Vader to test Vimscript.
Plug 'vlime/vlime', {'rtp': 'vim/'}|                     "Common Lisp dev environment



" }}}1

"Default
" ------------------------------------------------------------------------------
" editor enhancements {{{1
" ------------------------------------------------------------------------------

Plug 'benmills/vimux'
Plug 'dyng/ctrlsf.vim'
Plug 'liuchengxu/vista.vim'
" TODO Rview keybinding
" Plug 'tpope/vim-unimpaired'


" Not sure why this plugin: https://github.com/tpope/vim-unimpaired/blob/master/plugin/unimpaired.vim
" Plug 'vim-scripts/gitignore'

"}}}1
call plug#end()

