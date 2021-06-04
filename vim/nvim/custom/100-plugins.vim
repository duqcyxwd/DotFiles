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

" Plug 'clojure-vim/acid.nvim'|                      "Asynchronous Clojure Interactive Development
Plug 'Olical/conjure'

" Plug 'guns/vim-clojure-static'
Plug 'clojure-vim/clojure.vim'|                      " Replace guns/vim-clojure-static
" Plug 'guns/vim-sexp'
" Plug 'duqcyxwd/vim-sexp'                           "My version of vim-sexp
" Or load it locally
" source /Users/EYONDUU/duqcyxwd/vim-sexp/autoload/sexp.vim
" source /Users/EYONDUU/duqcyxwd/vim-sexp/plugin/sexp.vim
Plug '~/duqcyxwd/vim-sexp'
" Plug 'tpope/vim-sexp-mappings-for-regular-people'| " Tesing

" Interesting tag jumping!
Plug 'tpope/vim-fireplace'
Plug 'venantius/vim-cljfmt', { 'for': 'clojure' }
Plug 'luochen1990/rainbow'

" VimWiki with Markdown {{{1
" ------------------------------------------------------------------------------
Plug 'vimwiki/vimwiki'

" Markdown
" Plug 'jtratner/vim-flavored-markdown'
Plug 'plasticboy/vim-markdown'
" I just need one for folding
" Plug 'rlue/vim-getting-things-down'
Plug 'masukomi/vim-markdown-folding'


" Not working
"Plug 'matt-snider/vim-tagquery', { 'do': 'bash install.sh' }

" Git {{{1
" ------------------------------------------------------------------------------
Plug 'jreybert/vimagit'
Plug 'lambdalisue/gina.vim'
Plug 'shumphrey/fugitive-gitlab.vim'|                  "Open gitlab link
Plug 'tpope/vim-fugitive'|                             "Raw git command, Gblame
Plug 'airblade/vim-gitgutter'|                         "shows a git diff in the sign column



" Editor enhancements {{{1
" ------------------------------------------------------------------------------

Plug 'AndrewRadev/undoquit.vim'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
" Plug 'junegunn/fzf.vim'
Plug 'duqcyxwd/fzf.vim'|                           " My quick fix for fzf vim
Plug 'junegunn/limelight.vim'
Plug 'kassio/neoterm'
Plug 'kshenoy/vim-signature'
Plug 'liuchengxu/vim-which-key'
Plug 'ludovicchabant/vim-gutentags'|               "Good way to generate tags
Plug 'machakann/vim-sandwich'
Plug 'mg979/vim-visual-multi'|                     "vim multi cursor
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'ntpeters/vim-better-whitespace'
Plug 'osyo-manga/vim-over'
Plug 'preservim/tagbar'|                           "Create tag on fly, used by markdown
Plug 'schickling/vim-bufonly'
Plug 'tpope/vim-commentary'|                       "Gcc
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'|                           "Automatically adjusts 'shiftwidth' and 'expandtab'
Plug 'tpope/vim-surround'
Plug 'unblevable/quick-scope'|                     "Quick highlight for f/F


" Telescope
Plug 'nvim-lua/popup.nvim'| "required by telescope
Plug 'nvim-lua/plenary.nvim'| "required by telescope
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-telescope/telescope-symbols.nvim'

" Align text
Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align'

Plug 'pseewald/vim-anyfold'

" Languages/Filetypes {{{1
" ------------------------------------------------------------------------------

Plug 'OrangeT/vim-csharp'
Plug 'adimit/prolog.vim'
Plug 'cespare/vim-toml'
Plug 'christoomey/vim-tmux-navigator'
Plug 'evedovelli/rst-robotframework-syntax-vim'
Plug 'kchmck/vim-coffee-script'
Plug 'leafgarland/typescript-vim'
Plug 'mattn/emmet-vim'
Plug 'pangloss/vim-javascript'
Plug 'pedrohdz/vim-yaml-folds'
Plug 'rust-lang/rust.vim'
Plug 'seeamkhan/robotframework-vim'
Plug 'tfnico/vim-gradle'
Plug 'tmux-plugins/vim-tmux'
Plug 'tweekmonster/gofmt.vim'
Plug 'udalov/kotlin-vim'
Plug 'zah/nim.vim'
Plug 'zinit-zsh/zinit-vim-syntax'

" Misc+ {{{1
" ------------------------------------------------------------------------------

Plug 'chrisbra/unicode.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/gv.vim'|                                "A git commit browser.
Plug 'mattn/vim-findroot'
Plug 'mhinz/vim-startify'
Plug 'szw/vim-dict'|                                   "A dict client
Plug 'tpope/vim-eunuch'|                               "Vim sugar from tpope
Plug 'tyru/open-browser.vim'

" WIP {{{1
" ------------------------------------------------------------------------------

Plug 'yuki-yano/fzf-preview.vim', { 'branch': 'release/remote', 'do': ':UpdateRemotePlugins' }
" preview buffer tags, not working in clojure yet

" Disable default highlight before use this
" Not working yet
" Plug 'folke/todo-comments.nvim'

Plug 'mbbill/undotree'
Plug 'daveyarwood/vim-alda'|                             "editing and playing Alda scores.
Plug 'habamax/vim-asciidoctor'
Plug 'junegunn/vader.vim'|                               "use Vader to test Vimscript.
Plug 'vlime/vlime', {'rtp': 'vim/'}|                     "Common Lisp dev environment

" WIP
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " Better syntax highlight

Plug 'neovim/nvim-lspconfig'


" }}}1

"Default
" ------------------------------------------------------------------------------
" editor enhancements {{{1
" ------------------------------------------------------------------------------

Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'benmills/vimux'
Plug 'dyng/ctrlsf.vim'
Plug 'liuchengxu/vista.vim'
Plug 'machakann/vim-highlightedyank'
Plug 'tpope/vim-unimpaired'


" Not sure why this plugin: https://github.com/tpope/vim-unimpaired/blob/master/plugin/unimpaired.vim
" Plug 'vim-scripts/gitignore'

"}}}1
call plug#end()

