" /***********************************************/
" /*         ____  _             _               */
" /*        |  _ \| |_   _  __ _(_)_ __          */
" /*        | |_) | | | | |/ _` | | '_ \         */
" /*        |  __/| | |_| | (_| | | | | |        */
" /*        |_|   |_|\__,_|\__, |_|_| |_|        */
" /*                       |___/                 */
" /***********************************************/

"
call plug#begin('$HOME/.local/share/nvim/plug')

"New for chuan {{{1
" ------------------------------------------------------------------------------

" Aesthetic {{{2
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

" Clojure things {{{2
" ------------------------------------------------------------------------------
" Plug 'eraserhd/parinfer-rust'
" Plug 'bhurlow/vim-parinfer'
" Plug 'vim-scripts/paredit.vim'

" Plug 'clojure-vim/acid.nvim'|                      "Asynchronous Clojure Interactive Development
Plug 'Olical/conjure'

Plug 'clojure-vim/clojure.vim'|                      " Replace guns/vim-clojure-static
" Plug 'guns/vim-sexp'
" Plug 'duqcyxwd/vim-sexp'                           "My version of vim-sexp
" Or load it locally
source /Users/EYONDUU/duqcyxwd/vim-sexp/autoload/sexp.vim
source /Users/EYONDUU/duqcyxwd/vim-sexp/plugin/sexp.vim
" Plug 'tpope/vim-sexp-mappings-for-regular-people'| " Tesing

Plug 'tpope/vim-fireplace'
Plug 'venantius/vim-cljfmt', { 'for': 'clojure' }
Plug 'luochen1990/rainbow'

" Editor enhancements {{{2
" ------------------------------------------------------------------------------

Plug 'AndrewRadev/undoquit.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/limelight.vim'
Plug 'kassio/neoterm'
Plug 'kshenoy/vim-signature'
Plug 'liuchengxu/vim-which-key'
Plug 'machakann/vim-sandwich'
Plug 'mg979/vim-visual-multi'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'ntpeters/vim-better-whitespace'
Plug 'osyo-manga/vim-over'
Plug 'schickling/vim-bufonly'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-surround'
Plug 'unblevable/quick-scope'


" Align text
Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align'

" VimWiki
Plug 'vimwiki/vimwiki'
Plug 'preservim/tagbar'
" Not working
"Plug 'matt-snider/vim-tagquery', { 'do': 'bash install.sh' }


" Markdown
" Plug 'jtratner/vim-flavored-markdown'
" Plug 'godlygeek/tabular' " Required by vim-markdown
Plug 'plasticboy/vim-markdown'

" Languages/Filetypes {{{2
" ------------------------------------------------------------------------------

Plug 'OrangeT/vim-csharp'
Plug 'adimit/prolog.vim'
Plug 'cespare/vim-toml'
Plug 'christoomey/vim-tmux-navigator'
Plug 'evedovelli/rst-robotframework-syntax-vim'
Plug 'kchmck/vim-coffee-script'
Plug 'keith/swift.vim'
Plug 'leafgarland/typescript-vim'
Plug 'mattn/emmet-vim'
Plug 'pangloss/vim-javascript'
Plug 'rust-lang/rust.vim'
Plug 'seeamkhan/robotframework-vim'
Plug 'tfnico/vim-gradle'
Plug 'tmux-plugins/vim-tmux'
Plug 'tweekmonster/gofmt.vim'
Plug 'udalov/kotlin-vim'
Plug 'zah/nim.vim'
Plug 'zinit-zsh/zinit-vim-syntax'

" Misc+ {{{2
" ------------------------------------------------------------------------------

Plug 'chrisbra/unicode.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/gv.vim'
Plug 'mattn/vim-findroot'
Plug 'mhinz/vim-startify'
Plug 'shumphrey/fugitive-gitlab.vim'
Plug 'szw/vim-dict'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tyru/open-browser.vim'

" WIP {{{2
" ------------------------------------------------------------------------------

Plug 'yuki-yano/fzf-preview.vim'
" https://github.com/clojure-vim/clj-refactor.nvim

"Default {{{1
" ------------------------------------------------------------------------------
" editor enhancements {{{2
" ------------------------------------------------------------------------------

Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'airblade/vim-gitgutter'
Plug 'benmills/vimux'
Plug 'dyng/ctrlsf.vim'
Plug 'embear/vim-localvimrc'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'liuchengxu/vista.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'machakann/vim-highlightedyank'
Plug 'tpope/vim-unimpaired'
" Not sure why this plugin: https://github.com/tpope/vim-unimpaired/blob/master/plugin/unimpaired.vim
" Plug 'vim-scripts/gitignore'

" other languages/filetypes {{{2
" ------------------------------------------------------------------------------

Plug 'daveyarwood/vim-alda'
Plug 'habamax/vim-asciidoctor'
Plug 'junegunn/vader.vim'
Plug 'vlime/vlime', {'rtp': 'vim/'}

" misc {{{2
" ------------------------------------------------------------------------------

Plug 'jreybert/vimagit'
Plug 'lambdalisue/gina.vim'

"}}}1
call plug#end()
