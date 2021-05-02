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
" Plug 'eraserhd/parinfer-rust'
" Plug 'bhurlow/vim-parinfer'
" Plug 'vim-scripts/paredit.vim'

" Plug 'clojure-vim/acid.nvim'|                      "Asynchronous Clojure Interactive Development
Plug 'Olical/conjure'

Plug 'clojure-vim/clojure.vim'|                      " Replace guns/vim-clojure-static
Plug 'guns/vim-sexp'
Plug 'duqcyxwd/vim-sexp'
" Plug 'tpope/vim-sexp-mappings-for-regular-people'| " Tesing

" Or load it locally
Plug 'tpope/vim-fireplace'
Plug 'venantius/vim-cljfmt', { 'for': 'clojure' }
Plug 'luochen1990/rainbow'

" Editor enhancements {{{2
Plug 'AndrewRadev/undoquit.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-easy-align'
Plug 'liuchengxu/vim-which-key'
Plug 'mg979/vim-visual-multi'
Plug 'osyo-manga/vim-over'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

" Language {{{2
Plug 'evedovelli/rst-robotframework-syntax-vim'
Plug 'seeamkhan/robotframework-vim'
Plug 'zinit-zsh/zinit-vim-syntax'

" Misc+ {{{2
Plug 'mattn/vim-findroot'
Plug 'mhinz/vim-startify'
Plug 'shumphrey/fugitive-gitlab.vim'
Plug 'tpope/vim-fugitive'

" WIP {{{2
Plug 'yuki-yano/fzf-preview.vim'
Plug 'junegunn/goyo.vim'
" https://github.com/clojure-vim/clj-refactor.nvim

"Default {{{1
" ------------------------------------------------------------------------------


" editor enhancements {{{2
" ------------------------------------------------------------------------------
Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'airblade/vim-gitgutter'
Plug 'benmills/vimux'
Plug 'christoomey/vim-tmux-navigator'
Plug 'dyng/ctrlsf.vim'
Plug 'embear/vim-localvimrc'
Plug 'gerw/vim-HiLinkTrace'
Plug 'godlygeek/tabular'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'jiangmiao/auto-pairs'
Plug 'kassio/neoterm'
Plug 'kshenoy/vim-signature'
Plug 'liuchengxu/vista.vim'
" Plug 'ludovicchabant/vim-gutentags'
Plug 'machakann/vim-highlightedyank'
Plug 'machakann/vim-sandwich'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'ntpeters/vim-better-whitespace'
Plug 'schickling/vim-bufonly'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-unimpaired'
Plug 'unblevable/quick-scope'
" Plug 'vim-scripts/gitignore'

" other languages/filetypes {{{2
" ------------------------------------------------------------------------------
Plug 'adimit/prolog.vim'
Plug 'aklt/plantuml-syntax'
Plug 'bakpakin/fennel.vim'
Plug 'cespare/vim-toml'
Plug 'dag/vim-fish'
Plug 'dart-lang/dart-vim-plugin'
Plug 'daveyarwood/vim-alda'
Plug 'derekwyatt/vim-scala'
Plug 'digitaltoad/vim-pug'
Plug 'dleonard0/pony-vim-syntax'
Plug 'dpwright/vim-tup'
Plug 'elixir-lang/vim-elixir'
Plug 'ElmCast/elm-vim'
Plug 'habamax/vim-asciidoctor'
Plug 'idris-hackers/idris-vim'
Plug 'iloginow/vim-stylus'
Plug 'IoLanguage/io', { 'rtp': 'extras/SyntaxHighlighters/Vim' }
Plug 'jceb/vim-orgmode'
Plug 'jdonaldson/vaxe'
Plug 'jtratner/vim-flavored-markdown'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'junegunn/vader.vim'
Plug 'kchmck/vim-coffee-script'
Plug 'keith/swift.vim'
Plug 'leafgarland/typescript-vim'
Plug 'lfe/vim-lfe'
Plug 'mattn/emmet-vim'
Plug 'matze/vim-lilypond'
Plug 'mustache/vim-mustache-handlebars'
Plug 'OrangeT/vim-csharp'
Plug 'pangloss/vim-javascript'
Plug 'PProvost/vim-ps1'
Plug 'raichoo/purescript-vim'
Plug 'reasonml-editor/vim-reason-plus'
Plug 'rhysd/vim-crystal'
Plug 'rust-lang/rust.vim'
Plug 'stephencelis/vim-mml'
Plug 'tfnico/vim-gradle'
Plug 'tmux-plugins/vim-tmux'
Plug 'tweekmonster/gofmt.vim'
Plug 'udalov/kotlin-vim'
Plug 'vlime/vlime', {'rtp': 'vim/'}
Plug 'wlangstroth/vim-racket'
Plug 'zah/nim.vim'

" misc {{{2
" ------------------------------------------------------------------------------
Plug 'chrisbra/unicode.vim'
Plug 'jreybert/vimagit'
Plug 'junegunn/gv.vim'
Plug 'lambdalisue/gina.vim'
Plug 'mileszs/ack.vim'
Plug 'szw/vim-dict'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-rhubarb'
Plug 'tyru/open-browser.vim'
" Plug 'Valloric/ListToggle'
Plug 'vimwiki/vimwiki'

"}}}1
call plug#end()
