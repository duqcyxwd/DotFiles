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
Plug 'rrethy/vim-hexokinase', { 'do': 'make hexokinase' }                "displaying the colours in the file

Plug 'nvim-lualine/lualine.nvim'
Plug 'arkav/lualine-lsp-progress'                                        " It is working but only on inactive line
Plug 'kyazdani42/nvim-web-devicons'                                      " Recommended (for coloured icons)
Plug 'akinsho/bufferline.nvim'

" Theme
Plug 'mhartington/oceanic-next'
Plug 'ayu-theme/ayu-vim'
Plug 'sonph/onehalf', {'rtp': 'vim/' }
Plug 'cormacrelf/vim-colors-github'
Plug 'EdenEast/nightfox.nvim'
Plug 'dracula/vim',   { 'as': 'dracula' }


Plug 'junegunn/limelight.vim'
Plug 'junegunn/goyo.vim'
Plug 'folke/twilight.nvim'                                               " Dims inactive portions build on tree sitter
Plug 'folke/zen-mode.nvim'

" Buffers and Windows+ {{{1
" ------------------------------------------------------------------------------
"
Plug 'simeji/winresizer', { 'on': 'WinResizerStartResize' }
Plug 'Asheq/close-buffers.vim'
Plug 'AndrewRadev/undoquit.vim'
Plug 'jeetsukumaran/vim-buffergator'                                    " open a window listing all buffers

" Editor enhancements {{{1
" ------------------------------------------------------------------------------

" Plug 'liuchengxu/vi|-which-key'
Plug 'folke/which-key.nvim'

Plug 'jiangmiao/auto-pairs'
Plug '/usr/local/opt/fzf'                                               " Use the fzf installed by brew
" Plug 'junegunn/fzf',     { 'dir': '~/.fzf', 'do': './install --all' } " fzf Download fzf repo and install from source code???
Plug '~/duqcyxwd/fzf.vim', {'on': []}                                   " My quick fix for fzf vim, e.g FFFiles
Plug 'ibhagwan/fzf-lua'                                                 " fzf written in lua            TODO Explain why I need it

Plug 'wellle/targets.vim'                                               " Addition support for text object
Plug 'michaeljsmith/vim-indent-object'                                  "Text Object indent
" Cheat sheeti https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
"
Plug 'kyazdani42/nvim-web-devicons'                                     " optional for icon support
Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins', 'on': 'Defx' } " File browser
Plug 'kshenoy/vim-signature'                                            " A plugin to place, toggle and display marks.
Plug 'machakann/vim-highlightedyank'                                    " Show highlight for yank
Plug 'machakann/vim-sandwich'
Plug 'mbbill/undotree'
Plug 'mg979/vim-visual-multi', {'on': []}                               " vim multi cursor
Plug 'ntpeters/vim-better-whitespace'
Plug 'osyo-manga/vim-over'                                              " Preview replace chanage
Plug 'preservim/tagbar'                                                 " Create tag on fly, used by markdown
Plug 'tpope/vim-commentary'                                             " Gcc
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sleuth'                                                 " Automatically adjusts 'shiftwidth' and 'expandtab'
Plug 'tpope/vim-surround'
Plug 'unblevable/quick-scope', {'on': []}                               " Quick highlight for f/F
Plug 'phaazon/hop.nvim'                                                 " Jump Everywhere
Plug 'Yggdroot/indentLine', {'on': []}

" Tree sitter
" Plug 'nvim-treesitter/nvim-treesitter', {'commit': 'e7bdcee167ae41295a3e99ad460ae80d2bb961d7'} " Better syntax highlight
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}              " Better syntax highlight

Plug 'nvim-treesitter/playground'
Plug '~/duqcyxwd/nvim-treesitter-textobjects'                            " Testing my textobjects for comments
" Plug 'theHamsta/nvim-treesitter-pairs',                                 " Create your own pair objects using tree-sitter queries!
" Plug 'nvim-treesitter/nvim-treesitter-refactor',                        " Provide some highlight navigation based on treesitter

Plug 'kyazdani42/nvim-tree.lua'                                         "A File Explorer For Neovim Written In Lua

Plug 'simrat39/symbols-outline.nvim'
" Plug '~/duqcyxwd/coc.nvim'                                              " Fix the compete problem with cmp
" 2023-03-17 disable coc, I don't need it
" " Plug 'neoclide/coc.nvim', {'branch': 'release'}


" Telescope
" Plug 'nvim-lua/popup.nvim'                        "required by telescope
" Plug 'nvim-lua/plenary.nvim'                      "required by telescope
" Plug 'nvim-telescope/telescope.nvim'
" Plug 'nvim-telescope/telescope-symbols.nvim'

" Align text
Plug 'godlygeek/tabular', {'on': 'Tabularize'}
Plug 'junegunn/vim-easy-align'

" Terminal
Plug 'kassio/neoterm'                                        " Terminal
Plug 'voldikss/vim-floaterm', #{ on: [ 'FloatermNew', 'FloatermHide', 'FloatermToggle' ] }


" Completion {{{1

function! s:nvim_cmp()
  Plug 'hrsh7th/nvim-cmp'
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/cmp-cmdline'
  Plug 'onsails/lspkind-nvim'                          " Add icon for cmp

  " For luasnip users.
  Plug 'L3MON4D3/LuaSnip'                              " Follows lsp protocol
  Plug 'saadparwaiz1/cmp_luasnip'
  Plug 'rafamadriz/friendly-snippets'                  " Snippet collection
endfunction

call s:nvim_cmp()


" LSP+ {{{1
" ------------------------------------------------------------------------------
" https://medium.com/prodhacker/how-to-set-up-neovim-0-5-modern-plugins-lsp-treesitter-etc-542c3d9c9887
Plug 'neovim/nvim-lspconfig'
" Plug 'neovim/nvim-lspconfig',    {'on': []}
Plug 'tami5/lspsaga.nvim'
" Plug 'tami5/lspsaga.nvim',       {'on': []}

" Languages/Filetypes {{{1
" ------------------------------------------------------------------------------

Plug 'OrangeT/vim-csharp'
Plug 'adimit/prolog.vim'
Plug 'cespare/vim-toml'
Plug 'evedovelli/rst-robotframework-syntax-vim'
Plug 'seeamkhan/robotframework-vim'
Plug 'kchmck/vim-coffee-script'
Plug 'mattn/emmet-vim'
Plug 'pangloss/vim-javascript'
Plug 'pedrohdz/vim-yaml-folds'
Plug 'tfnico/vim-gradle'
Plug 'tmux-plugins/vim-tmux'                           " Vim plugin for .tmux.conf.
Plug 'tweekmonster/gofmt.vim'
Plug 'zdharma-continuum/zinit-vim-syntax'
Plug 'jparise/vim-graphql'

Plug 'othree/xml.vim'

" I don't need them yet and they overwrite my formatoptions
" Plug 'rust-lang/rust.vim'
" Plug 'zah/nim.vim'
" Plug 'leafgarland/typescript-vim'
" Plug 'udalov/kotlin-vim'

" Clojure things {{{1
" ------------------------------------------------------------------------------
" Deprecated
" Plug 'eraserhd/parinfer-rust'
" Plug 'bhurlow/vim-parinfer'
" Plug 'vim-scripts/paredit.vim'
" Plug 'guns/vim-clojure-static'
" Plug 'tpope/vim-sexp-mappings-for-regular-people'| " Tesing
" m, Make next available mark


Plug 'Olical/conjure',       { 'for': 'clojure' }                        " Add for lazy loading
" Plug '~/duqcyxwd/conjure',  { 'for': 'clojure' }                       " Add for lazy loading
Plug '~/duqcyxwd/vim-sexp',  {'for': 'clojure'}                          " My version of vim-sexp


" Interesting tag jumping!
Plug 'tpope/vim-fireplace',  { 'for': 'clojure' }
Plug 'venantius/vim-cljfmt', { 'for': 'clojure' }                        " uses a vim-fireplace REPL connection to only format the current file

" WATCH LIST SEE IF I STILL NEED IT
" Plug 'clojure-vim/acid.nvim'                           "Asynchronous Clojure Interactive Development
" Plug 'clojure-vim/clojure.vim'                         " Replace guns/vim-clojure-static, code style

" VimWiki with Markdown {{{1
" ------------------------------------------------------------------------------
"  vim-markdown's color is better but it is slow
" Plug 'vimwiki/vimwiki'

" Markdown
" Plug 'jtratner/vim-flavored-markdown'
" Plug 'plasticboy/vim-markdown'                      " A better markdown plugin
" I just need one for folding
" Plug 'rlue/vim-getting-things-down'

Plug 'plasticboy/vim-markdown'                      " A better markdown syntax plugin than vimwiki
Plug 'masukomi/vim-markdown-folding'                " Can fold by header


" Git {{{1
" ------------------------------------------------------------------------------
Plug 'jreybert/vimagit'                                " ViMagit to stage changes
Plug 'lambdalisue/gina.vim', { 'on': 'Gina' }
Plug 'tpope/vim-fugitive'                              " Raw git command, Gblame
Plug 'shumphrey/fugitive-gitlab.vim'                   " Open private git repo link
Plug 'tpope/vim-rhubarb'                               " Open github link
" Plug 'airblade/vim-gitgutter'                        " shows a git diff in the sign column
Plug 'lewis6991/gitsigns.nvim'                         " git decorations
Plug 'nvim-lua/plenary.nvim'                           " Required by gitsigns
Plug 'junegunn/gv.vim'                                 " A git better commit browser.
Plug 'itchyny/vim-gitbranch'                           " WIP Add branch for auto save session name

" WIP
Plug 'nvim-lua/plenary.nvim'
Plug 'ruifm/gitlinker.nvim'



" Misc+ {{{1
" ------------------------------------------------------------------------------

Plug 'chrisbra/unicode.vim'
Plug 'mattn/vim-findroot'
Plug 'mhinz/vim-startify'
Plug 'szw/vim-dict'                                     " A dict client :Dict Hello
Plug 'tpope/vim-eunuch'                                 " UNIX shell commands
Plug 'norcalli/nvim_utils'                              " Nvim utils functions
Plug 'christoomey/vim-tmux-navigator'                   " Quickly switch vim split with tmux panel.
Plug 'tyru/open-browser.vim', { 'on': ['OpenBrowser', '<Plug>(openbrowser-', 'OpenBrowserSmartSearch'] }

" Disabled+ {{{1
" ------------------------------------------------------------------------------
" Plug 'nathom/filetype.nvim'                             " Increase filetype loading speed "Disabled for nvim 0.9
" Plug 'romgrk/nvim-treesitter-context'                   "Disabled for nvim 0.9
" WIP {{{1
" ------------------------------------------------------------------------------

Plug 'tommcdo/vim-exchange'                          " Exchange lines, cx/X/cxx/cxc, works with '.'

" Useage: ColorHighlight
Plug 'chrisbra/Colorizer'                                 " View log file :ColorToggle  TEST: r! exa --color=always --icons -l,

" Plug 'j-hui/fidget.nvim'                      " Show lsp progress, NOT WORKING

" }}}1

"Default
" ------------------------------------------------------------------------------
" editor enhancements {{{1
" ------------------------------------------------------------------------------

Plug 'benmills/vimux'

"}}}1
call plug#end()

" Cleanup
" UpdateRemotePlugins
