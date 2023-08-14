return {
  -- " Languages/Filetypes

  "OrangeT/vim-csharp",
  "adimit/prolog.vim",
  "cespare/vim-toml",
  "evedovelli/rst-robotframework-syntax-vim",
  "seeamkhan/robotframework-vim",
  "kchmck/vim-coffee-script",
  "mattn/emmet-vim",
  "pangloss/vim-javascript",
  "pedrohdz/vim-yaml-folds",
  "tfnico/vim-gradle",
  "tmux-plugins/vim-tmux", --                           " Vim plugin for .tmux.conf.
  "tweekmonster/gofmt.vim",
  "zdharma-continuum/zinit-vim-syntax",
  "jparise/vim-graphql",
  "othree/xml.vim",

  -- " I don't need them yet and they overwrite my formatoptions
  -- " Plug 'rust-lang/rust.vim'
  -- " Plug 'zah/nim.vim'
  -- " Plug 'leafgarland/typescript-vim'
  -- " Plug 'udalov/kotlin-vim'

  -- " Clojure things {{{1
  -- " ------------------------------------------------------------------------------
  -- " Deprecated
  -- " Plug 'eraserhd/parinfer-rust'
  -- " Plug 'bhurlow/vim-parinfer'
  -- " Plug 'vim-scripts/paredit.vim'
  -- " Plug 'guns/vim-clojure-static'
  -- " Plug 'tpope/vim-sexp-mappings-for-regular-people'| " Tesing
  -- " m, Make next available mark

  { "Olical/conjure", ft = "clojure" }, --,                               " Add for lazy loading
  -- " Plug '~/duqcyxwd/conjure',                                         " Add for lazy loading
  { dir = "~/duqcyxwd/vim-sexp", ft = "clojure", pin = true }, --,        " My version of vim-sexp

  -- " Interesting tag jumping!
  { "tpope/vim-fireplace", ft = "clojure" },
  { "venantius/vim-cljfmt", ft = "clojure" }, --                         " uses a vim-fireplace REPL connection to only format the current file

  -- " WATCH LIST SEE IF I STILL NEED IT
  -- " Plug 'clojure-vim/acid.nvim'                           "Asynchronous Clojure Interactive Development
  -- " Plug 'clojure-vim/clojure.vim'                         " Replace guns/vim-clojure-static, code style

  -- "  vim-markdown's color is better but it is slow
  -- " Plug 'vimwiki/vimwiki'

  -- " Markdown
  -- " Plug 'jtratner/vim-flavored-markdown'
  -- " Plug 'plasticboy/vim-markdown'                      " A better markdown plugin
  -- " I just need one for folding
  -- " Plug 'rlue/vim-getting-things-down'

  { "plasticboy/vim-markdown", ft = "markdown" }, --                      " A better markdown syntax plugin than vimwiki
  { "masukomi/vim-markdown-folding", ft = "markdown"  }, --                " Can fold by header
}
