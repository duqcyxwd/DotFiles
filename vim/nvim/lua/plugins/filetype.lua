return {

  ------------------------------------------------------ |
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
  "tweekmonster/gofmt.vim",
  "zdharma-continuum/zinit-vim-syntax", --               | zinit
  "jparise/vim-graphql",--                               | graphql
  { "othree/xml.vim", ft = "xml" },
  "tmux-plugins/vim-tmux", --                            | Vim plugin for .tmux.conf.
  "towolf/vim-helm",       --                            | Helm


  {
    "plasticboy/vim-markdown", --                        | A better markdown syntax plugin than vimwiki
    --                                                   |  vim-markdown's color is better but it is slow
    ft = "markdown",
    config = function()
      vim.g.vim_markdown_folding_disabled = 1
      vim.g.vim_markdown_conceal = 1
    end
  },
  -- {
  --   "masukomi/vim-markdown-folding", --                  | Can fold by header [VS treesitter one]
  --   ft = "markdown"
  -- },

  -- " Clojure things {{{1
  -- " ------------------------------------------------------------------------------
  -- " Deprecated
  -- " Plug 'eraserhd/parinfer-rust'
  -- " Plug 'bhurlow/vim-parinfer'
  -- " Plug 'vim-scripts/paredit.vim'
  -- " Plug 'guns/vim-clojure-static'
  -- " Plug 'tpope/vim-sexp-mappings-for-regular-people'| " Tesing
  -- " m, Make next available mark

  { "Olical/conjure",            ft = "clojure" },             --,                               " Add for lazy loading
  -- " Plug '~/duqcyxwd/conjure',                                         " Add for lazy loading
  { dir = "~/duqcyxwd/vim-sexp", ft = "clojure", pin = true }, --,        " My version of vim-sexp

  -- " Interesting tag jumping!
  { "tpope/vim-fireplace",       ft = "clojure" },
  { "venantius/vim-cljfmt",      ft = "clojure" }, --                         " uses a vim-fireplace REPL connection to only format the current file

  -- " WATCH LIST SEE IF I STILL NEED IT
  -- " Plug 'clojure-vim/acid.nvim'                           "Asynchronous Clojure Interactive Development
  -- " Plug 'clojure-vim/clojure.vim'                         " Replace guns/vim-clojure-static, code style

}
