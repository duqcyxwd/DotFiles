
require'nvim-treesitter.configs'.setup {
    ensure_installed = "maintained",                             -- one of "all", "maintained" (parsers with maintainers), or a list of languages
    ignore_install = { "javascript", "zsh", "vim", "bash" },     -- List of parsers to ignore installing
    highlight = {
      enable = true,                                               -- false will disable the whole extension
      custom_capures = {
        ["enable"] = "Identifier",
      },
      disable = { "c", "rust", "vim", "clojure", "bash", "zsh" },  -- list of language that will be disabled
    },
    additional_vim_regex_highlighting = false,
    incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
      },
    },
  }
