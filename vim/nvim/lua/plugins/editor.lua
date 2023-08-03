return { -- Editor enhancements

  ------------------------------------------------------------------------- |
  "folke/which-key.nvim",

  "jiangmiao/auto-pairs",
  "wellle/targets.vim",              --                                     | Addition support for text object
  "michaeljsmith/vim-indent-object", --                                     | Text Object indent
  -- " Cheat sheeti https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
  -- "
  -- Plug 'Shougo/defx.nvim',                                               | { 'do': ':UpdateRemotePlugins', 'on': 'Defx' } " File browser
  "kshenoy/vim-signature",         --                                       | A plugin to place, toggle and display marks.
  "machakann/vim-highlightedyank", --                                       | Show highlight for yank
  "machakann/vim-sandwich",        --
  "mbbill/undotree",               --
  { -- "mg979/vim-visual-multi",                                            | vim multi cursor
    "mg979/vim-visual-multi",
    lazy = true,
    event = "VeryLazy",
  },
  { -- "ntpeters/vim-better-whitespace"                                     | Only use for highlighting
    "ntpeters/vim-better-whitespace",
    init = function (...)
      vim.g.better_whitespace_enabled=1
      vim.g.strip_whitespace_on_save=0
    end

  },
  {
    -- "cappyzawa/trim.nvim",                                               | Use to auto trim whitespace
    "cappyzawa/trim.nvim",
    opts = {
      ft_blocklist = {'diff', 'gitcommit', 'unite', 'qf', 'help', 'ctrlsf'},
      trim_on_write = true,
    }
  },
  "osyo-manga/vim-over",            --                                      | Preview replace chanage
  "preservim/tagbar",               --                                      | Create tag on fly, used by markdown
  "tpope/vim-commentary",           --                                      | Gcc
  "tpope/vim-repeat",
  "tpope/vim-sleuth",               --                                      | Automatically adjusts 'shiftwidth' and 'expandtab'
  "tpope/vim-surround",             --
  -- "unblevable/quick-scope", --, {'on': []}                               | Quick highlight for f/F
  "phaazon/hop.nvim",               --                                      | Jump Everywhere

  -- " Align text
  "godlygeek/tabular", --, {'on': 'Tabularize'}
  "junegunn/vim-easy-align",

  -- " Terminal
  "kassio/neoterm",        --                                               | Terminal
  "voldikss/vim-floaterm", --,                                              | #{ on: [ 'FloatermNew', 'FloatermHide', 'FloatermToggle' ] }
  {                        -- "nvim-telescope/telescope.nvim",
    "nvim-telescope/telescope.nvim",
    commit = vim.fn.has("nvim-0.9.0") == 0 and "057ee0f8783" or nil,
    cmd = "Telescope",
    version = false, -- telescope did only one release, so use HEAD for now
    opts = {
      defaults = {
        prompt_prefix = " ",
        selection_caret = " ",
        mappings = {
          i = {
            ["<esc>"] = function(...)
              return require("telescope.actions").close(...)
            end,
            ["<c-t>"] = function(...)
              return require("trouble.providers.telescope").open_with_trouble(...)
            end,
            ["<a-t>"] = function(...)
              return require("trouble.providers.telescope").open_selected_with_trouble(...)
            end,
            ["<C-Down>"] = function(...)
              return require("telescope.actions").cycle_history_next(...)
            end,
            ["<C-Up>"] = function(...)
              return require("telescope.actions").cycle_history_prev(...)
            end,
            ["<C-f>"] = function(...)
              return require("telescope.actions").preview_scrolling_down(...)
            end,
            ["<C-b>"] = function(...)
              return require("telescope.actions").preview_scrolling_up(...)
            end,
          },
          n = {
            ["q"] = function(...)
              return require("telescope.actions").close(...)
            end,
          },
        },
      },
    },
  }
}
