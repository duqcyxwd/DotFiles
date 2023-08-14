local core = require("funcs.nvim_core")

return { -- Editor enhancements

  ------------------------------------------------------------------------- |
  "folke/which-key.nvim",
  { -- "jiangmiao/auto-pairs"                                               | Auto pair quote and add space
    "jiangmiao/auto-pairs",
    init = function()
      vim.g.AutoPairsMapSpace = 0 --                                        | Disable because it adds space for test('abc')
      core.autogroup({
        autopairs_config = {
          { "FileType", { "lisp", "scheme", "clojure" }, function() vim.b.AutoPairs = {} end },
          -- Disable double quote match for vim file
          { "FileType", { "vim" }, function()
            vim.b.AutoPairs = {
              ['<'] = '>',
              ['('] = ')',
              ['['] = ']',
              ["'"] = "'",
              ['"'] = '',
              ['`'] = '`',
              ['```'] = '```',
              ["'''"] = "'''"
            }
          end },

        }
      })
    end
  },

  -- Text Objedt
  --                                                                        | Cheat sheeti https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
  -- "vim-scripts/argtextobj.vim", --                                       | I think targets covers it
  { -- "wellle/targets.vim",                                                | Addition support for text object
    "wellle/targets.vim",
    init = function(...)
      -- " Target, use single quote for "'`, which is default to q
      vim.cmd(
        [[ autocmd User targets#mappings#user call targets#mappings#extend({ "'": {'quote': [{'d':"'"}, {'d':'"'}, {'d':'`'}]} }) ]])
    end
  },
  "michaeljsmith/vim-indent-object", --                                     | Text Object indent
  "tpope/vim-sleuth",                --                                     | Automatically adjusts 'shiftwidth' and 'expandtab'
  "tpope/vim-surround",
  -- "machakann/vim-sandwich", --                                           | Should be replace by vim-surround, sa -> ys, visual -> S


  "kshenoy/vim-signature",         --                                       | A plugin to place, toggle and display marks.
  "machakann/vim-highlightedyank", --                                       | Show highlight for yank
  "mbbill/undotree",
  { -- "mg979/vim-visual-multi",                                            | vim multi cursor
    "mg979/vim-visual-multi",
    lazy = true,
    event = "VeryLazy",
  },
  { -- "ntpeters/vim-better-whitespace"                                     | Only use for highlighting
    "ntpeters/vim-better-whitespace",
    init = function(...)
      vim.g.better_whitespace_enabled = 1
      vim.g.strip_whitespace_on_save = 0
    end

  },
  { -- "cappyzawa/trim.nvim",                                               | Use to auto trim whitespace
    "cappyzawa/trim.nvim",
    opts = {
      ft_blocklist = { 'diff', 'gitcommit', 'unite', 'qf', 'help', 'ctrlsf' },
      trim_on_write = true,
    }
  },
  "osyo-manga/vim-over",  --                                                | Preview replace chanage
  "preservim/tagbar",     --                                                | Create tag on fly, used by markdown
  "tpope/vim-commentary", --                                                | Gcc
  "tpope/vim-repeat",

  -- Vim motions, jump, search
  -- "unblevable/quick-scope", --, {'on': []}                               | Quick highlight for f/F | Not working with Lazy
  { -- "ggandor/lightspeed.nvim",--                                         | A motion plugin, f/F
    "ggandor/lightspeed.nvim",
    config = {
      special_keys = { },
      limit_ft_matches = 10,
    },
  },
  { -- "phaazon/hop.nvim",                                                  | Jump Everywhere
    "phaazon/hop.nvim",
    config = true,
    lazy = true,
    cmd = { "HopVerticalMW", "HopChar1", "HopChar2" }
  },

  -- " Align text
  "godlygeek/tabular", --, {'on': 'Tabularize'}
  "junegunn/vim-easy-align",

  -- " Terminal
  "kassio/neoterm",        --                                               | Terminal
  "voldikss/vim-floaterm", --                                               | #{ on: [ 'FloatermNew', 'FloatermHide', 'FloatermToggle' ] }

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
