local core = require("funcs.nvim_core")

return {
  ------------------------------------------------------------------------- |
  "folke/which-key.nvim",
  { -- "jiangmiao/auto-pairs",                                              | Auto pair quote and add space
    "jiangmiao/auto-pairs",
    init = function()
      vim.g.AutoPairsMapSpace = 0 --                                        | Disable because it adds space for test('abc')
      core.autogroup({
        autopairs_config = {
          {
            "FileType",
            { "lisp", "scheme", "clojure" },
            function()
              vim.b.AutoPairs = {}
            end,
          },
          -- Disable double quote match for vim file
          {
            "FileType",
            { "vim" },
            function()
              vim.b.AutoPairs = {
                ["<"] = ">",
                ["("] = ")",
                ["["] = "]",
                ["'"] = "'",
                ['"'] = "",
                ["`"] = "`",
                ["```"] = "```",
                ["'''"] = "'''",
              }
            end,
          },
        },
      })
    end,
  },
  "tpope/vim-sleuth", --                                     | Automatically adjusts 'shiftwidth' and 'expandtab'

  -- Text Objedt
  -- Treesitter also provides text object
  --                                                                        | Cheat sheeti https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
  { -- "wellle/targets.vim",                                                | Addition Text Object For DEFAULT OPERATOR, Pair/Quote/Separator/Argument/Tag e.g di' or diq, vi', vaq
    "wellle/targets.vim",
  },
  "michaeljsmith/vim-indent-object", --                                     | Indent Text Object, e.g vii, vai
  -- "machakann/vim-sandwich",       --                                     | Should be replace by vim-surround, sa -> ys, visual -> S
  -- "tpope/vim-surround",           --                                     | provides mappings to easily delete, change and add such surroundings in pairs, e.g cs"'

  -- Text Objedt Operator
  {                -- "kylechui/nvim-surround",                                            | Surround Operator, use for normal: ys/ds/cs/ + '"`f visual:S
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup({
        -- Configuration here, or leave empty to use defaults
        aliases = {
          ["a"] = ">",
          ["b"] = ")",
          ["B"] = "}",
          ["r"] = "]",
          ["q"] = { '"', "'", "`" },
          ["s"] = { " ", "}", "]", ")", ">", '"', "'", "`" },
        },
        surrounds = {
          ["("] = {
            add = { "(", ")" },
          },
          [")"] = {
            add = { "( ", " )" },
          },
          ["{"] = {
            add = { "{", "}" },
          },
          ["}"] = {
            add = { "{ ", " }" },
          },
          ["<"] = {
            add = { "<", ">" },
          },
          [">"] = {
            add = { "< ", " >" },
          },
          ["["] = {
            add = { "[", "]" },
          },
          ["]"] = {
            add = { "[ ", " ]" },
          },
          ["'"] = {
            add = { "'", "'" },
          },
          ['"'] = {
            add = { '"', '"' },
          },
          ["`"] = {
            add = { "`", "`" },
          },
          invalid_key_behavior = {
            add = function(char)
              if not char or char:find("%c") then
                return nil
              end
              return { { char }, { char } }
            end,
            find = function(char)
              if not char or char:find("%c") then
                return nil
              end
              return M.get_selection({
                pattern = vim.pesc(char) .. ".-" .. vim.pesc(char),
              })
            end,
            delete = function(char)
              if not char or char:find("%c") then
                return nil
              end
              return M.get_selections({
                char = char,
                pattern = "^(.)().-(.)()$",
              })
            end,
          },
        },
      })
    end,
  },

  "tommcdo/vim-exchange",  --             | Exchange lines, cx/X/cxx/cxc, works with '.'

  "kshenoy/vim-signature", --                                               | A plugin to place, toggle and display marks.
  "mbbill/undotree",
  {                        -- "mg979/vim-visual-multi",                                            | vim multi cursor
    "mg979/vim-visual-multi",
    lazy = true,
    event = "VeryLazy",
  },
  { -- "ntpeters/vim-better-whitespace"                                     | Only use for highlighting
    "ntpeters/vim-better-whitespace",
    init = function(...)
      vim.g.better_whitespace_enabled = 1
      vim.g.strip_whitespace_on_save = 0
    end,
  },
  { -- "cappyzawa/trim.nvim",                                               | Use to auto trim whitespace
    "cappyzawa/trim.nvim",
    opts = {
      ft_blocklist = { "diff", "gitcommit", "unite", "qf", "help", "ctrlsf" },
      trim_on_write = true,
    },
  },
  "preservim/tagbar",     --                                                | Create tag on fly, used by markdown
  "tpope/vim-commentary", --                                                | Gcc
  "tpope/vim-repeat",

  -- Highlight
  "machakann/vim-highlightedyank", --                                       | Show highlight for yank
  "osyo-manga/vim-over",           --                                       | Preview replace chanage

  -- Vim motions, jump, search
  -- "unblevable/quick-scope", --, {'on': []}                               | Quick highlight for f/F | Not working with Lazy
  { -- "ggandor/lightspeed.nvim",                                           | A motion plugin, f/F
    "ggandor/lightspeed.nvim",
    enabled = false,
    config = {
      special_keys = {},
      limit_ft_matches = 10,
    },
  },
  { -- "folke/flash.nvim",                                                  | Replacement for light speed, support Tree sitter search, e.g f/F/sS, + search / <Space>v
    "folke/flash.nvim",
    event = "VeryLazy",
    enabled = true,
    opts = {
      modes = {
        char = {
          jump_labels = false,
        },
      },
    },
    -- stylua: ignore
    keys = {
      { "s", mode = { "n", "o", "x" }, function() require("flash").jump() end,              desc = "Flash", },
      { "r", mode = { "o" },           function() require("flash").remote() end,            desc = "Remote Flash", },
      { "R", mode = { "o", "x" },      function() require("flash").treesitter_search() end, desc = "Treesitter Search", },
    },
  },
  { -- "phaazon/hop.nvim",                                                  | Jump Everywhere
    "phaazon/hop.nvim",
    config = true,
    lazy = true,
    cmd = { "HopVerticalMW", "HopChar1", "HopChar2" },
  },

  -- " Align Text
  "godlygeek/tabular", --, {'on': 'Tabularize'}
  "junegunn/vim-easy-align",

  -- " Terminal
  "kassio/neoterm",        --                                                      | Terminal
  "voldikss/vim-floaterm", --                                               | #{ on: [ 'FloatermNew', 'FloatermHide', 'FloatermToggle' ] }
  {                        -- "chrisgrieser/nvim-origami",                                         | Fold with relentless elegance. h/l
    "chrisgrieser/nvim-origami",
    event = "BufReadPost", -- later or on keypress would prevent saving folds
    opts = true,           -- needed even when using default config
  },
  {
    "anuvyklack/pretty-fold.nvim", --                                       | Setup my folding text
    config = function()
      require("pretty-fold").setup({})
      require("pretty-fold").ft_setup("lua", {
        fill_char = "-",
        add_close_pattern = false, -- true, 'last_line' or false
      })
    end,
  },

  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make',
    init = function()
      -- You dont need to set any of these options. These are the default ones. Only
      -- the loading is important
      require('telescope').setup {
        extensions = {
          fzf = {
            fuzzy = true,                   -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true,    -- override the file sorter
            case_mode = "smart_case",       -- or "ignore_case" or "respect_case"
            -- the default case_mode is "smart_case"
          }
        }
      }
      -- To get fzf loaded and working with telescope, you need to call
      -- load_extension, somewhere after setup function:
      require('telescope').load_extension('fzf')
    end
  },
  { -- "nvim-telescope/telescope.nvim",
    "nvim-telescope/telescope.nvim",
    commit = vim.fn.has("nvim-0.9.0") == 0 and "057ee0f8783" or nil,
    cmd = "Telescope",
    version = false, -- telescope did only one release, so use HEAD for now
    config = true,
    opts = {
      extensions = {
        fzf = {
          fuzzy = true,               -- false will only do exact matching
          override_generic_sorter = true, -- override the generic sorter
          override_file_sorter = true, -- override the file sorter
          case_mode = "smart_case",   -- or "ignore_case" or "respect_case"
          -- the default case_mode is "smart_case"
        }
      },
      defaults = {
        prompt_prefix = " ",
        selection_caret = " ",
        mappings = {
          i = {
            ["<C-h>"] = "which_key",
            ["<esc>"] = function(...)
              return require("telescope.actions").close(...)
            end,
            ["<c-t>"] = function(...)
              return require("trouble.providers.telescope").open_with_trouble(...)
            end,
            ["<a-t>"] = function(...)
              return require("trouble.providers.telescope").open_selected_with_trouble(...)
            end,
            ["<Down>"] = function(...)
              return require("telescope.actions").cycle_history_next(...)
            end,
            ["<Up>"] = function(...)
              return require("telescope.actions").cycle_history_prev(...)
            end,
            ["<C-f>"] = function(...)
              return require("telescope.actions").results_scrolling_down(...)
            end,
            ["<C-b>"] = function(...)
              return require("telescope.actions").results_scrolling_up(...)
            end,
            ["<C-j>"] = function(...)
              return require("telescope.actions").preview_scrolling_down(...)
            end,
            ["<C-k>"] = function(...)
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
  },

}
