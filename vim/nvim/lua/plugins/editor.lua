local core = require("funcs.nvim_core")

return {
  ------------------------------------------------------------------------- |
  -- Keys mappings
  "folke/which-key.nvim",
  {
    "jiangmiao/auto-pairs", --                                              | Auto pair quote and add space
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
  "tpope/vim-sleuth", --                                                    | Automatically adjusts 'shiftwidth' and 'expandtab'

  -- Text Object Operator (d/c/y/x)
  {
    "kylechui/nvim-surround", --                                            | Surround Operator, use for normal: ys/ds/cs/ + '"`f visual:S
    --                                                                      | Replace "tpope/vim-surround"
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
  "tommcdo/vim-exchange",  --                                               | Exchange lines, cx/X/cxx/cxc, works with '.'
  "tpope/vim-commentary", --                                                | Gcc
  "tpope/vim-repeat",

  -- Text Object
  -- Treesitter also provides text object
  {
    "wellle/targets.vim", --                                                | Addition Text Object For DEFAULT OPERATOR, Pair/Quote/Separator/Argument/Tag e.g di' or diq, vi', vaq
    --                                                                      | Cheat Sheet https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
  },
  "michaeljsmith/vim-indent-object", --                                     | Indent Text Object, e.g vii, vai

  -- Highlight for action
  "machakann/vim-highlightedyank", --                                       | Show highlight for yank
  "osyo-manga/vim-over",           --                                       | Preview replace chanage

  -- Vim motions, jump, search
  { -- "ggandor/lightspeed.nvim",                                           | A motion plugin, f/F
    "ggandor/lightspeed.nvim",
    enabled = false,
    config = {
      special_keys = {},
      limit_ft_matches = 10,
    },
  },
  { "folke/flash.nvim", --                                                  | Replacement for light speed, e.g f/F/sS, + search / <Space>v
    --                                                                      | support Tree sitter search,
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
  {
    "chrisgrieser/nvim-origami", --                                         | Motion for fold: h/l
    event = "BufReadPost", -- later or on keypress would prevent saving folds
    opts = true,           -- needed even when using default config
  },

  -- Block Text Editor
  {
    "mg979/vim-visual-multi", --                                            | vim multi cursor
    lazy = true,
    event = "VeryLazy",
  },
  {
    "junegunn/vim-easy-align" --                                            | ga*,
  },
  "godlygeek/tabular", --                                                   | {'on': 'Tabularize'}

  -- Terminal
  "kassio/neoterm", --                                                      | Terminal
  "voldikss/vim-floaterm", --                                               | #{ on: [ 'FloatermNew', 'FloatermHide', 'FloatermToggle' ] }

  -- Others
  {
    "ntpeters/vim-better-whitespace", --                                    | Only use for highlighting
    init = function(...)
      vim.g.better_whitespace_enabled = 1
      vim.g.strip_whitespace_on_save = 0
    end,
  },
  {
    "cappyzawa/trim.nvim", --                                               | Use to auto trim whitespace
    opts = {
      ft_blocklist = { "diff", "gitcommit", "unite", "qf", "help", "ctrlsf" },
      trim_on_write = true,
    },
  },
  "preservim/tagbar",     --                                                | Create tag on fly, used by markdown

  "mbbill/undotree",
}
