require("lua.global")
local core = require("funcs.nvim_core")

return {
  -- Section: Keys mappings --------------------- | Description
  {
    "folke/which-key.nvim" ,
    lazy = true,
    event = "VeryLazy",
    config = function ()
      require'which-key'.setup({
        layout = {
          height = { min = 15, max = 25 }, -- min and max height of the columns
          width = { min = 20, max = 50 },  -- min and max width of the columns
          spacing = 3,                     -- spacing between columns
          align = "left",                  -- align columns left, center or right
        }
      })
      R("config.keys")
    end,
  },
  {
    "jiangmiao/auto-pairs",       --              | Auto pair quote and add space
    enabled = true,
    init = function()
      vim.g.AutoPairsMapSpace = 0 --              | Disable because it adds space for test('abc')
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
  "tpope/vim-sleuth", --                          | Automatically adjusts 'shiftwidth' and 'expandtab'

  -- Text Object Operator (d/c/y/x)
  {
    "kylechui/nvim-surround", --                  | Surround Operator, use for normal: ys/ds/cs/ + '"`f visual:S
    --                                            | Replace "tpope/vim-surround"
    version = "*",            -- Use for stability; omit to use `main` branch for the latest features
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
  "tommcdo/vim-exchange", --                      | Exchange lines, cx/X/cxx/cxc, works with '.'
  "tpope/vim-commentary", --                      | Gcc
  "tpope/vim-repeat",

  -- Text Object
  -- Treesitter also provides text object
  {
    "wellle/targets.vim", --                      | Addition Text Object For DEFAULT OPERATOR, Pair/Quote/Separator/Argument/Tag e.g di' or diq, vi', vaq
    --                                            | Cheat Sheet https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
  },
  {
    "michaeljsmith/vim-indent-object", --         | Indent Text Object, e.g vii, vai
    -- Replaced by mini.indentscope
    enabled = false,
},

  -- Highlight for action
  "machakann/vim-highlightedyank", --             | Show highlight for yank
  "osyo-manga/vim-over",           --             | Preview replace chanage

  -- Vim motions, jump, search
  { -- "ggandor/lightspeed.nvim",                 | A motion plugin, f/F
    "ggandor/lightspeed.nvim",
    enabled = false,
    config = {
      special_keys = {},
      limit_ft_matches = 10,
    },
  },
  {
    "folke/flash.nvim", --                        | Replacement for light speed, e.g f/F/sS, + search / <Space>v
    --                                            | support Tree sitter search,
    event = "VeryLazy",
    enabled = true,
    opts = {
      modes = {
        -- options used when flash is activated through
        -- a regular search with `/` or `?`
        search = {
          -- when `true`, flash will be activated during regular search by default.
          -- You can always toggle when searching with `require("flash").toggle()`
          enabled = false,
          highlight = { backdrop = false },
          jump = { history = true, register = true, nohlsearch = true },
          search = {
            -- `forward` will be automatically set to the search direction
            -- `mode` is always set to `search`
            -- `incremental` is set to `true` when `incsearch` is enabled
          },
        },
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
  { -- "phaazon/hop.nvim",                        | Jump Everywhere
    "phaazon/hop.nvim",
    config = true,
    lazy = true,
    cmd = { "HopVerticalMW", "HopChar1", "HopChar2" },
  },
  {
    "chrisgrieser/nvim-origami", --               | Motion for fold: h/l
    event = "BufReadPost",       -- later or on keypress would prevent saving folds
    opts = {
      pauseFoldsOnSearch = false,
    },
    config = true
  },

  -- Block Text Editor
  {
    "mg979/vim-visual-multi", --                  | vim multi cursor
    lazy = true,
    event = "VeryLazy",
  },
  {
    "junegunn/vim-easy-align" --                  | ga*,
  },
  "godlygeek/tabular", --                         | {'on': 'Tabularize'}

  -- Terminal
  {
    "kassio/neoterm", --                          | Terminal
    cmd = { "Ttoggle" },
    init = function()
      vim.g.neoterm_shell = 'zsh'
      vim.g.neoterm_autoscroll = 1
      vim.g.neoterm_keep_term_open = 1
      vim.g.neoterm_autoinsert = 0
      vim.g.neoterm_autojump = 1
      vim.g.neoterm_automap_keys = '-tt'
    end,
    config = function()
      -- " Works great with nvr, works through varible $NVIM in its child terminal
      -- " https://github.com/mhinz/neovim-remote

      -- Define the autocommand group 'neoterm'
      vim.cmd('augroup neoterm')
      vim.cmd('autocmd!')

      -- Set 'scrolloff' to 0 when entering terminal buffers
      vim.cmd('autocmd TermEnter * setlocal scrolloff=0')

      -- Automatically start insert mode when opening terminal buffers
      vim.cmd('autocmd TermOpen * startinsert')

      -- Create a mapping to toggle the terminal with <Esc> in normal mode
      vim.cmd('autocmd FileType neoterm nnoremap <buffer> <Esc> :Ttoggle<CR>')

      -- End the autocommand group
      vim.cmd('augroup END')
    end

  },
  "voldikss/vim-floaterm", --                     | #{ on: [ 'FloatermNew', 'FloatermHide', 'FloatermToggle' ] }

  -- Others
  {
    "ntpeters/vim-better-whitespace", --          | Only use for highlighting
    init = function(...)
      vim.g.better_whitespace_enabled = 1
      vim.g.strip_whitespace_on_save = 0
    end,
  },
  {
    "cappyzawa/trim.nvim", --                     | Use to auto trim whitespace
    opts = {
      ft_blocklist = { "diff", "gitcommit", "unite", "qf", "help", "ctrlsf" },
      trim_on_write = true,
    },
  },
  "preservim/tagbar", --                          | Create tag on fly, used by markdown

  "mbbill/undotree",
}
