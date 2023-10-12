require("lua.global")
-- 'nvim-treesitter/nvim-treesitter-refactor'
local refactor = {
  highlight_definitions = {
    enable = true,
    -- Set to false if you have an `updatetime` of ~100.
    clear_on_cursor_move = true,
  },
  highlight_current_scope = { enable = false },
  navigation = {
    enable = false,
    keymaps = {
      -- goto_definition = "gnd",
      -- list_definitions = "gnD",
      -- list_definitions_toc = "gO",
      -- goto_next_usage = "<a-*>",
      -- goto_previous_usage = "<a-#>",
    },
  },
  smart_rename = {
    enable = true,
    keymaps = {
      -- smart_rename = "grr",
    },
  },
}
-- local textobjects =
local ts_plug_lazy_config = function(_, opts)
  IfHasModule('nvim-treesitter.configs', function(ts)
    ts.setup(opts)
  end)
end


return {
  -- " Tree sitter
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = "VeryLazy",
    dependencies = {
      -- "nvim-treesitter/nvim-treesitter-refactor"
    },
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = { "c", "lua", "rust" },
        -- Automatically install missing parsers when entering buffer
        -- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
        auto_install = false,
        highlight = {
          enable = true,
          -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
          -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
          -- Using this option may slow down your editor, and you may see some duplicate highlights.
          -- Instead of true it can also be a list of languages
          custom_capures = {
            ["enable"] = "Identifier",
            ["XXX"] = "Identifier",
          },
          -- I don't like the highlight of markdown, and json
          disable = { "markdown", "json", "zsh", "bash", "yaml" },
          additional_vim_regex_highlighting = { "python" },
        },
        incremental_selection = { enable = true },
        -- pairs = pairs,
        -- textobjects = textobjects,
        refactor = refactor,
      })
      -- WIP Not sure if I need treesitter for bash and zsh
      local ft_to_lang = require('nvim-treesitter.parsers').ft_to_lang
      require('nvim-treesitter.parsers').ft_to_lang = function(ft)
        if ft == 'zsh' then
          return 'bash'
        end
        return ft_to_lang(ft)
      end
    end,
  },
  {
    'andymass/vim-matchup',
    event = 'BufReadPost',
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    -- Need to load at BufReadPost
    init = function()
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_matchparen_offscreen = { method = 'popup', highlight = 'TreeSitterContext' }
    end,
    opts = {
      matchup = {
        enable = true,
      },
    },
    config = ts_plug_lazy_config,
  },
  {
    "nvim-treesitter/nvim-treesitter-context",
    event = 'VeryLazy',
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      require 'treesitter-context'.setup {
        enable = true,            -- Enable this plugin (Can be enabled/disabled later via commands)
        max_lines = 0,            -- How many lines the window should span. Values <= 0 mean no limit.
        min_window_height = 0,    -- Minimum editor window height to enable context. Values <= 0 mean no limit.
        line_numbers = true,
        multiline_threshold = 20, -- Maximum number of lines to show for a single context
        trim_scope = 'outer',     -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
        mode = 'cursor',          -- Line used to calculate context. Choices: 'cursor', 'topline'
        -- Separator between context and content. Should be a single character string, like '-'.
        -- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
        separator = nil,
        zindex = 20,     -- The Z-index of the context window
        on_attach = nil, -- (fun(buf: integer): boolean) return false to disable attaching
      }
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects", --         | Regular text objects
    -- { dir = "~/duqcyxwd/nvim-treesitter-textobjects" }, -- | Testing my textobjects for comments
    event = 'VeryLazy',
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      textobjects = {
        swap = {
          enable = true,
          swap_next = {
            ["]A"] = "@list.inner",
            ["]a"] = "@parameter.inner",
          },
          swap_previous = {
            ["[A"] = "@list.inner",
            ["[a"] = "@parameter.inner",
          },
        },
        select = {
          enable = true,

          -- Automatically jump forward to textobj, similar to targets.vim
          lookahead = true,

          keymaps = {
            -- You can use the capture groups defined in textobjects.scm
            ["il"] = "@parameter.inner",
            ["al"] = "@parameter.outer",
            ["af"] = "@function.outer",
            ["if"] = "@function.inner",
            ["ac"] = "@comment.outer",
            ["ic"] = "@comment.inner",
          },
        },
        move = {
          enable = true,
          set_jumps = true,
          -- whether to set jumps in the jumplist
          -- goto_next_start = {
          --   ["]m"] = "@function.outer",
          --   ["]]"] = "@class.outer",
          -- },
          -- goto_next_end = {
          --   ["]M"] = "@function.outer",
          --   ["]["] = "@class.outer",
          -- },
          -- goto_previous_start = {
          --   ["[m"] = "@function.outer",
          --   ["[["] = "@class.outer",
          -- },
          -- goto_previous_end = {
          --   ["[M"] = "@function.outer",
          --   ["[]"] = "@class.outer",
          -- },
        },
      }
    },
    config = ts_plug_lazy_config,
  },
  {
    "nvim-treesitter/playground",
    cmd = { "TSPlaygroundToggle" },
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      playground = {
        enable = false,
        disable = {},
        updatetime = 25,         -- Debounced time for highlighting nodes in the playground from source code
        persist_queries = false, -- Whether the query persists across vim sessions
        keybindings = {
          toggle_query_editor = "o",
          toggle_hl_groups = "i",
          toggle_injected_languages = "t",
          toggle_anonymous_nodes = "a",
          toggle_language_display = "I",
          focus_language = "f",
          unfocus_language = "F",
          update = "R",
          goto_node = "<cr>",
          show_help = "?",
        },
      }
    },

    config = ts_plug_lazy_config,
  },
  {
    'theHamsta/nvim-treesitter-pairs', --                     | Create your own pair objects using tree-sitter queries!
    enabled = false,
    event = 'VeryLazy',
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      pairs = {
        enable = true,
        disable = {},
        highlight_pair_events = {},                                   -- e.g. {"CursorMoved"}, -- when to highlight the pairs, use {} to deactivate highlighting
        highlight_self = true,                                        -- whether to highlight also the part of the pair under cursor (or only the partner)
        goto_right_end = false,                                       -- whether to go to the end of the right partner or the beginning
        fallback_cmd_normal = "call matchit#Match_wrapper('',1,'n')", -- What command to issue when we can't find a pair (e.g. "normal! %")
        keymaps = {
          goto_partner = "<leader>%",
          delete_balanced = "X",
        },
        delete_balanced = {
          only_on_first_char = false, -- whether to trigger balanced delete when on first character of a pair
          fallback_cmd_normal = nil,  -- fallback command when no pair found, can be nil
          longest_partner = false,    -- whether to delete the longest or the shortest pair when multiple found.
          -- E.g. whether to delete the angle bracket or whole tag in  <pair> </pair>
        },
      },
    },

    config = ts_plug_lazy_config,
  },
  {
    "nvim-treesitter/nvim-treesitter-refactor",
    enabled = true,
    event = 'VeryLazy',
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts = {
      refactor = {
        highlight_definitions = {
          enable = true,
          -- Set to false if you have an `updatetime` of ~100.
          clear_on_cursor_move = true,
        },
        highlight_current_scope = { enable = false },
        navigation = {
          enable = true,
          keymaps = {
            goto_definition = "gnd",
            list_definitions = "gnD",
            list_definitions_toc = "gO",
            -- goto_next_usage = "<a-*>",
            -- goto_previous_usage = "<a-#>",
          },
        },
        smart_rename = {
          enable = true,
          keymaps = {
            smart_rename = "grr",
          },
        },
      },
    },

    config = ts_plug_lazy_config,
  },

}
