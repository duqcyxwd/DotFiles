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
local playground = {
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
local pairs = {
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
}
local textobjects = {
  swap = {
    enable = true,
    swap_next = {
      ["]a"] = "@list.inner",
    },
    swap_previous = {
      ["[a"] = "@list.inner",
    },
  },
  select = {
    enable = true,

    -- Automatically jump forward to textobj, similar to targets.vim
    lookahead = true,

    keymaps = {
      -- You can use the capture groups defined in textobjects.scm
      ["af"] = "@function.outer",
      ["if"] = "@function.inner",
      ["ac"] = "@comment.outer",
      ["ic"] = "@comment.inner",
    },
  },
  move = {
    enable = true,
    set_jumps = true, -- whether to set jumps in the jumplist
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

return {
  -- " Tree sitter
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
    event = "VeryLazy",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-context",
      "nvim-treesitter/playground",
      "nvim-treesitter/nvim-treesitter-textobjects", --         | Regular text objects
      -- { dir = "~/duqcyxwd/nvim-treesitter-textobjects" }, -- | Testing my textobjects for comments
      -- "nvim-treesitter/nvim-treesitter-refactor"
      -- 'theHamsta/nvim-treesitter-pairs',                     | Create your own pair objects using tree-sitter queries!
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
        playground = playground,
        pairs = pairs,
        textobjects = textobjects,
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
}
