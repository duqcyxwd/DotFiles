require("funcs.global")
local M = {}

M.config = function()
  SR("nvim-treesitter.configs").setup({
    ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
    ignore_install = { "javascript", "zsh", "vim", "bash" }, -- List of parsers to ignore installing
    highlight = {
      enable = true, -- false will disable the whole extension
      custom_capures = {
        ["enable"] = "Identifier",
        ["XXX"] = "Identifier",
      },
      -- disable = { "c", "rust", "vim", "clojure", "bash", "zsh" },  -- list of language that will be disabled
      disable = { "c", "rust" }, -- list of language that will be disabled

      -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
      -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
      -- Using this option may slow down your editor, and you may see some duplicate highlights.
      -- Instead of true it can also be a list of languages
      additional_vim_regex_highlighting = false,
    },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = "gnn",
        node_incremental = "grn",
        scope_incremental = "grc",
        node_decremental = "grm",
      },
    },
    rainbow = {
      enable = true,
      -- disable = { "jsx", "cpp" }, list of languages you want to disable the plugin for
      extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
      max_file_lines = nil, -- Do not enable for files with more than n lines, int
      -- colors = {}, -- table of hex strings
      -- termcolors = {} -- table of colour name strings
    },
    playground = {
      enable = true,
      disable = {},
      updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
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
    },
    pairs = {
      enable = true,
      disable = {},
      highlight_pair_events = {}, -- e.g. {"CursorMoved"}, -- when to highlight the pairs, use {} to deactivate highlighting
      highlight_self = true, -- whether to highlight also the part of the pair under cursor (or only the partner)
      goto_right_end = false, -- whether to go to the end of the right partner or the beginning
      fallback_cmd_normal = "call matchit#Match_wrapper('',1,'n')", -- What command to issue when we can't find a pair (e.g. "normal! %")
      keymaps = {
        goto_partner = "<leader>%",
        delete_balanced = "X",
      },
      delete_balanced = {
        only_on_first_char = false, -- whether to trigger balanced delete when on first character of a pair
        fallback_cmd_normal = nil, -- fallback command when no pair found, can be nil
        longest_partner = false, -- whether to delete the longest or the shortest pair when multiple found.
        -- E.g. whether to delete the angle bracket or whole tag in  <pair> </pair>
      },
    },
    textobjects = {
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
    },
    refactor = {
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
    },
  })

  SR("treesitter-context").setup({
    enable = true, -- Enable this plugin (Can be enabled/disabled later via commands)
    throttle = true, -- Throttles plugin updates (may improve performance)
    max_lines = 0, -- How many lines the window should span. Values <= 0 mean no limit.
    patterns = { -- Match patterns for TS nodes. These get wrapped to match at word boundaries.
      -- For all filetypes
      -- Note that setting an entry here replaces all other patterns for this entry.
      -- By setting the 'default' entry below, you can control which nodes you want to
      -- appear in the context window.
      default = {
        "class",
        "function",
        "method",
        "def",
        "defn",
        "let",
        "for", -- These won't appear in the context
        "while",
        "if",
        "switch",
        "case",
      },
      -- Example for a specific filetype.
      -- If a pattern is missing, *open a PR* so everyone can benefit.
      --   rust = {
      --       'impl_item',
      --   },
    },
    exact_patterns = {
      -- Example for a specific filetype with Lua patterns
      -- Treat patterns.rust as a Lua pattern (i.e "^impl_item$" will
      -- exactly match "impl_item" only)
      -- rust = true,
    },
  })
end
M.config()

function test(a, b, c, d)
  print(a + b + c + d)
  print(a + b + c + d)
  return a + b + c + d
end

function test2(a, b, c, d)
  print(a + b + c + d)
  print(a + b + c + d)
  return a + b + c + d
end

return M


