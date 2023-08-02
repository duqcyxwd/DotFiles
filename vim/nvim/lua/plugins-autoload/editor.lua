local function my_on_attach(bufnr)
  local api = require("nvim-tree.api")

  local function opts(desc)
    return { desc = "nvim-tree: " .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end

  -- default mappings
  api.config.mappings.default_on_attach(bufnr)

  -- custom mappings
  vim.keymap.set("n", "<C-t>", api.tree.change_root_to_parent, opts("Up"))
  vim.keymap.set("n", "?", api.tree.toggle_help, opts("Help"))
end

return { -- Editor enhancements

  ------------------------------------------------------------------------- |
  "folke/which-key.nvim",

  "jiangmiao/auto-pairs",

  "wellle/targets.vim", --                                                  | Addition support for text object
  "michaeljsmith/vim-indent-object", --                                     | Text Object indent
  -- " Cheat sheeti https://github.com/wellle/targets.vim/blob/master/cheatsheet.md
  -- "
  -- Plug 'Shougo/defx.nvim',                                               | { 'do': ':UpdateRemotePlugins', 'on': 'Defx' } " File browser
  "kshenoy/vim-signature", --                                               | A plugin to place, toggle and display marks.
  "machakann/vim-highlightedyank", --                                       | Show highlight for yank
  "machakann/vim-sandwich", --
  "mbbill/undotree", --
  -- "mg979/vim-visual-multi", --, {'on': []}                                  | vim multi cursor
  "ntpeters/vim-better-whitespace", --
  "osyo-manga/vim-over", --                                                 | Preview replace chanage
  "preservim/tagbar", --                                                    | Create tag on fly, used by markdown
  "tpope/vim-commentary", --                                                | Gcc
  "tpope/vim-repeat",
  "tpope/vim-sleuth", --                                                    | Automatically adjusts 'shiftwidth' and 'expandtab'
  "tpope/vim-surround", --
  -- "unblevable/quick-scope", --, {'on': []}                               | Quick highlight for f/F
  "phaazon/hop.nvim", --                                                    | Jump Everywhere
  {
    "Yggdroot/indentLine",
    init = function()
      vim.g.indentLine_char_list = { "|", "¦", "┆", "┊" }
      vim.g.indentLine_leadingSpaceEnabled = 0
      vim.g.indentLine_enabled = 0
    end,
  },

  { "simrat39/symbols-outline.nvim", cmd = "SymbolsOutline", config = true },
  { -- "kyazdani42/nvim-tree.lua"
    "kyazdani42/nvim-tree.lua", --                                            | A File Explorer For Neovim Written In Lua
    dependencies = {
      "kyazdani42/nvim-web-devicons", --                                        | optional for icon support
    },
    cmd = "NvimTreeFindFileToggle",
    opts = {
      on_attach = my_on_attach,
      sort_by = "name",
      diagnostics = {
        enable = false,
      },
      update_focused_file = {
        enable = true,
        update_cwd = true,
      },
      renderer = {
        highlight_git = true,
        group_empty = true,
        icons = {
          padding = " ",
          show = {
            file = true,
            folder = true,
            folder_arrow = true,
            git = true,
          },
          glyphs = {
            git = {
              unstaged = "",
              staged = "",
              unmerged = "",
              renamed = "",
              untracked = "",
              deleted = "",
              ignored = "",
            },
          },
        },
      },

      filters = {
        dotfiles = false,
        custom = {},
      },
    },
  },

  -- " Align text
  "godlygeek/tabular", --, {'on': 'Tabularize'}
  "junegunn/vim-easy-align",

  -- " Terminal
  "kassio/neoterm", --                                                      | Terminal
  "voldikss/vim-floaterm", --,                                              | #{ on: [ 'FloatermNew', 'FloatermHide', 'FloatermToggle' ] }

}
