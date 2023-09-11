local t = require("funcs.toggle")

return {
  "chrisbra/unicode.vim",
  -- "mattn/vim-findroot",
  -- { -- "notjedi/nvim-rooter.lua",
  --   "notjedi/nvim-rooter.lua",
  --   config = true,
  --   opts = {
  --     manual = true,
  --     rooter_patterns = t.root_pattern,
  --     update_cwd = true,
  --     update_focused_file = {
  --       enable = true,
  --       update_cwd = true
  --     },
  --   }
  -- },
  "szw/vim-dict",                   -- | A dict client :Dict Hello
  "tpope/vim-eunuch",               -- | UNIX shell commands
  "norcalli/nvim_utils",            -- | Nvim utils functions
  "christoomey/vim-tmux-navigator", -- | Quickly switch vim split with tmux panel.
  -- { "tyru/open-browser.vim", cmd = { "OpenBrowser", "OpenBrowserSmartSearch" } },
  "tyru/open-browser.vim",
  { -- "luckasRanarison/nvim-devdocs",
    "luckasRanarison/nvim-devdocs",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    opts = {}
  }
}
