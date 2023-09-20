local t = require("funcs.toggle")

return {
  "chrisbra/unicode.vim",
  "szw/vim-dict",                   -- | A dict client :Dict Hello
  "tpope/vim-eunuch",               -- | UNIX shell commands
  "norcalli/nvim_utils",            -- | Nvim utils functions
  "christoomey/vim-tmux-navigator", -- | Quickly switch vim split with tmux panel.
  "tyru/open-browser.vim",
  "luckasRanarison/nvim-devdocs",
  'saifulapm/chartoggle.nvim', --      | Toogle end comma(,), semicolon(;)
  { -- 'nguyenvukhang/nvim-toggler',   | Enable custom Text Toggler
    'nguyenvukhang/nvim-toggler',
    config = true,
    opt = {
      inverses = {
      },
      remove_default_keybinds = true,
      remove_default_inverses = false,
    }
  }
}
