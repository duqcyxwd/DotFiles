local t = require("funcs.toggle")

return {
  "chrisbra/unicode.vim",
  "szw/vim-dict",                     -- | A dict client :Dict Hello
  "tpope/vim-eunuch",                 -- | UNIX shell commands
  "norcalli/nvim_utils",              -- | Nvim utils functions
  {
    "christoomey/vim-tmux-navigator", -- | Quickly switch vim split with tmux panel.
    config = function ()
      vim.g.tmux_navigator_no_mappings=1
    end

  },
  "tyru/open-browser.vim",
  "luckasRanarison/nvim-devdocs",
  {
    "nguyenvukhang/nvim-toggler",     -- | Enable custom Text Toggler
    config = true,
    opts = {
      inverses = {
        ['true'] = 'false',
        ['True'] = 'False',
        ['yes'] = 'no',
        ['on'] = 'off',
        ['left'] = 'right',
        ['up'] = 'down',
        ['!='] = '==',
        ['require'] = 'disable',
      },
      remove_default_keybinds = true,
      remove_default_inverses = true,
    }
  }
}
