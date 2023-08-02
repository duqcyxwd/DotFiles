return {

  "folke/neodev.nvim",

  "nvim-lua/plenary.nvim",
  "ruifm/gitlinker.nvim",

  "tommcdo/vim-exchange", --                          " Exchange lines, cx/X/cxx/cxc, works with '.'

  "benmills/vimux",

  -- " Useage: ColorHighlight
  "chrisbra/Colorizer", --                                 " View log file :ColorToggle  TEST: r! exa --color=always --icons -l,
  {
    "jackMort/ChatGPT.nvim",
    event = "VeryLazy",
    config = function()
      require("chatgpt").setup()
    end,
    dependencies = {
      "MunifTanjim/nui.nvim",
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
  },
}
