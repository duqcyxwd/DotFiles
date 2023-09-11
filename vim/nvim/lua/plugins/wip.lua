return {

  "nvim-lua/plenary.nvim", --             | Lua fns

  "tommcdo/vim-exchange",  --             | Exchange lines, cx/X/cxx/cxc, works with '.'

  "benmills/vimux",

  -- " Useage: ColorHighlight
  "chrisbra/Colorizer", --               | View log file :ColorToggle  TEST: r! exa --color=always --icons -l,

  { "tiagovla/scope.nvim",
    config = function ()
      require("telescope").load_extension("scope")
      require("scope").setup({})
    end
  },

  -- { 'Bekaboo/dropbar.nvim' }   -- Wait for neovim 10

}
