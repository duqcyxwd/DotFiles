return {

  "nvim-lua/plenary.nvim", --             | Lua fns


  "benmills/vimux",

  -- " Useage: ColorHighlight
  "chrisbra/Colorizer", --               | View log file :ColorToggle  TEST: r! exa --color=always --icons -l,


  { "m-demare/hlargs.nvim", config = true },

  {
    "desdic/agrolens.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    config = function()
      require "telescope".load_extension("agrolens")
    end
  },

  -- { 'Bekaboo/dropbar.nvim' }   -- Wait for neovim 10

}
