require("funcs.global")
return {

  { -- "norcalli/nvim-colorizer.lua", | Show color
    "norcalli/nvim-colorizer.lua",
    lazy = false,
    init = function()
      vim.opt.termguicolors = true
    end,
    config = function()
      require("colorizer").setup()
    end,
  },
  { -- "nvim-lualine/lualine.nvim",
    "nvim-lualine/lualine.nvim",
    dependencies = { "arkav/lualine-lsp-progress" },
    config = function()
      R("lualine").setup({
        options = {
          icons_enabled = true,
          theme = "auto",
          component_separators = { left = "", right = "" },
          section_separators = { left = "", right = "" },
          disabled_filetypes = {},
          always_divide_middle = true,

          show_bufnr = true, -- this appends [bufnr] to buffer section,
          modified_icon = "+ ", -- change the default modified icon
        },
        sections = {
          lualine_a = { "mode" },
          lualine_b = { "branch", "diff", "diagnostics" },
          lualine_c = { "filename" },
          lualine_x = { "lazy" },
          lualine_y = { "encoding", "fileformat", "filetype" },
          lualine_z = { "progress", "location" },
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = { "filename" },
          lualine_x = {},
          lualine_y = {},
          lualine_z = { "progress" },
        },
        tabline = {},
        extensions = {},
      })
    end,
  },
  { -- "akinsho/bufferline.nvim",
    "akinsho/bufferline.nvim",
    main = "bufferline",
    config = function()
      R("bufferline").setup({
        options = {
          -- numbers = function(opts)
          --   return string.format('%s.', opts.ordinal)
          -- end,
          numbers = "ordinal",
          diagnostics = "none",
          offsets = { { filetype = "coc-explorer" } },
          show_tab_indicators = true,
          enforce_regular_tabs = true,
          always_show_bufferline = false,
          separator_style = "slant",
          max_prefix_length = 10,
          max_name_length = 30,
          tab_size = 25,
          show_buffer_close_icons = false,
          show_buffer_icons = false,
          middle_mouse_command = "bdelete! %d",
        },
      })
    end,
  },

  "kyazdani42/nvim-web-devicons", -- Recommended ()

  -- Theme
  "mhartington/oceanic-next",
  "ayu-theme/ayu-vim",
  "sonph/onehalf", -- {'rtp': 'vim/' }
  "cormacrelf/vim-colors-github",
  "EdenEast/nightfox.nvim",
  "dracula/vim", --   { 'as': 'dracula' }

  -- Zen
  { "junegunn/limelight.vim", cmd = "Limelight" },
  { "junegunn/goyo.vim", cmd = "Goyo" },
  { -- "folke/twilight.nvim",
    "folke/twilight.nvim",
    -- Dims inactive portions build on tree sitter
    cmd = "Twilight",
    config = function()
      R("twilight").setup({
        dimming = {
          alpha = 0.25, -- amount of dimming
          -- we try to get the foreground from the highlight groups or fallback color
          color = { "Normal", "#ffffff" },
          inactive = false, -- when true, other windows will be fully dimmed (unless they contain the same buffer)
        },
        context = 10, -- amount of lines we will try to show around the current line
        sitter = true, -- use treesitter when available for the filetype
        -- treesitter is used to automatically expand the visible text,
        -- but you can further control the types of nodes that should always be fully expanded
        expand = { -- for treesitter, we we always try to expand to the top-most ancestor with these types
          "function",
          "method",
          "table",
          "if_statement",
        },
        exclude = {}, -- exclude these filetypes
      })
    end,
  },
  { -- "folke/zen-mode.nvim",
    "folke/zen-mode.nvim",
    cmd = "ZenMode",
    config = function()
      R("zen-mode").setup({
        window = {
          backdrop = 0.95, -- shade the backdrop of the Zen window. Set to 1 to keep the same as Normal
          -- height and width can be:
          -- * an absolute number of cells when > 1
          -- * a percentage of the width / height of the editor when <= 1
          -- * a function that returns the width or the height
          width = 120, -- width of the Zen window
          height = 1, -- height of the Zen window
          -- by default, no options are changed for the Zen window
          -- uncomment any of the options below, or add other vim.wo options you want to apply
          options = {
            -- signcolumn = "no", -- disable signcolumn
            -- number = false, -- disable number column
            -- relativenumber = false, -- disable relative numbers
            -- cursorline = false, -- disable cursorline
            -- cursorcolumn = false, -- disable cursor column
            -- foldcolumn = "0", -- disable fold column
            -- list = false, -- disable whitespace characters
          },
        },
        plugins = {
          -- disable some global vim options (vim.o...)
          -- comment the lines to not apply the options
          options = {
            enabled = true,
            ruler = false, -- disables the ruler text in the cmd line area
            showcmd = false, -- disables the command in the last line of the screen
          },
          twilight = { enabled = true }, -- enable to start Twilight when zen mode opens
          gitsigns = { enabled = false }, -- disables git signs
          tmux = { enabled = false }, -- disables the tmux statusline
          -- this will change the font size on kitty when in zen mode
          -- to make this work, you need to set the following kitty options:
          -- - allow_remote_control socket-only
          -- - listen_on unix:/tmp/kitty
          kitty = {
            enabled = false,
            font = "+4", -- font size increment
          },
        },
        -- callback where you can add custom code when the Zen window opens
        on_open = function(win) end,
        -- callback where you can add custom code when the Zen window closes
        on_close = function() end,
      })
    end,
  },
}
