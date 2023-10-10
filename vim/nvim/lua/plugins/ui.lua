require("funcs.global")
local core = require("funcs.nvim_core")
local vim_u = require("funcs.nvim_utility")
local api = vim.api

local digits = function(number)
    local numberStr = tostring(number)
    local digitCount = select(2, numberStr:gsub("%d", ""))
    return digitCount
end
local number_of_folded_lines = function()
   local total_lines = vim.api.nvim_buf_line_count(0)
  return string.format("%".. digits(total_lines) .."d lines", vim.v.foldend - vim.v.foldstart + 1)
end


local function nvim_tree_on_attach(bufnr)
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

local logo = [[
**********************************************************************
*       ___          ___          ___          ___          ___      *
*      /\  \        /\  \        /\  \        /\__\        /|  |     *
*      \:\  \       \:\  \      /::\  \      /:/  /       |:|  |     *
*       \:\  \       \:\  \    /:/\:\  \    /:/  /        |:|  |     *
*   ___ /::\  \  ___ /::\  \  /:/ /::\  \  /:/  /  ___  __|:|  |     *
*  /\  /:/\:\__\/\  /:/\:\__\/:/_/:/\:\__\/:/__/  /\__\/\ |:|__|____ *
*  \:\/:/  \/__/\:\/:/  \/__/\:\/:/  \/__/\:\  \ /:/  /\:\/:::::/__/ *
*   \::/__/      \::/__/      \::/__/      \:\  /:/  /  \::/~~/~     *
*    \:\  \       \:\  \       \:\  \       \:\/:/  /    \:\~~\      *
*     \:\__\       \:\__\       \:\__\       \::/  /      \:\__\     *
*      \/__/        \/__/        \/__/        \/__/        \/__/     *
* Happy Hacking. Chuan's new vim                                     *
**********************************************************************
]]


return {

  -- Section: Sidebar -------------------------- | Description
  { "simrat39/symbols-outline.nvim", cmd = "SymbolsOutline", config = true },
  {
    "nvim-tree/nvim-tree.lua", --                | A File Explorer For Neovim Written In Lua
    dependencies = {
      "nvim-tree/nvim-web-devicons", --          | optional for icon support
    },
    cmd = { "NvimTreeFindFileToggle", "NvimTreeToggle" },
    opts = {
      on_attach = nvim_tree_on_attach,
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
  { "nvim-tree/nvim-web-devicons",   lazy = true },
  'kiyoon/nvim-tree-remote.nvim',--              | Support for  Treemux

  "kshenoy/vim-signature", --                    | A plugin to place, toggle and display marks.


  { -- "norcalli/nvim-colorizer.lua",            | Show color
    "norcalli/nvim-colorizer.lua",
    init = function()
      vim.opt.termguicolors = true
    end,
    config = function()
      require("colorizer").setup()
    end,
  },
  { -- "nvim-lualine/lualine.nvim",              | Lines at bottom
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    dependencies = { "arkav/lualine-lsp-progress" },
    config = function()
      -- local statusLineMode = { "mode" } -- Indicate macro
      -- if vim_u.has("noice.nvim") then
      --   -- TODO Check if this works
      --   local mode_indicator = {
      --     require("noice").api.statusline.mode.get,
      --     cond = require("noice").api.statusline.mode.has,
      --     -- color = { fg = "#ff9e64" },
      --   }
      --   statusLineMode = { mode_indicator, "mode" }
      -- end
      R("lualine").setup({
        options = {
          icons_enabled = true,
          theme = "auto",
          component_separators = { left = "", right = "" },
          section_separators = { left = "", right = "" },
          disabled_filetypes = {},
          always_divide_middle = true,

          show_bufnr = true,    -- this appends [bufnr] to buffer section,
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
  { -- "akinsho/bufferline.nvim",   --           | Show top tab/buffer lines
    "akinsho/bufferline.nvim",
    enabled = true,
    main = "bufferline",
    config = function()
      R("bufferline").setup({
        options = {
          diagnostics = "none",
          offsets = {
            {
              filetype = "NvimTree",
              text = "File Explorer",
              text_align = "left",
              separator = true
            }
          },
          show_tab_indicators = true,
          enforce_regular_tabs = true,
          always_show_bufferline = true,
          separator_style = "slant",
          max_name_length = 30,
          tab_size = 18,
          buffer_close_icon = '', -- Set to '' to save space
          show_buffer_close_icons = false,
          show_buffer_icons = true,
          middle_mouse_command = "bdelete! %d",
        },
      })
    end,
  },

  { -- "tiagovla/scope.nvim",                    | Add buffer scope to Tabs
    "tiagovla/scope.nvim",
    enabled = true,
    event = "VeryLazy",
    config = function()
      require("telescope").load_extension("scope")
      require("scope").setup({})
    end
  },

  { -- "folke/noice.nvim"                        | Provides CMD, Messages provides lsp progress
    "folke/noice.nvim",
    enabled = true,
    event = "VeryLazy",
    opts = {
      cmdline = {
        enabled = true, -- enables the Noice cmdline UI
        view = "cmdline_popup",
      },
      views = {
        cmdline_popup = {
          -- Clean cmdline pop
          -- https://github.com/folke/noice.nvim/wiki/Configuration-Recipes#clean-cmdline_popup
          border = {
            style = "none",
            padding = { 1, 2 },
          },
          filter_options = {},
          win_options = {
            winhighlight = "NormalFloat:NormalFloat,FloatBorder:FloatBorder",
          },
        },
      },
      lsp = {
        override = {
          ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
          ["vim.lsp.util.stylize_markdown"] = true,
          ["cmp.entry.get_documentation"] = true,
        },
      },
      routes = {
        {
          -- Allow indicator for @
          view = "mini",
          filter = { event = "msg_showmode" },
          format = {
            align = "left"
          },
        },

        {
          filter = { event = "msg_show", kind = "search_count" },
          view = "mini",
          opts = { skip = false },
        },
        {
          filter = {
            event = "msg_show",
            any = {
              { find = "%d+L, %d+B" },
              { find = "; after #%d+" },
              { find = "; before #%d+" },
            },
          },
          view = "mini",
        },
      },
      presets = {
        bottom_search = true,
        command_palette = true,
        long_message_to_split = true,
        inc_rename = true,
      },
    },
    -- stylua: ignore
    keys = {
      {
        "<S-Enter>",
        function()
          require("noice").redirect(vim.fn.getcmdline())
        end,
        mode = "c",
        desc = "Redirect Cmdline",
      },
      {
        "<leader>snl",
        function()
          require("noice").cmd("last")
        end,
        desc = "Noice Last Message",
      },
      {
        "<leader>snh",
        function()
          require("noice").cmd("history")
        end,
        desc = "Noice History",
      },
      {
        "<leader>sna",
        function()
          require("noice").cmd("all")
        end,
        desc = "Noice All",
      },
      {
        "<leader>snd",
        function()
          require("noice").cmd("dismiss")
        end,
        desc = "Dismiss All",
      },
      {
        "<c-f>",
        function()
          if not require("noice.lsp").scroll(4) then
            return "<c-f>"
          end
        end,
        silent = true,
        expr = true,
        desc = "Scroll forward",
        mode = { "i", "n", "s" },
      },
      {
        "<c-b>",
        function()
          if not require("noice.lsp").scroll(-4) then
            return "<c-b>"
          end
        end,
        silent = true,
        expr = true,
        desc = "Scroll backward",
        mode = { "i", "n", "s" },
      },
    },
  },
  { -- "rcarriga/nvim-notify",                   | A fancy notification manager for NeoVim, required for noce.nvim
    "rcarriga/nvim-notify",
    enabled = true,
    opts = {
      fps = 30,
      render = "default",
      stages = "static",
      timeout = 5000,
      max_height = function()
        return math.floor(vim.o.lines * 0.75)
      end,
      max_width = function()
        return math.floor(vim.o.columns * 0.75)
      end,
    },
    init = function()
      core.nvim_create_augroups({
        notify = {
          { "FileType", "notify", "nmap <buffer> q :q<CR>" },
          { "FileType", "notify", "nmap <buffer> <esc> :q<CR>" }
        }
      })
      -- when noice is not enabled, install notify on VeryLazy
      local Util = require("funcs.nvim_utility")
      if not Util.has("noice.nvim") then
        Util.on_very_lazy(function()
          vim.notify = require("notify")
        end)
      end
    end,
  },
  {
    "stevearc/dressing.nvim", --                 | better vim.ui
    enabled = true,
    lazy = true,
    init = function()
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.select = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.select(...)
      end
      ---@diagnostic disable-next-line: duplicate-set-field
      vim.ui.input = function(...)
        require("lazy").load({ plugins = { "dressing.nvim" } })
        return vim.ui.input(...)
      end
    end,
  },


  -- ui components
  { "MunifTanjim/nui.nvim",          lazy = true },

  -- Zen
  { "junegunn/limelight.vim",        cmd = "Limelight" },
  { "junegunn/goyo.vim",             cmd = "Goyo" },
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
        context = 10,       -- amount of lines we will try to show around the current line
        sitter = true,      -- use treesitter when available for the filetype
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
          height = 1,  -- height of the Zen window
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
            ruler = false,                -- disables the ruler text in the cmd line area
            showcmd = false,              -- disables the command in the last line of the screen
          },
          twilight = { enabled = true },  -- enable to start Twilight when zen mode opens
          gitsigns = { enabled = false }, -- disables git signs
          tmux = { enabled = false },     -- disables the tmux statusline
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

  -- Home page and Session---------------------- | Description
  {
    "goolord/alpha-nvim",--                      | Home Page: a fast and fully programmable greeter
    event = "VimEnter",
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    opts = function()
      local dashboard = require("alpha.themes.dashboard")

      dashboard.section.header.val = vim.split(logo, "\n")
      dashboard.section.buttons.val = {
        dashboard.button("f", " " .. " Find file", ":Telescope find_files <CR>"),
        dashboard.button("n", " " .. " New file", ":ene <BAR> startinsert <CR>"),
        dashboard.button("r", " " .. " Recent files", ":Telescope oldfiles <CR>"),
        dashboard.button("c", " " .. " Config", ":e $MYVIMRC <CR>"),
        dashboard.button("R", " " .. " Restore Session in current directory", [[:SessionRestore<cr>]]),
        dashboard.button("s", " " .. " Sessions", [[:SearchSession<cr>]]),
        dashboard.button("l", "󰒲 " .. " Lazy", ":Lazy<CR>"),
        dashboard.button("q", " " .. " Quit", ":qa<CR>"),
      }
      for _, button in ipairs(dashboard.section.buttons.val) do
        button.opts.hl = "AlphaButtons"
        button.opts.hl_shortcut = "AlphaShortcut"
      end
      dashboard.section.header.opts.hl = "AlphaHeader"
      dashboard.section.buttons.opts.hl = "AlphaButtons"
      dashboard.section.footer.opts.hl = "AlphaFooter"
      dashboard.opts.layout[1].val = 8
      return dashboard
    end,
    config = function(_, dashboard)
      -- close Lazy and re-open when the dashboard is ready
      if vim.o.filetype == "lazy" then
        vim.cmd.close()
        vim.api.nvim_create_autocmd("User", {
          pattern = "AlphaReady",
          callback = function()
            require("lazy").show()
          end,
        })
      end

      require("alpha").setup(dashboard.opts)

      vim.api.nvim_create_autocmd("User", {
        pattern = "LazyVimStarted",
        callback = function()
          local stats = require("lazy").stats()
          local ms = (math.floor(stats.startuptime * 100 + 0.5) / 100)
          dashboard.section.footer.val = "⚡ Neovim loaded " .. stats.count .. " plugins in " .. ms .. "ms"
          pcall(vim.cmd.AlphaRedraw)
        end,
      })
    end,
  },
  { -- 'rmagatti/auto-session',
    "rmagatti/auto-session",
    config = function()
      require("auto-session").setup {
        log_level = "error",
        auto_restore_enabled = false,
        auto_save_enabled = true,
        auto_session_enable_last_session = vim.loop.cwd() == vim.loop.os_homedir(),
        auto_session_create_enabled = true,
        auto_session_root_dir = vim.fn.stdpath("state") .. "/sessions/",
        auto_session_suppress_dirs = { "~/", "~/Downloads", "/", "~/.local/", "~/work_credential/", "/private/" },
        auto_session_use_git_branch = false,
      }
    end
  },
  {
    "rmagatti/session-lens",--                   | Session switcher for Session Lens extends auto-session through Telescope.nvim
    cmd = { "SearchSession" },
    dependencies = { "nvim-telescope/telescope.nvim" },
    config = true,
  },
  {
    "folke/persistence.nvim", --                 | persistence session
    disable = true,
    event = "BufReadPre",
    opts = {
      dir = vim.fn.stdpath("state") .. "/sessions/",
      options = { "buffers", "curdir", "tabpages", "winsize", "help", "globals", "skiprtp" }
    },
    config = true,
    -- stylua: ignore
    -- keys = {
    --   { "<leader>qs", function() require("persistence").load() end,                desc = "Restore Session" },
    --   { "<leader>ql", function() require("persistence").load({ last = true }) end, desc = "Restore Last Session" },
    --   { "<leader>qd", function() require("persistence").stop() end,                desc = "Don't Save Current Session" },
    -- },
  },

  -- No identation for following plugins
  {
    "anuvyklack/pretty-fold.nvim", --           | Setup my folding text
    config = function()
      require("pretty-fold").setup({
        process_comment_signs = 'delete',
        keep_indentation = false,
        fill_char = '━',
        sections = {
          left = {
            '', function() return string.rep('+', vim.v.foldlevel) end, '', 'content', ''
          },
          right = {
            '┫ ', number_of_folded_lines,
            ': ', 'percentage', ' ┣━',
          }
        }
      })
      require("pretty-fold").ft_setup("lua", {
        keep_indentation = true,
        process_comment_signs = 'delete',
        sections = {
          left = {
            function (config)
              local fillChar = config.fill_char
              local line =  vim.fn.getline(vim.v.foldstart)

              -- Generic
              line = line:gsub(" = {", " = { ... }", 1)
              line = line:gsub("{{{%d", "", 1)
              -- Replace leading -- to space
              line = line:gsub("^%-%-", "+ ", 1)


              -- Add second line if current line is empty
              if string.match(line, "^%s+{$") then
                local line2 = vim.fn.getline(vim.v.foldstart + 1)
                local pattern = "^%s+"
                line = line .. string.gsub(line2, pattern, " ")
              end

              -- Fix inline commands with good aligment
              local commentPattern = "%-%- "
              if string.match(line, "| ") then
                -- Add spaces before |
                line = line:gsub(commentPattern, "", 1)
                line = line:gsub("| ", "   | ")
                line = line:gsub(commentPattern, "   ")
              else
                line = line:gsub(commentPattern, "")
              end

              -- line = line:gsub("| ", "  ") -- Hiding Alignment char |

              -- if vim.v.foldlevel > 2 then
              --   local levelSpace =  string.rep('%s', vim.v.foldlevel - 2)
              --   local plus =  string.rep('+', vim.v.foldlevel - 2)
              --   line = line:gsub("^%s+%S", function(match) return match:gsub( levelSpace .. " %S", function(m) return m:gsub(levelSpace .. " ", plus .. " ", 1) end , 1) end)
              -- end

              -- Hack: Show disabled plugins
              if string.match(line, "^%s+{") then
                for l=vim.v.foldstart,vim.v.foldend do
                  local parseline = vim.fn.getline(l)
                  if parseline:match("^%s+enabled = false") then
                    line = line:gsub(",           ", ", [disabled]")
                    break
                  end
                end
              end

              line = line:gsub("[^%s]%s%s+", function (match) return match:gsub("%s", fillChar) end)
              line = line:gsub("([^%s])" .. fillChar, "%1 ", 1)
              return line
            end,
            ' '

          },
          right = {
            '| ', number_of_folded_lines , ': ', 'percentage', ' |',
          }
        },
        -- fill_char = "-",
        fill_char = '━',
        -- fill_char = ' ',
        add_close_pattern = false, -- true, 'last_line' or false
      })
    end,
  },
  { -- "Yggdroot/indentLine",
    "Yggdroot/indentLine",
    enabled = true,
    init = function()
      vim.g.indentLine_char_list = { "|", "¦", "┆", "┊" }
      vim.g.indentLine_leadingSpaceEnabled = 0
      vim.g.indentLine_enabled = 0
    end,
  },
  { -- "echasnovski/mini.indentscope",
    -- Active indent guide and indent text objects. When you're browsing
    -- code, this highlights the current level of indentation, and animates
    -- the highlighting.
    "echasnovski/mini.indentscope",
    version = false, -- wait till new 0.7.0 release to put it back on semver
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      -- symbol = "▏",
      symbol = "│",
      options = { try_as_border = true },
    },
    init = function()
      vim.api.nvim_create_autocmd("FileType", {
        pattern = {
          "help",
          "alpha",
          "dashboard",
          "neo-tree",
          "Trouble",
          "lazy",
          "mason",
          "notify",
          "toggleterm",
          "lazyterm",
        },
        callback = function()
          vim.b.miniindentscope_disable = true
        end,
      })
    end,
  },
}
