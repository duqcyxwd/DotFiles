local t = require("lua.table")
local vim_u = require 'funcs.nvim_utility'
return {
  -- Fzf
  { -- "/usr/local/opt/fzf"        | Use the fzf installed by brew, Provide FZF command
    dir = "/usr/local/opt/fzf",
    lazy = false,
    cmd = "FZF"
  },
  { -- "junegunn/fzf.vim",         | Default FZF plguin
    "junegunn/fzf.vim",
    enabled = false,
    -- dir = "~/duqcyxwd/fzf.vim", | " My quick fix for fzf vim, e.g FFFiles
    dependencies = { dir = "/usr/local/opt/fzf" },
    lazy = false,
    init = function()
      vim.g.fzf_command_prefix = "FF"
    end,
  },
  { -- "ibhagwan/fzf-lua",         | Fzf support in lua
    -- "duqcyxwd/fzf-lua", --         | My local improvement for fzf-lua
    "ibhagwan/fzf-lua",
    lazy = true,
    event = "VeryLazy",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      local fzf_lua = require("fzf-lua")
      fzf_lua.setup({
        "fzf-tp",
        fzf_opts = { ["--preview-window"] = "down,70%,border-top,nohidden" },
        winopts = {
          border = { " ", " ", " ", " ", " ", " ", " ", " " },
          preview = { default = "bat" },
        },
        previewers = {
          bat = {
            cmd = "bat",
            args = "--style=numbers,changes --color always",
            theme = "Dracula", -- bat preview theme (bat --list-themes)
            config = nil,      -- nil uses $BAT_CONFIG_PATH
          },
        },
        grep = {
          -- Enable search for hidden file
          rg_opts = "--column --line-number --hidden --no-heading --color=always --smart-case --max-columns=4096 -e",
        },
        keymap = { builtin = {}, },
      })

      -- Custom fzf plug fn config

      local jump_to_plugin = function ()
        local defaults = require'fzf-lua.defaults'.defaults
        local plugsin_fzf_opts = t.deep_clone_merge(defaults.grep, {
          actions = fzf_lua.defaults.actions.files,
          prompt = "Plugins> " })

        -- local vimrootPath = "$XDG_CONFIG_HOME/vim/lua/plugins/**.lua"
        -- fzf_lua.fzf_exec("rg --column --line-number --no-heading --color=always --smart-case --max-columns=4096 --type lua -e \"[\\\"'][A-Za-z0-9._\\-]+/[A-Za-z0-9._\\-]+[\\\"']\" -e \"dir =\" " .. vimrootPath, plugsin_fzf_opts)
        fzf_lua.fzf_exec("rg --column --line-number --no-heading --color=always --smart-case --max-columns=4096 --type lua -e \"[\\\"'][A-Za-z0-9._\\-]+/[A-Za-z0-9._\\-]+[\\\"']\" -e \"dir =\" ", plugsin_fzf_opts)
      end

      local plugin_detail = function()
        require 'fzf-lua'.fzf_exec(vim_u.get_all_plugins(), {
          actions = {
            ['default'] = function(selected)
              local plug_info = require("lazy.core.config").spec.plugins[selected[1]]
              vim.notify(vim.inspect(plug_info))
            end
          }
        })
      end

      require("funcs.plug").fzf = {
        jump_to_plugin = jump_to_plugin,
        plugin_detail = plugin_detail,
      }
    end,
  },

  -- telescope
  {
    "tzachar/fuzzy.nvim",
    lazy = true,
    dependencies = { "nvim-telescope/telescope-fzf-native.nvim" }
  },
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    -- TODO CHECK THIS PLUGIN
    enabled = true,
    lazy = true,
    build = 'make',
    config = function()
      -- You dont need to set any of these options. These are the default ones. Only
      -- the loading is important
      require('telescope').setup {
        extensions = {
          fzf = {
            fuzzy = true,                   -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true,    -- override the file sorter
            case_mode = "smart_case",       -- or "ignore_case" or "respect_case"
            -- the default case_mode is "smart_case"
          }
        }
      }
      -- To get fzf loaded and working with telescope, you need to call
      -- load_extension, somewhere after setup function:
      require('telescope').load_extension('fzf')
    end
  },
  { -- "nvim-telescope/telescope.nvim",
    "nvim-telescope/telescope.nvim",
    -- enabled = vim_u.enabled("nvim-treesitter/nvim-treesitter"),
    enabled = true,
    commit = vim.fn.has("nvim-0.9.0") == 0 and "057ee0f8783" or nil,
    cmd = "Telescope",
    version = false, -- telescope did only one release, so use HEAD for now
    opts = {
      extensions = {
        fzf = {
          fuzzy = true,               -- false will only do exact matching
          override_generic_sorter = true, -- override the generic sorter
          override_file_sorter = true, -- override the file sorter
          case_mode = "smart_case",   -- or "ignore_case" or "respect_case"
          -- the default case_mode is "smart_case"
        }
      },
      defaults = {
        prompt_prefix = " ",
        selection_caret = " ",
        mappings = {
          i = {
            ["<C-h>"] = "which_key",
            ["<esc>"] = function(...)
              return require("telescope.actions").close(...)
            end,
            ["<c-t>"] = function(...)
              return require("trouble.providers.telescope").open_with_trouble(...)
            end,
            ["<a-t>"] = function(...)
              return require("trouble.providers.telescope").open_selected_with_trouble(...)
            end,
            ["<Down>"] = function(...)
              return require("telescope.actions").cycle_history_next(...)
            end,
            ["<Up>"] = function(...)
              return require("telescope.actions").cycle_history_prev(...)
            end,
            ["<C-f>"] = function(...)
              return require("telescope.actions").results_scrolling_down(...)
            end,
            ["<C-b>"] = function(...)
              return require("telescope.actions").results_scrolling_up(...)
            end,
            ["<C-j>"] = function(...)
              return require("telescope.actions").preview_scrolling_down(...)
            end,
            ["<C-k>"] = function(...)
              return require("telescope.actions").preview_scrolling_up(...)
            end,
          },
          n = {
            ["q"] = function(...)
              return require("telescope.actions").close(...)
            end,
          },
        },
      },
    },
    config = function (_, opts)
      require"telescope".setup(opts)
      IfHasModule("scope", function ()
        require("telescope").load_extension("scope")
      end )
    end
  },
}
