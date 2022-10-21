require("funcs.global")

-- https://github.com/kyazdani42/nvim-tree.lua
-- following options are the default
-- each of these are documented in `:help nvim-tree.OPTION_NAME`
SR'nvim-tree'.setup {
  sort_by = "name",
  open_on_setup = true,
  open_on_setup_file = true,
  diagnostics = {
    enable = false,
  },
  update_focused_file = {
    enable      = true,
    update_cwd  = false,
    ignore_list = {}
  },
  filters = {
    dotfiles = false,
    custom = {}
  },
  git = {
    enable = true,
    ignore = true,
    timeout = 500,
  },
  view = {
    hide_root_folder = false,
    mappings = {
      custom_only = false,
      list = {
        { key = "?", action = "toggle_help" },

      }
    },
  },

  renderer = {
    highlight_git = true,
    indent_markers = {
      enable = true,
    },
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
    special_files = { "Cargo.toml", "Makefile", "README.md", "readme.md" },
    symlink_destination = true,
  },
  actions = {
    change_dir = {
      enable = true,
      global = false,
    },
    open_file = {
      quit_on_open = false,
      window_picker = {
        enable = true,
        chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890",
        exclude = {
          filetype = {
            "notify",
            "packer",
            "qf"
          }
        }
      }
    }
  }
}

DR("nvim-tree").setup()
