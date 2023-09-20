return {
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
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("fzf-lua").setup({
        "fzf-tmux",
        fzf_tmux_opts = { ["-p"] = "80%,90%" },
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
        keymap = {
          builtin = {
            ["?"] = "toggle-preview",
            ["<C-j>"] = "preview-page-down",
            ["<C-k>"] = "preview-page-up",
            ["<C-w>"] = "toggle-preview-wrap",
          },
        },
      })
    end,
  },
}
