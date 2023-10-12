require("lua.global")
local gitsigns = {
  "lewis6991/gitsigns.nvim",
  config = true,
  opt = function()
    return {

      -- stylua: ignore
      signs          = {
        add          = { hl = "GitSignsAdd",    text = "│", numhl = "GitSignsAddNr",    linehl = "GitSignsAddLn"    },
        change       = { hl = "GitSignsChange", text = "│", numhl = "GitSignsChangeNr", linehl = "GitSignsChangeLn" },
        delete       = { hl = "GitSignsDelete", text = "_", numhl = "GitSignsDeleteNr", linehl = "GitSignsDeleteLn" },
        topdelete    = { hl = "GitSignsDelete", text = "‾", numhl = "GitSignsDeleteNr", linehl = "GitSignsDeleteLn" },
        changedelete = { hl = "GitSignsChange", text = "~", numhl = "GitSignsChangeNr", linehl = "GitSignsChangeLn" },
      },
      signcolumn = true, -- Toggle with `:Gitsigns toggle_signs`
      numhl = false,     -- Toggle with `:Gitsigns toggle_numhl`
      linehl = false,    -- Toggle with `:Gitsigns toggle_linehl`
      word_diff = false, -- Toggle with `:Gitsigns toggle_word_diff`
      watch_gitdir = {
        interval = 1000,
        follow_files = true,
      },
      attach_to_untracked = false,
      current_line_blame = true, -- Toggle with `:Gitsigns toggle_current_line_blame`
      current_line_blame_opts = {
        virt_text = true,
        virt_text_pos = "eol", -- 'eol' | 'overlay' | 'right_align'
        delay = 500,
        ignore_whitespace = true,
        virt_text_priority = 1000,
      },
      current_line_blame_formatter = "    <author>, <author_time:%Y-%m-%d> - <summary>",
      sign_priority = 6,
      update_debounce = 100,
      status_formatter = nil, -- Use default
      max_file_length = 40000,
      preview_config = {
        -- Options passed to nvim_open_win
        border = "single",
        style = "minimal",
        relative = "cursor",
        row = 0,
        col = 1,
      },
      yadm = {
        enable = false,
      },
    }
  end,
}

return {
  "jreybert/vimagit",              -- | ViMagit to stage changes
  { "lambdalisue/gina.vim", cmd = "Gina" },
  "tpope/vim-fugitive",            -- | Raw git command, Gblame
  "shumphrey/fugitive-gitlab.vim", -- | Open private git repo link
  "tpope/vim-rhubarb",             -- | Open github link
  gitsigns,                        -- | git decorations, git hunk, show diff
  "junegunn/gv.vim",               -- | A git better commit browser.
}
