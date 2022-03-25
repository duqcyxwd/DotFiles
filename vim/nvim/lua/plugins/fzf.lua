 -- set border to none to avoid conflict between fzf-lua and fzf.vim
R('fzf-lua').setup{
  fzf_opts = {
    ['--border']        = 'none',
  },
  manpages = { previewer = { _ctor = require'fzf-lua.previewer'.fzf.man_pages } },
  border           = 'none',
  winopts = {
    -- (i.e. when 'split' is not defined, default)
    height           = 0.85,            -- window height
    width            = 0.90,            -- window width
    row              = 0.50,            -- window row position (0=top, 1=bottom)
    col              = 0.50,            -- window col position (0=left, 1=right)
    hl = {
      normal         = 'Normal',        -- window normal color (fg+bg)
      border         = 'FloatBorder',        -- border color (try 'FloatBorder')
    },
  },
  previewers = {
    bat = {
      cmd             = "bat",
      args            = "--style=numbers,changes --color always",
      theme           = 'Coldark-Dark', -- bat preview theme (bat --list-themes)
      config          = nil,            -- nil uses $BAT_CONFIG_PATH
    },
  },
  -- preview = {
  --   default     = 'bat',
  -- },
  keymap = {
    -- These override the default tables completely
    -- no need to set to `false` to disable a bind
    -- delete or modify is sufficient
    builtin = {
      ["?"]          = "toggle-preview",
      ["<C-j>"]      = "preview-page-down",
      ["<C-k>"]      = "preview-page-up",
      ["<C-w>"]      = "toggle-preview-wrap",
    }
  }
}
