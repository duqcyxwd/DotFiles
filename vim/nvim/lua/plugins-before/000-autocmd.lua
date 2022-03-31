require("nvim_utils")
-- diagnostic Autocmd

vim.o.updatetime = 250
vim.api.nvim_command([[ autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335]])
vim.api.nvim_command([[ autocmd! ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]])

local autocmds = {

  default = {
    -- Keep folding when switch buffer
    { "BufEnter", "*",        "silent! loadview" },
    { "BufLeave", "*",        "silent! mkview" },

  },
  commentary_config = {
    -- tpope/vim-commentary
    { "FileType", "robot",          "setlocal commentstring=#\\ %s" },
    { "FileType", "clojure",        "setlocal commentstring=;;\\ %s" },
    { "FileType", "lisp,racket",    "setlocal commentstring=;;\\ %s" },
    { "FileType", "resolv,crontab", "setlocal commentstring=#\\ %s" },

  },

}

nvim_create_augroups(autocmds)

