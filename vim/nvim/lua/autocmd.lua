require("nvim_utils")

-- diagnostic Autocmd
vim.o.updatetime = 250
vim.api.nvim_command([[ autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335]])
vim.api.nvim_command([[ autocmd! ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]])

local autocmds = {
  luafold = {
    { "FileType", "lua", "set foldexpr=nvim_treesitter#foldexpr() foldmethod=expr" },
  },
}

nvim_create_augroups(autocmds)
