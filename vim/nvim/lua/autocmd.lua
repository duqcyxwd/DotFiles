

-- diagnostic Autocmd
vim.o.updatetime = 250
vim.api.nvim_command([[ autocmd! CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false}) ]])
-- vim.api.nvim_command([[ autocmd! CursorHold * lua vim.diagnostic.open_float(nil, {focus=false}) ]])
vim.api.nvim_command([[ autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335]])
vim.api.nvim_command([[ autocmd! ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]])
