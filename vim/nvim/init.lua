require("funcs.global")

vim.g.verbosefile = vim.fn.stdpath('cache') .. '/nvim/nvim.log'
vim.g.verbose = 0

R("config.pre")

if vim.g.neovide then
  R('neovide')
end

R("config.options")
R("config.lazy")
R("config.keys")
-- TODO Lazy load on event
R("config.color")
-- TODO Lazy load on event
R("config.autocmds")
-- TODO Lazy load on event
R("config.diagnostic")

-- Load before and after
R_VIM("$XDG_CONFIG_HOME/vim/lazy-adapter.vim")
R_VIM("$XDG_CONFIG_HOME/vim/vim-util/vim-fns.vim")
