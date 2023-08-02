require("funcs.global")

R("config.pre")

if vim.g.neovide then
  R('neovide')
end

R("config.options")
R("config.lazy")
R("config.keys")

R("config.color")

-- TODO Lazy load on event
R("config.autocmds")
-- TODO Lazy load on event
R("config.diagnostic")

-- Load before and after
R_VIM("$XDG_CONFIG_HOME/vim/lazy-adapter.vim")


-- R_VIM("$XDG_CONFIG_HOME/vim/before/700-colorscheme.vim")
