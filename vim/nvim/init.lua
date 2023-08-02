require("funcs.global")

R_VIM("$XDG_CONFIG_HOME/vim/before/000-vim-settings.vim")
R_VIM("$XDG_CONFIG_HOME/vim/before/200-homepage.vim")

-- Loading lazy plugins as well as dependencies
R("config.lazy-plugins-loader")

-- R("lua.lazy")
-- R('lua.settings')
R_FOLD("plugins-before")
R("keys")
R("settings")
-- R_VIM_FOLD('after')
R_VIM("$XDG_CONFIG_HOME/vim/lazy-adapter.vim")
R_VIM("$XDG_CONFIG_HOME/vim/before/700-colorscheme.vim")

-- R_FOLD("plugins", { "cmp", "fzf" })

-- My plan
-- Auto load plugsin in auto config
-- Load plugsin specified plugin map
