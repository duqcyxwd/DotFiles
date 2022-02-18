-- print("Loading mapping.lua file")
-- https://gitlab.com/AckslD/config/-/blob/master/nvim/lua/keys.lua

-- keymap fn {{{1

local function set_keymap(mode, opts, keymaps)
	for _, keymap in ipairs(keymaps) do
		vim.api.nvim_set_keymap(mode, keymap[1], keymap[2], opts)
	end
end

-- https://gitlab.com/AckslD/config/-/blob/master/nvim/lua/keys.lua

-- leader
-- vim.g.mapleader = ','
-- vim.g.maplocalleader = "\\"

-- vim.g.mapleader = "\\"
-- vim.g.maplocalleader = ","

require("hop").setup()
