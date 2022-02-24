-- https://gitlab.com/AckslD/config/-/blob/master/nvim/lua/keys.lua

local M = {}

-- https://gitlab.com/AckslD/config/-/blob/master/nvim/lua/keys.lua

local opts = { noremap = true, silent = true }

-- {{{1 on_attach_keymapping
-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
M.lsp_on_attach = function(_, bufnr)
	local function bmap(...)
		vim.api.nvim_buf_set_keymap(bufnr, ...)
	end

	bmap("n", "<localleader>f", "<CMD>lua vim.lsp.buf.formatting()<CR>", opts)
	bmap("n", "<localleader>q", "<CMD>lua vim.diagnostic.setloclist()<CR>", opts)
end

-- lsp_on_attach_power {{{1
-- Power on_attach function, maps all binding including go to definition
M.lsp_on_attach_power = function(_, bufnr)
	M.lsp_on_attach(_, bufnr)

	local function bmap(...)
		vim.api.nvim_buf_set_keymap(bufnr, ...)
	end
	-- bmap("n", "K", "<CMD>lua vim.lsp.buf.hover()<CR>", opts)
	bmap("n", "K", "<CMD>lua require('lspsaga.hover').render_hover_doc()<CR>", opts)
	bmap("n", "<localleader>K", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)

	-- lspsaga config: https://github.com/tami5/lspsaga.nvim/wiki

	bmap("n", "gD", "<CMD>lua vim.lsp.buf.declaration()<CR>", opts)
	bmap("n", "gd", "<CMD>lua vim.lsp.buf.definition()<CR>", opts)

	-- bmap("n", "gi", "<CMD>lua vim.lsp.buf.implementation()<CR>", opts)
	-- bmap("n", "gr", "<CMD>lua vim.lsp.buf.references()<CR>", opts)

	-- bmap("n", "<localleader>rn", "<CMD>lua vim.lsp.buf.rename()<CR>", opts)
	bmap("n", "<localleader>rn", "<CMD>:Lspsaga rename<CR>", opts)

	bmap("n", "<localleader>D", "<CMD>lua vim.lsp.buf.type_definition()<CR>", opts)
	bmap("n", "<localleader>gD", "<CMD>lua require'lspsaga.provider'.preview_definition()<CR>", opts)

	-- bmap("n", "<localleader>ca", "<CMD>lua vim.lsp.buf.code_action()<CR>", opts)
	bmap("n", "<localleader>ca", "<cmd>lua require('lspsaga.codeaction').code_action()<CR>", opts)
	bmap("v", "<localleader>ca", ":<C-U>lua require('lspsaga.codeaction').range_code_action()<CR>", opts)

	bmap("n", "<localleader>wa", "<CMD>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
	bmap("n", "<localleader>wr", "<CMD>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
	bmap("n", "<localleader>wl", "<CMD>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", opts)
end
-- }}}1

-- WIP Use lua whichkey

---- set leader
---- vim.g.mapleader = '\\'
---- vim.g.maplocalleader = ','
----
--M.init = function ()
--	require("which-key").setup({
--	})
--	local wk = require("which-key")
--	wk.register({
--		["1"] = "which_key_ignore",  -- special label to hide it in the popup
--		f = {
--			name = "file", -- optional group name
--			n = { "New File" }, -- just a label. don't create any mapping
--			e = "Edit File", -- same as above
--			b = { function() print("bar") end, "Foobar" } -- you can also pass functions!
--		},
--	}, { prefix = "<Space>" })
--end

-- M.init()


return M
