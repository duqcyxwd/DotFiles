-- https://gitlab.com/AckslD/config/-/blob/master/nvim/lua/keys.lua

local M = {}



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


-- {{{1 on_attach_keymapping
-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
M.lsp_on_attach = function(_, bufnr)
	local function buf_set_keymap(...)
		vim.api.nvim_buf_set_keymap(bufnr, ...)
	end

	local opts = { noremap = true, silent = true }

	-- lspsaga config: https://github.com/tami5/lspsaga.nvim/wiki

	buf_set_keymap("n", "gD", "<CMD>lua vim.lsp.buf.declaration()<CR>", opts)
	buf_set_keymap("n", "gd", "<CMD>lua vim.lsp.buf.definition()<CR>", opts)
	buf_set_keymap("n", "K", "<CMD>lua vim.lsp.buf.hover()<CR>", opts)
	-- buf_set_keymap('n', 'K',               '<CMD>Lspsaga hover_doc<CR>',                                            opts)
	-- buf_set_keymap("n", "<localleader>k", "<CMD>lua vim.lsp.buf.hover()<CR>",                                       opts)
	buf_set_keymap("n", "<localleader>k", "<CMD>lua require('lspsaga.hover').render_hover_doc()<CR>", opts)

	buf_set_keymap("n", "gi", "<CMD>lua vim.lsp.buf.implementation()<CR>", opts)
	-- buf_set_keymap('n', '<C-k>',           '<CMD>lua vim.lsp.buf.signature_help()<CR>',                             opts)
	buf_set_keymap("n", "<localleader>wa", "<CMD>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)
	buf_set_keymap("n", "<localleader>wr", "<CMD>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)
	buf_set_keymap(
		"n",
		"<localleader>wl",
		"<CMD>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
		opts
	)

	buf_set_keymap("n", "<localleader>D", "<CMD>lua vim.lsp.buf.type_definition()<CR>", opts)
	buf_set_keymap("n", "<localleader>gD", "<CMD>lua require'lspsaga.provider'.preview_definition()<CR>", opts)

	buf_set_keymap("n", "<localleader>rn", "<CMD>lua vim.lsp.buf.rename()<CR>", opts)
	buf_set_keymap("n", "<localleader>rN", "<CMD>:Lspsaga rename<CR>", opts)

	buf_set_keymap("n", "<localleader>ca", "<CMD>lua vim.lsp.buf.code_action()<CR>", opts)
	buf_set_keymap("n", "<localleader>cA", "<CMD>lua require('lspsaga.codeaction').code_action()<CR>", opts)
	buf_set_keymap("n", "gr", "<CMD>lua vim.lsp.buf.references()<CR>", opts)

	buf_set_keymap("n", "<localleader>f", "<CMD>lua vim.lsp.buf.formatting()<CR>", opts)

	buf_set_keymap("n", "[e", "<CMD>Lspsaga diagnostic_jump_next<CR>", opts)
	buf_set_keymap("n", "]e", "<CMD>Lspsaga diagnostic_jump_prev<CR>", opts)

	-- buf_set_keymap('n', '<localleader>ca', "<cmd>lua require('lspsaga.codeaction').code_action()<CR>",             opts)
	-- buf_set_keymap('v', '<localleader>ca', ":<C-U>lua require('lspsaga.codeaction').range_code_action()<CR>",      opts)
	--

	-- "   buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
	-- "   buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
	-- "   buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
	-- "   buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
	-- "   buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
	-- "   buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
	-- "   buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
	-- "   buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
	-- "   buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
	-- "   buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
	-- "   buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
	-- "   buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
	-- "   buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
	-- "   buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
	-- "   buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
	-- "   buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
	-- "   buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
end

return M
