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

function _G.ReloadConfig() -- ReloadConfig {{{1
	local hls_status = vim.v.hlsearch
	for name, _ in pairs(package.loaded) do
		if name:match("^cnull") then
			package.loaded[name] = nil
		end
	end

	dofile(vim.env.MYVIMRC)
	if hls_status == 0 then
		vim.opt.hlsearch = false
	end
end

-- vim.api.nvim_set_keymap('n', '<space>vs', '<Cmd>lua ReloadConfig()<CR>', { silent = true, noremap = true })
vim.cmd("command! ReloadConfig lua ReloadConfig()")

-- LSP Config  {{{1

local nvim_lsp = require("lspconfig")

-- {{{1 on_attach_keymapping
-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach_keymapping = function(_, bufnr)
	local function buf_set_keymap(...)
		vim.api.nvim_buf_set_keymap(bufnr, ...)
	end

	-- Enable completion triggered by <c-x><c-o>
	-- vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

	-- Mappings.
	-- See `:help vim.lsp.*` for documentation on any of the below functions

	local opts = { noremap = true, silent = true }

	buf_set_keymap("n", "gD", "<CMD>lua vim.lsp.buf.declaration()<CR>", opts)
	buf_set_keymap("n", "gd", "<CMD>lua vim.lsp.buf.definition()<CR>", opts)
	buf_set_keymap("n", "K", "<CMD>lua vim.lsp.buf.hover()<CR>", opts)
	-- buf_set_keymap('n', 'K',               '<CMD>Lspsaga hover_doc<CR>',                                      opts)
	buf_set_keymap("n", "<localleader>k", "<CMD>lua vim.lsp.buf.hover()<CR>", opts)
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
	buf_set_keymap("n", "<localleader>rn", "<CMD>lua vim.lsp.buf.rename()<CR>", opts)
	-- buf_set_keymap('n', '<localleader>rn', '<CMD>:Lspsaga rename<CR>',                                     opts)

	buf_set_keymap("n", "<localleader>ca", "<CMD>lua vim.lsp.buf.code_action()<CR>", opts)
	buf_set_keymap("n", "gr", "<CMD>lua vim.lsp.buf.references()<CR>", opts)
	buf_set_keymap("n", "<localleader>f", "<CMD>lua vim.lsp.buf.formatting()<CR>", opts)

	-- TODO
	-- buf_set_keymap('n', '<localleader>ca', "<cmd>lua require('lspsaga.codeaction').code_action()<CR>",             opts)
	-- buf_set_keymap('v', '<localleader>ca', ":<C-U>lua require('lspsaga.codeaction').range_code_action()<CR>",      opts)
	--
	-- " nnoremap <silent><leader>ca <cmd>lua require('lspsaga.codeaction').code_action()<CR>
	-- " vnoremap <silent><leader>ca :<C-U>lua require('lspsaga.codeaction').range_code_action()<CR>

	-- UI Customization
	-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization
end

-- Lua LSP {{{1
nvim_lsp.sumneko_lua.setup({
	settings = {
		Lua = {
			runtime = {
				-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
				version = "LuaJIT",
				-- Setup your lua path
				path = "/usr/local/bin/lua",
			},
			diagnostics = {
				-- Get the language server to recognize the `vim` global
				globals = { "vim" },
				enable = true,
			},
			workspace = {
				-- Make the server aware of Neovim runtime files
				library = vim.api.nvim_get_runtime_file("", true),
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
		},
	},
})

------------------------------------------------------------------------------ }}}1

-- Require diagnostics  {{{1
-- WIP Able to see the highlight, but no popup
-- Setup diagnostics formaters and linters can be use for non LSP
nvim_lsp.diagnosticls.setup({
	on_attach = on_attach_keymapping,
	cmd = { "diagnostic-languageserver", "--stdio" },
	filetypes = {
		"javascript",
		"javascriptreact",
		"json",
		"typescript",
		"typescriptreact",
		"css",
		"less",
		"scss",
		"markdown",
		"pandoc",
		"lua",
		"vim",
		"sh",
	},
	init_options = {
		linters = {
			shellcheck = {
				command = "shellcheck",
				debounce = 100,
				args = { "--format", "json", "-" },
				sourceName = "shellcheck",
				parseJson = {
					line = "line",
					column = "column",
					endLine = "endLine",
					endColumn = "endColumn",
					message = "${message} [${code}]",
					security = "level",
				},
				securities = {
					error = "error",
					warning = "warning",
					info = "info",
					style = "hint",
				},
			},
			eslint = {
				command = "eslint_d",
				rootPatterns = { ".git" },
				debounce = 100,
				args = { "--stdin", "--stdin-filename", "%filepath", "--format", "json" },
				sourceName = "eslint_d",
				parseJson = {
					errorsRoot = "[0].messages",
					line = "line",
					column = "column",
					endLine = "endLine",
					endColumn = "endColumn",
					message = "[eslint] ${message} [${ruleId}]",
					security = "severity",
				},
				securities = {
					[2] = "error",
					[1] = "warning",
				},
			},
			vint = {
				command = "vint",
				debounce = 100,
				args = { "--enable-neovim", "-" },
				offsetLine = 0,
				offsetColumn = 0,
				sourceName = "vint",
				formatLines = 1,
				formatPattern = { "[^:]+:(\\d+):(\\d+):\\s*(.*)(\\r|\\n)*$", { line = 1, column = 2, message = 3 } },
			},
			zsh = {
				command = "zsh",
				args = { "-n", "%file" },
				isStdout = false,
				isStderr = true,
				sourceName = "zsh",
				formatLines = 1,
				formatPattern = { "^[^:]+:(\\d+):\\s+(.*)$", { line = 1, message = 2 } },
			},
		},
		filetypes = {
			javascript = "eslint",
			javascriptreact = "eslint",
			typescript = "eslint",
			typescriptreact = "eslint",
			sh = "shellcheck",
			vim = "vint",
		},
		formatters = {
			eslint_d = {
				command = "eslint_d",
				args = { "--stdin", "--stdin-filename", "%filename", "--fix-to-stdout" },
				rootPatterns = { ".git" },
			},
			prettier = {
				command = "prettier",
				args = { "--stdin-filepath", "%filename" },
			},
			shfmt = {
				command = "shfmt",
				args = { "-i", "2", "-bn", "-ci", "-sr" },
			},
			stylua = {
				command = "stylua",
				args = {},
			},
		},
		formatFiletypes = {
			sh = "shfmt",
			css = "prettier",
			javascript = "eslint_d",
			javascriptreact = "eslint_d",
			scss = "prettier",
			less = "prettier",
			typescript = "eslint_d",
			typescriptreact = "eslint_d",
			json = "prettier",
			markdown = "prettier",
			lua = "prettier",
		},
	},
})

-- }}}1

-- Enable diagnostics {{{1
-- UI customization https://github.com/neovim/nvim-lspconfig/wiki/UI-customization
local border = {
	{ "🭽", "FloatBorder" },
	{ "▔", "FloatBorder" },
	{ "🭾", "FloatBorder" },
	{ "▕", "FloatBorder" },
	{ "🭿", "FloatBorder" },
	{ "▁", "FloatBorder" },
	{ "🭼", "FloatBorder" },
	{ "▏", "FloatBorder" },
}

-- LSP settings (for overriding per client)
local handlers = {
	["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
	["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
	["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, { border = border }),
}

vim.o.updatetime = 250
vim.api.nvim_command([[ autocmd! CursorHold,CursorHoldI * lua vim.diagnostic.open_float(nil, {focus=false}) ]])
vim.api.nvim_command([[ autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335]])
vim.api.nvim_command([[ autocmd! ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]])

vim.diagnostic.config({
	signs = true,
	underline = true,
	update_in_insert = true,
	virtual_text = false,
	float = {
		source = "always",
		border = border,
	},
})

-- Something I learned from those setting.
-- vim.diagnostic.config sets the default config, in the handle, we can overwrite those config
-- Floating window is opened by CursorHold, so I can't overwrite it in handler
-- I can also overwrite open_floating_preview function to use my config
-- The handlers can be used to overwrite behaviours

-- LSP Config setup  {{{1
-- Use a loop to conveniently both setup defined servers
-- and map buffer local keybindings when the language server attaches
-- local servers = { 'pyright', 'tsserver', 'clojure_lsp', 'bashls'}
local servers = { "pyright", "tsserver", "clojure_lsp" }
for _, lsp in pairs(servers) do
	local not_created = not nvim_lsp[lsp].commands_created
	if not_created then
		nvim_lsp[lsp].setup({
			on_attach = on_attach_keymapping,
			handlers = handlers,
		})
	end
end

-- SIGN {{{1
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
	local hl = "DiagnosticSign" .. type
	vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- }}}

require("hop").setup()
-- print("mapping.lua loaded")
