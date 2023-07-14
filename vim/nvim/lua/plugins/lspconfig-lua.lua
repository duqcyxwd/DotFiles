local mapping = require("plugins.buffer-keys")

-- -- Lua LSP {{{1

require("lspconfig").lua_ls.setup({
	on_attach = mapping.lsp_on_attach_power,
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
				checkThirdParty = false,
			},
			-- Do not send telemetry data containing a randomized but unique identifier
			telemetry = {
				enable = false,
			},
		},
	},
})

------------------------------------------------------------------------------ }}}1
