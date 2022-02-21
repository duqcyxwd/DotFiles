local mapping = require("plugin_settings.mapping")

-- -- Lua LSP {{{1
-- local sumneko_lua_loaded = not vim.inspect(package.loaded["lspconfig.configs"].sumneko_lua)
-- if sumneko_lua_loaded then
-- end

require("lspconfig").sumneko_lua.setup({
	on_attach = mapping.lsp_on_attach_power,
	capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities()),
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

-- print(vim.inspect(package.loaded))
-- TODO Load only once
-- print(vim.inspect(require("lspconfig").sumneko_lua))
-- enew| pu=execute('lua print(vim.inspect(require("lspconfig").sumneko_lua))')
-- enew| pu=execute('lua print(vim.inspect(package.loaded["lspconfigs.configs"]))')
-- lua print(vim.inspect(package.loaded["lspconfig.configs"].sumneko_lua))
