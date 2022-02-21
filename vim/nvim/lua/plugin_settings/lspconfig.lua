local nvim_lsp = require("lspconfig")
local mapping  = require('plugin_settings.mapping')


-- LSP Config setup  {{{1
-- Use a loop to conveniently both setup defined servers
-- and map buffer local keybindings when the language server attaches
-- local servers = { 'pyright', 'tsserver', 'clojure_lsp', 'bashls'}
local servers = { "pyright", "tsserver", "clojure_lsp" }
-- local servers = {}

-- UI Customization
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization


-- LSP settings (for overriding per client)
local handlers = {
	["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
	["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
	["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, { border = border }),
}

-- Something I learned from those setting.
-- vim.diagnostic.config sets the default config, in the handle, we can overwrite those config
-- Floating window is opened by CursorHold, so I can't overwrite it in handler
-- I can also overwrite open_floating_preview function to use my config
-- The handlers can be used to overwrite behaviours


-- Setup lspconfig.
local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())

for _, lsp in pairs(servers) do
	local not_created = not nvim_lsp[lsp].commands_created
	if not_created then
		nvim_lsp[lsp].setup({
			on_attach = mapping.lsp_on_attach_power,
			handlers = handlers,
			capabilities = capabilities,
		})
	end
end

-- }}}

