local M = {}
local opts = { noremap = true, silent = true }

-- UI Customization
-- https://github.com/neovim/nvim-lspconfig/wiki/UI-customization
local border = {
  { "0", "FloatBorder" },
  { "0", "FloatBorder" },
  { "0", "FloatBorder" },
  { "0", "FloatBorder" },
  { "0", "FloatBorder" },
  { "0", "FloatBorder" },
  { "0", "FloatBorder" },
  { "0", "FloatBorder" },
}

local handlers = {
  ["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = border }),
  ["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = border }),
  ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, { border = border }),
}

local capabilitiesFn = function()
  -- return SR("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())
  -- https://github.com/hrsh7th/nvim-cmp
  return SR("cmp_nvim_lsp").default_capabilities()
end

function M.lsp_keymap_global()
  -- lsp mapping from lsp-config https://github.com/neovim/nvim-lspconfig
  -- Global mappings.
  -- See `:help vim.diagnostic.*` for documentation on any of the below functions
  vim.keymap.set('n', '<localleader>e', ":lua vim.diagnostic.open_float()<cr>")
  vim.keymap.set('n', '<localleader>q', ":lua vim.diagnostic.setloclist()<cr>")
  vim.keymap.set('n', '[d',             ":lua vim.diagnostic.goto_prev()<cr>")
  vim.keymap.set('n', ']d',             ":lua vim.diagnostic.goto_next()<cr>")
  vim.keymap.set('n', '[e',             ":Lspsaga diagnostic_jump_next<CR>")
  vim.keymap.set('n', ']e',             ":Lspsaga diagnostic_jump_prev<CR>")
end

function M.lsp_keymap_buffer(ev)
  -- Enable completion triggered by <c-x><c-o>
  vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'


  vim.keymap.set('n', '<localleader>wa', vim.lsp.buf.add_workspace_folder,                                        opts)
  vim.keymap.set('n', '<localleader>wr', vim.lsp.buf.remove_workspace_folder,                                     opts)
  vim.keymap.set('n', '<localleader>wl', function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, opts)

  -- Buffer local mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local opts = { buffer = ev.buf }
  vim.keymap.set('n', 'gD',              ":lua vim.lsp.buf.declaration()<cr>",               opts)
  vim.keymap.set('n', 'gd',              ":lua vim.lsp.buf.definition()<cr>",                opts)
  vim.keymap.set('n', 'K',               ":lua vim.lsp.buf.hover()<cr>",                     opts)
  vim.keymap.set('n', 'gr',              ":lua vim.lsp.buf.references()<cr>",                opts)
  vim.keymap.set('n', 'gi',              ":lua vim.lsp.buf.implementation()<cr>",            opts)
  vim.keymap.set('n', '<localleader>k',  ":lua vim.lsp.buf.signature_help()<cr>",            opts)
  vim.keymap.set('n', '<localleader>D',  ":lua vim.lsp.buf.type_definition()<cr>",           opts)
  vim.keymap.set('n', '<localleader>rn', ":lua vim.lsp.buf.rename()<cr>",                    opts)
  vim.keymap.set('v', '<localleader>ca', ":lua vim.lsp.buf.code_action()<cr>",               opts)
  vim.keymap.set('n', '<localleader>f',  function() vim.lsp.buf.format { async = true } end, opts)


  -- Other plugins
  vim.keymap.set('n', 'L',               ":Lspsaga show_line_diagnostics<cr>", opts)
  vim.keymap.set('n', '<localleader>ca', ":CodeActionMenu<cr>",                opts)
  vim.keymap.set('n', '<localleader>o',  ":Lspsaga outline<cr>",               opts)
  vim.keymap.set('n', 'gp',              ":Lspsaga peek_definition<cr>",       opts)
  vim.keymap.set('n', 'gr',              ":FzfLua lsp_references<cr>",         opts)
  vim.keymap.set('n', '<localleader>js',  ":FzfLua lsp_document_symbols<cr>",   opts)


end

M.handler = handlers
M.capabilitiesFn = capabilitiesFn
M.capabilities = SR("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

return M
