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

local lsp_on_attach = function(_, bufnr)
  local function bmap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end

  bmap("n", "<localleader>f", "<CMD>lua vim.lsp.buf.format({async = true})<CR>", opts)
  bmap("n", "<localleader>q", "<CMD>lua vim.diagnostic.setloclist()<CR>", opts)

  -- jump diagnostic, Lspsag is the replacement

  bmap( "n", "[e", "<CMD>Lspsaga diagnostic_jump_next<CR>", opts)
  bmap( "n", "]e", "<CMD>Lspsaga diagnostic_jump_prev<CR>", opts)

  bmap( "n", "[D", "<Cmd>lua vim.diagnostic.goto_prev({float = true})<CR>", opts)
  bmap( "n", "]D", "<Cmd>lua vim.diagnostic.goto_next({float = true})<CR>", opts)

  bmap( "n", "[d", "<CMD>Lspsaga diagnostic_jump_next<CR>", opts)
  bmap( "n", "]d", "<CMD>Lspsaga diagnostic_jump_prev<CR>", opts)
end

local lsp_on_attach_power = function(_, bufnr)
  lsp_on_attach(_, bufnr)

  local function bmap(...)
    vim.api.nvim_buf_set_keymap(bufnr, ...)
  end

  -- bmap("n", "K", "<CMD>lua vim.lsp.buf.hover()<CR>", opts)
  bmap("n", "K", "<CMD>lua require('lspsaga.hover').render_hover_doc()<CR>", opts)
  -- WIP
  bmap("n", "L", "<CMD>lua require'lspsaga.diagnostic'.show_line_diagnostics()<CR>", opts)
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

local capabilitiesFn = function()
  -- return SR("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())
  -- https://github.com/hrsh7th/nvim-cmp
  return SR("cmp_nvim_lsp").default_capabilities()
end

M.handler = handlers
M.lsp_on_attach = lsp_on_attach
M.lsp_on_attach_power = lsp_on_attach_power
M.capabilitiesFn = capabilitiesFn
M.capabilities = SR("cmp_nvim_lsp").default_capabilities(vim.lsp.protocol.make_client_capabilities())

return M
