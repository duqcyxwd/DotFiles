-- diagnostics {{{1
-- UI customization https://github.com/neovim/nvim-lspconfig/wiki/UI-customization

require('nvim_utils')

local border = {
  { "▔", "FloatBorder" },
  { "▔", "FloatBorder" },
  { "▔", "FloatBorder" },
  { " ", "FloatBorder" },
  { "▁", "FloatBorder" },
  { "▁", "FloatBorder" },
  { "▁", "FloatBorder" },
  { " ", "FloatBorder" },
}

vim.diagnostic.config({
  signs = true,
  underline = true,
  update_in_insert = true,
  virtual_text = false,
  float = {
    header = "Diagnostics  : ",
    source = "always",
    border = border,
    format = function(diagnostic)
      if diagnostic.user_data and diagnostic.user_data.code then
        return string.format("%s %s", diagnostic.user_data.code, diagnostic.message)
      else
        return diagnostic.message
      end
    end,
  },
})

local diagnostics_enabled = true
vim.diagnostic.toggle = function()
  if diagnostics_enabled then
    vim.diagnostic.disable()
    print("Disable diagnostics")
    diagnostics_enabled = false
  else
    vim.diagnostic.enable()
    print("Enable diagnostics")
    diagnostics_enabled = true
  end
end


-- SIGN {{{1
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }

for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.cmd("highlight! link LspDiagnosticsDefaultError WarningMsg")

