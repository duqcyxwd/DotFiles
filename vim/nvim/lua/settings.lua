-- diagnostics {{{1
-- UI customization https://github.com/neovim/nvim-lspconfig/wiki/UI-customization

require('nvim_utils')

-- local border = {
--  { "🭽", "FloatBorder" },
--  { "▔", "FloatBorder" },
--  { "🭾", "FloatBorder" },
--  { "▕", "FloatBorder" },
--  { "🭿", "FloatBorder" },
--  { "▁", "FloatBorder" },
--  { "🭼", "FloatBorder" },
--  { "▏", "FloatBorder" },
-- }

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
    diagnostics_enabled = false
  else
    vim.diagnostic.enable()
    diagnostics_enabled = true
  end
end


local diagnostics_float_enabled = false
vim.diagnostic.float_toggle = function()
  local group = {
    DIAGNOSTICS_FLOAT = {
      {'CursorHold,CursorHoldI', '*', 'lua vim.diagnostic.open_float(nil, {focus=false})'}}
  }
  if diagnostics_float_enabled then
    nvim_create_augroups({ DIAGNOSTICS_FLOAT = {} })
    diagnostics_float_enabled = false
  else
    nvim_create_augroups(group)
    diagnostics_float_enabled = true
  end
end
vim.diagnostic.float_toggle()

-- SIGN {{{1
-- local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }

for type, icon in pairs(signs) do
  local hl = "DiagnosticSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

vim.cmd("highlight! link LspDiagnosticsDefaultError WarningMsg")


-- WIP Test
