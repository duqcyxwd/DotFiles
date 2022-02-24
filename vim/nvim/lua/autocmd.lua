require("nvim_utils")

-- diagnostic Autocmd
vim.o.updatetime = 250
vim.api.nvim_command([[ autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335]])
vim.api.nvim_command([[ autocmd! ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]])

local autocmds = {
  luafold = {
    { "FileType", "lua", "set foldexpr=nvim_treesitter#foldexpr() foldmethod=expr" },
    { "BufEnter", "keys.lua", "set foldmethod=marker" },
  },
  vim = {
    { "FileType", "vim", "set foldmethod=marker" },
  },
  -- coc_explorer = {
  --   { "FileType", "coc-explorer", "map <buffer> <ESC> q" },
  --   { "BufEnter", "Coc Explorer", "map <buffer> <ESC> :echom 'hahahah'" },
  -- }
}
-- vim.cmd('autocmd FileType coc-explorer nmap <buffer> <Esc> q')
-- vim.cmd('autocmd FileType coc-explorer echom "coc-explorer running"')

nvim_create_augroups(autocmds)


-- autogroup toggle
local diagnostics_float_enabled = false
vim.diagnostic.float_toggle = function()
  local group = {
    DIAGNOSTICS_FLOAT = {
       -- {'CursorHold,CursorHoldI', '*', 'lua vim.diagnostic.open_float(nil, {focus=false})'}}
       {'CursorHold,CursorHoldI', '*', "lua require'lspsaga.diagnostic'.show_line_diagnostics()"}}
       -- {'CursorHold,CursorHoldI', '*', "lua require'lspsaga.diagnostic'.show_cursor_diagnostics()"}}
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
