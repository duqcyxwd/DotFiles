require("nvim_utils")

local autocmds = {
  special = {
    { "BufEnter", "keys.lua", "set foldmethod=marker" },
  },

  allFileTypes = {
    { "FileType", "clojure", "set foldexpr=nvim_treesitter#foldexpr() foldmethod=expr" },
    { "FileType", "fugitive", "nmap <buffer> q gq" },
    { "FileType", "fugitiveblame", "nmap <buffer> q gq" },
    { "FileType", "fzf", "nnoremap <buffer> <ESC> :q<CR>" },
    { "FileType", "git", "nmap <buffer> q :bd<CR>" },
    { "FileType", "lua", "set foldexpr=nvim_treesitter#foldexpr() foldmethod=expr tabstop=2 softtabstop=2 shiftwidth=2" },
    { "FileType", "robot", "setlocal foldmethod=indent" },
    { "FileType", "sagahover", "nnoremap <buffer> <Esc> :q<CR>" },
    { "FileType", "startify", "DisableWhitespace" },
    { "FileType", "vim", "setlocal foldmethod=marker" },
  },
  Session = {
    -- { "VimLeave", "*", "SSave! cached | echom 'Save last session'" },
    -- testing
  },

  run = {
    { "BufEnter", "*.graphqls", "set syntax=graphql filetype=graphql" },
    { "BufEnter", "*.py", 'let @g=":w\\<CR>:sp |terminal python %\\<CR>"' },
    { "BufEnter", "*.js", 'let @g=":w\\<CR>:sp |terminal node %\\<CR>"' },
    { "BufEnter", "*.zsh", 'let @g=":w\\<CR>:sp |terminal %\\<CR>"' },
  },
}

nvim_create_augroups(autocmds)

-- autogroup toggle
local diagnostics_float_enabled = false
local float_toggle_group = {
  DIAGNOSTICS_FLOAT = {
    -- Only show diagnostic when edit complete. This will help me to get clean complete list
    { "CursorHold", "*", "lua require'lspsaga.diagnostic'.show_line_diagnostics()" },
  },
}
vim.diagnostic.float_toggle = function()
  if diagnostics_float_enabled then
    print("Disable diagnostics float")
    nvim_create_augroups({ DIAGNOSTICS_FLOAT = {} })
    diagnostics_float_enabled = false
  else
    print("Enable diagnostics float")
    nvim_create_augroups(float_toggle_group)
    diagnostics_float_enabled = true
  end
end

-- Enable by default
nvim_create_augroups(float_toggle_group)
-- vim.diagnostic.float_toggle()
