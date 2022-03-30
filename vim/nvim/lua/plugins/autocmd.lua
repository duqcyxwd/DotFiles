require("nvim_utils")
-- diagnostic Autocmd

vim.o.updatetime = 250
vim.api.nvim_command([[ autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335]])
vim.api.nvim_command([[ autocmd! ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]])

local autocmds = {
  luafold = {

    { "FileType", "lua",      "set foldexpr=nvim_treesitter#foldexpr() foldmethod=expr tabstop=2 softtabstop=2 shiftwidth=2" },
    { "BufEnter", "keys.lua", "set foldmethod=marker" },

    -- Keep folding when switch buffer
    { "BufEnter", "*",        "silent! loadview" },
    { "BufLeave", "*",        "silent! mkview" },

  },
  clojure = {
    { "FileType", "clojure",   "set foldexpr=nvim_treesitter#foldexpr() foldmethod=expr" },
  },
  allFileTypes = {
    { "FileType", "sagahover",     "nnoremap <buffer> <Esc> :q<CR>" },
    { "FileType", "startify",      "DisableWhitespace" },
    { "FileType", "fzf",           "nnoremap <buffer> <ESC> :q<CR>" },
    { "FileType", "robot",         "setlocal foldmethod=indent" },
    { "FileType", "vim",           "setlocal foldmethod=marker" },
    { "FileType", "fugitiveblame", "nmap <buffer> q gq" },
    { "FileType", "fugitive",      "nmap <buffer> q gq" },
    { "FileType", "git",           "nmap <buffer> q :bd<CR>" },

  },
  Session = {
    -- { "VimLeave", "*", "SSave! cached | echom 'Save last session'" },
    -- testing
  },

  run = {
    { "FileType", "sh",    'let @g=":w\\<CR>:sp |terminal %\\<CR>"' },
    { "BufEnter", "*.py",  'let @g=":w\\<CR>:sp |terminal python %\\<CR>"' },
    { "BufEnter", "*.js",  'let @g=":w\\<CR>:sp |terminal node %\\<CR>"' },
    { "BufEnter", "*.zsh", 'let @g=":w\\<CR>:sp |terminal %\\<CR>"' },
  },

}

nvim_create_augroups(autocmds)

-- autogroup toggle
local diagnostics_float_enabled = false
vim.diagnostic.float_toggle = function()
  local group = {
    DIAGNOSTICS_FLOAT = {
      -- Only show diagnostic when edit complete. This will help me to get clean complete list
       {'CursorHold', '*', "lua require'lspsaga.diagnostic'.show_line_diagnostics()"}
     }
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
