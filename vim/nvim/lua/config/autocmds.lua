local core = require("funcs.nvim_core")

-- diagnostic Autocmd

vim.o.updatetime = 250
vim.cmd([[ autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335]])
vim.cmd([[ autocmd! ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]])


vim.cmd[[let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"]]
vim.cmd[[let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"]]

vim.cmd[[autocmd ColorScheme * highlight Comment cterm=italic gui=italic]]

local autocmds = {

  default = {

    -- Large file loading: disable syntax for large file and syntax can be turn on by syntax on
    -- { "BufReadPre", "*", 'if getfsize(expand("%")) > 150000 | syn off | set foldmethod=manual | set noswapfile | echom "XXX Large file detected" | endif' },

    -- https://stackoverflow.com/questions/178257/how-to-avoid-syntax-highlighting-for-large-files-in-vim
    -- autocmd BufWinEnter * if line2byte(line("$") + 1) > 1000000 | syntax clear | endif
    -- autocmd BufReadPre * if getfsize(expand("%")) > 10000000 | syntax off | endif

  },


  buffer_view = {
    -- Keep folding when switch buffer
    -- Keep this off in diff mode
    -- { "BufEnter", "*", "if !&diff |silent! loadview | endif" },
    -- -- VimLeave called when exit vim. Last edit location will be saved
    -- { "VimLeave", "*", "if !&diff |silent! mkview | endif" },
    -- { "BufLeave", "*", "if !&diff |silent! mkview | endif" },
  },

  commentary_config = {
    -- tpope/vim-commentary
    { "FileType", "robot",          "setlocal commentstring=#\\ %s" },
    { "FileType", "clojure",        "setlocal commentstring=;;\\ %s" },
    { "FileType", "lisp,racket",    "setlocal commentstring=;;\\ %s" },
    { "FileType", "resolv,crontab", "setlocal commentstring=#\\ %s" },
  },

  my_fzf = {
    { "FileType", "fzf", "nnoremap <buffer> <esc> :q<CR>" },
  },

  special = {
    { "BufEnter", "keys.lua", "set foldmethod=marker" },
  },

  allFileTypes = {
    { "FileType", "clojure",       "set foldexpr=nvim_treesitter#foldexpr() foldmethod=expr" },
    { "FileType", "fugitive",      "nmap <buffer> q gq" },
    { "FileType", "fugitiveblame", "nmap <buffer> q gq" },
    { "FileType", "git",           "nmap <buffer> q :bd<CR>" },
    { "FileType", "lua",           "set foldexpr=nvim_treesitter#foldexpr() foldmethod=expr tabstop=2 softtabstop=2 shiftwidth=2" },
    { "FileType", "robot",         "setlocal foldmethod=indent" },
    { "FileType", "sagahover",     "nnoremap <buffer> <Esc> :q<CR>" },
    { "FileType", "startify",      "DisableWhitespace" },
    { "FileType", "vim",           "setlocal foldmethod=marker" },
  },
  Session = {
    -- { "VimLeave", "*", "SSave! cached | echom 'Save last session'" },
    -- testing
  },

  run = {
    { "BufEnter", "*.graphqls", "set syntax=graphql filetype=graphql" },
    { "BufEnter", "*.py",       'let @g=":w\\<CR>:sp |terminal python %\\<CR>"' },
    { "BufEnter", "*.js",       'let @g=":w\\<CR>:sp |terminal node %\\<CR>"' },
    { "BufEnter", "*.zsh",      'let @g=":w\\<CR>:sp |terminal %\\<CR>"' },
  },
}


-- autogroup toggle
local diagnostics_float_enabled = true
local float_toggle_group = {
  DIAGNOSTICS_FLOAT = {
    -- Only show diagnostic when edit complete. This will help me to get clean complete list
    { "CursorHold", "*", "lua require'lspsaga.diagnostic'.show_line_diagnostics()" },
  },
}
vim.diagnostic.float_toggle = function()
  if diagnostics_float_enabled then
    print("Disable diagnostics float")
      core.nvim_create_augroups({ DIAGNOSTICS_FLOAT = {} })
    diagnostics_float_enabled = false
  else
    print("Enable diagnostics float")
    core.nvim_create_augroups(float_toggle_group)
    diagnostics_float_enabled = true
  end
end

-- Enable by default
core.nvim_create_augroups(autocmds)
core.nvim_create_augroups(float_toggle_group)
