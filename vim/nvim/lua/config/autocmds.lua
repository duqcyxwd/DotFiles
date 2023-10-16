local core = require("funcs.nvim_core")
local toggle = require("funcs.toggle")
local lsp_util = require("config.lspconfig-util")

-- diagnostic Autocmd

local M = {}

vim.o.updatetime = 250
vim.cmd([[ autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335 ]])
vim.cmd([[ autocmd! ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335 ]])
vim.cmd [[ autocmd! ColorScheme * highlight Comment cterm=italic gui=italic ]]


vim.cmd [[let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"]]
vim.cmd [[let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"]]


local markdown_key_map = function()
  vim.cmd([[
    nnoremap <buffer> <Tab> >>
    nnoremap <buffer> <S-Tab> <<

    vnoremap <buffer> <Tab> >
    vnoremap <buffer> <S-Tab> <

    inoremap <buffer> <Tab> >
    inoremap <buffer> <S-Tab> <

    imap <buffer> <Tab> <C-t>
    imap <buffer> <S-Tab> <C-d>
  ]])
end

local autocmds = {
  default = {

    -- Large file loading: disable syntax for large file and syntax can be turn on by syntax on
    { "BufReadPre", "*", 'if getfsize(expand("%")) > 150000 | syn off | set foldmethod=manual noswapfile nonumber norelativenumber  | echom "XXX Large file detected" | endif' },

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


  line_number = {
    { { "BufEnter", "FocusGained", "InsertLeave", "WinEnter" }, "*", toggle.r_number.enterHook },
    { { "BufLeave", "FocusLost",   "InsertEnter", "WinLeave" }, "*", toggle.r_number.leaveHook },
    -- { { "BufEnter", "FocusGained", "InsertLeave", "CmdlineLeave", "WinEnter" }, "*", toggle.relative_num_buffer_hook.enter },
    -- { { "BufLeave", "FocusLost",   "InsertEnter", "CmdlineEnter", "WinLeave" }, "*", toggle.relative_num_buffer_hook.leave },
  },

  DEV_DEV = {
    -- { "DirChanged", "*",   "echom 'Current Directory changed'; pwd" },
    -- { "DirChanged", "*",   function () print("Current directory changed."); print(vim.fn.getcwd()) end },
    -- { "BufEnter", "*",   function () print(vim.fn.getcwd()) end },
    -- Check Event
    --

    -- {"BufNewFile",   "*", "echom 'BufNewFile'"},
    -- {"BufEnter",     "*", "echom 'BufEnter'"},
    -- {"BufWritePre",  "*", "echom 'BufWritePre'"},
    -- {"BufWritePost", "*", "echom 'BufWritePost'"},
    -- {"VimEnter",     "*", "echom 'VimEnter'"},
    -- {"VimLeave",     "*", "echom 'VimLeave'"},
    -- {"WinEnter",     "*", "echom 'WinEnter'"},
    -- {"WinLeave",     "*", "echom 'WinLeave'"},
    -- {"InsertEnter",  "*", "echom 'InsertEnter'"},
    -- {"InsertLeave",  "*", "echom 'InsertLeave'"},

    -- {"FileType",     "*", "echom 'FileType'"},
  },

}

local file_type_autocmds = {

  DEFAULT_FILE_TYPE = {
    -- Fix <CR> in quickfix
    { "BufReadPost", "quickfix",      'nnoremap <buffer> <CR> <CR>' },

    { "FileType",    "clojure",       "set foldexpr=nvim_treesitter#foldexpr() foldmethod=expr" },
    { "FileType",    "fugitive",      "nmap <buffer> q gq" },
    { "FileType",    "fugitiveblame", "nmap <buffer> q gq" },
    { "FileType",    "git",           "nmap <buffer> q :bd<CR>" },
    { "FileType",    "json",          "setlocal foldexpr=nvim_treesitter#foldexpr() foldmethod=expr" },
    { "FileType",    "lua",           "setlocal foldexpr=nvim_treesitter#foldexpr() foldmethod=expr tabstop=2 softtabstop=2 shiftwidth=2" },
    { "FileType",    "robot",         "setlocal foldmethod=indent" },
    { "FileType",    "sagahover",     "nnoremap <buffer> <Esc> :q<CR>" },
    { "FileType",    "startify",      "DisableWhitespace" },
    { "FileType",    "vim",           "setlocal foldmethod=marker" },
    -- { "FileType",    "markdown",      "setlocal foldmethod=expr conceallevel=2 foldtext=foldtext()" },
    { "FileType",    "markdown",      "setlocal foldexpr=nvim_treesitter#foldexpr() foldmethod=expr conceallevel=2" },
    { "FileType",    "markdown",      markdown_key_map },
    { "FileType",    "zsh",           "setlocal foldmethod=marker" },
  },
}


-- My custom fold will be replace by pretty-fold
-- vim.opt.foldtext = 'v:lua.custom_fold_text()'
-- vim.opt.foldtext = 'v:lua.nvim_treesitter#foldexpr()'
function _G.custom_fold_text()
  local line = vim.fn.getline(vim.v.foldstart)

  local repeatsymbol = '-'
  local prefix = ''

  local w = vim.fn.winwidth(0) - vim.o.foldcolumn - (vim.o.number and 8 or 0)
  local foldSize = 1 + vim.v.foldend - vim.v.foldstart
  local foldSizeStr = ' ' .. foldSize .. ' lines '
  local foldLevelStr = string.rep('+--', vim.v.foldlevel)
  local lineCount = vim.fn.line('$')
  local foldPercentage = string.format('[%.1f', (foldSize * 1.0) / lineCount * 100) .. '%] '
  local expansionString = string.rep(repeatsymbol,
    w - vim.fn.strwidth(prefix .. foldSizeStr .. line .. foldLevelStr .. foldPercentage))

  return prefix .. line .. expansionString .. foldSizeStr .. foldPercentage .. foldLevelStr
end

-- autogroup toggle
local diagnostics_float_enabled = false
local float_toggle_group = {
  DIAGNOSTICS_FLOAT = {
    -- Only show diagnostic when edit complete. This will help me to get clean complete list
    { "CursorHold", "*", vim.lsp.buf.hover },
    -- { "CursorHold", "*", function() require 'lspsaga.diagnostic'.show_line_diagnostics() end },
    -- { "CursorHold", "*", "Lspsaga show_line_diagnostics ++unfocus" },
  },
}
if diagnostics_float_enabled then
  core.autogroup(float_toggle_group)
end
M.toggle_diagnostics = function()
  if diagnostics_float_enabled then
    print("Disable diagnostics float")
    core.autogroup({ DIAGNOSTICS_FLOAT = {} })
    diagnostics_float_enabled = false
  else
    print("Enable diagnostics float")
    core.autogroup(float_toggle_group)
    diagnostics_float_enabled = true
  end
end

-- Enable by default
core.autogroup(autocmds)
core.autogroup(file_type_autocmds)


-- Use LspAttach autocommand to only map the following keys
-- after the language server attaches to the current buffer
vim.api.nvim_create_autocmd('LspAttach', {
  group = vim.api.nvim_create_augroup('UserLspConfig', {}),
  callback = lsp_util.lsp_keymap_buffer
})

return M
