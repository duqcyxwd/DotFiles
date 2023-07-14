require("nvim_utils")
-- diagnostic Autocmd

vim.o.updatetime = 250
vim.api.nvim_command([[ autocmd! ColorScheme * highlight NormalFloat guibg=#1f2335]])
vim.api.nvim_command([[ autocmd! ColorScheme * highlight FloatBorder guifg=white guibg=#1f2335]])

local autocmds = {

  default = {
    -- Keep folding when switch buffer
    -- Keep this off in diff mode
    { "BufEnter",   "*",        "if !&diff |silent! loadview | endif" },
    { "BufLeave",   "*",        "if !&diff |silent! mkview | endif" },

    -- mkdview will save
    -- VimLeave called when exit vim. Last edit location will be saved
    { "VimLeave",   "*",        "if !&diff |silent! mkview | endif" },
    -- " Manully update last open session so __LAST__ will not be updated
    { "VimLeave",   "*",        "call SaveCurrentSessions()" },

    -- Large file loading: disable syntax for large file and syntax can be turn on by syntax on
    { "BufReadPre", "*",        'if getfsize(expand("%")) > 150000 | syn off | set foldmethod=manual | set noswapfile | echom "XXX Large file detected" | endif' },

    -- https://stackoverflow.com/questions/178257/how-to-avoid-syntax-highlighting-for-large-files-in-vim
    -- autocmd BufWinEnter * if line2byte(line("$") + 1) > 1000000 | syntax clear | endif
    -- autocmd BufReadPre * if getfsize(expand("%")) > 10000000 | syntax off | endif

  },
  commentary_config = {
    -- tpope/vim-commentary
    { "FileType", "robot",          "setlocal commentstring=#\\ %s" },
    { "FileType", "clojure",        "setlocal commentstring=;;\\ %s" },
    { "FileType", "lisp,racket",    "setlocal commentstring=;;\\ %s" },
    { "FileType", "resolv,crontab", "setlocal commentstring=#\\ %s" },

  },

}

nvim_create_augroups(autocmds)

