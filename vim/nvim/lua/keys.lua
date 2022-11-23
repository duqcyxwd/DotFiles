require("funcs.utility")
require("nvim_utils")
local brj = require("funcs.bracket_jump")

vim.g.mapleader = "\\"
vim.g.maplocalleader = ","

-- init which-key {{{1
-- stylua: ignore start
local status, wk = pcall(require, "which-key")
if not status then
  vim.notify("couldn't load which-key, skipping mappings")
  return
end

local space_key_nmap = {}
local space_key_vmap = {}

-- Local Utility functions {{{1
local function set_keymap(mode, opts, keymaps)
  for _, keymap in ipairs(keymaps) do
    vim.api.nvim_set_keymap(mode, keymap[1], keymap[2], merge(opts, keymap[3]))
  end
end

local function quick_swap(data)
  -- quick swap first two column in a table
  if type(data) == "table" and type(data[1]) == "string" then
    local temp = data[1]
    if data[2] == nil or data[2] == "" then
      data[2] = temp
    else
      data[1] = data[2]
      data[2] = temp
    end
  elseif type(data) == "table" then
    for _, v in pairs(data) do
      quick_swap(v)
    end
  end
end

local reformate_key_map = function(keymap)
  for _, mapping in pairs(keymap) do
    quick_swap(mapping)
  end
end

-- }}}1

-- Mapping
-- normal mapping {{{1


-- vim.cmd("vnoremap <silent> <C-Space> :lua require'nvim-treesitter.incremental_selection'.node_incremental()<CR>")
-- vim.cmd("vnoremap <silent> -         :lua require'nvim-treesitter.incremental_selection'.node_decremental()<CR>")

-- Bash like keys for the command line
vim.cmd("cnoremap <C-A> <Home>")
vim.cmd("cnoremap <C-E> <End>")
vim.cmd("cnoremap <C-K> <C-U>")
vim.cmd("cnoremap <C-P> <Up>")
vim.cmd("cnoremap <C-N> <Down>")

-- Start interactive EasyAlign in visual mode (e.g. vipga)
vim.cmd("xmap ga <Plug>(EasyAlign)")
-- Start interactive EasyAlign for a motion/text object (e.g. gaip)
vim.cmd("nmap ga <Plug>(EasyAlign)")

-- " Fix paste
-- " p will not overwrite register
-- " https://vim.fandom.com/wiki/Replace_a_word_with_yanked_text
-- " xnoremap p "_dP
vim.cmd('xnoremap <silent> p p:let @+=@0<CR>:let @"=@0<CR>')

-- WIP Add jump by j k
-- " j, k          Store relative line number jumps in the jumplist.
vim.cmd("nnoremap <expr> k (v:count > 1 ? \"m'\" . v:count : '') . 'k'")
vim.cmd("nnoremap <expr> j (v:count > 1 ? \"m'\" . v:count : '') . 'j'")


-- Normal mapping --{{{1
set_keymap("n", { noremap = true, silent = true }, {
  -- execute q macro
  { "Q", "@q" },

  -- vim.keymap is supported in latest neovim
  -- vim.keymap.set("n", "<Esc>", clear, { desc = "clear" })
  -- vim.keymap.set("n", "<Right>", brj.next, { desc = "next (])" })
  -- vim.keymap.set("n", "<Left>", brj.prev, { desc = "prev ([)" })
  { "<Left>",  ':lua R("funcs.bracket_jump").prev()<CR>'},
  { "<Right>", ':lua R("funcs.bracket_jump").next()<CR>'},
  { "<Esc>",   ':lua R("funcs.vim_utility").clear()<CR>'},

  -- yank to end of line
  -- { "Y", "y$" },

  -- yank/paste clipboard
  { "gy", '"+y' },
  { "gp", '"+p' },
  { "gP", '"+P' },
  { "gY", '"+y$' },
   {"gf", ':call GotoFirstFloat()<CR>'},

  { "j", 'gj' },
  { "k", 'gk' },

  { 'gs',      ':Gitsigns stage_hunk<CR>' },
  { 'gV',      '<Plug>(VM-Reselect-Last)', { noremap = false}},


  -- source config
  { "<C-s>", ':lua R("funcs.config").source()<CR>' },

  -- Resize split
  { "<S-Up>", ":resize +2<CR>" },
  { "<S-Down>", ":resize -2<CR>" },
  { "<S-Left>", ":vertical resize +2<CR>" },
  { "<S-Right>", ":vertical resize -2<CR>" },

  -- Quickfix
  -- { "<Up>",   ":copen<CR>" },
  -- { "<Down>", ":cclose<CR>" },
  -- {'<Left>', ':cprevious<CR>'},
  -- {'<Right>', ':cnext<CR>'},

  { "cg*", "*Ncgn" },
  { "g.", [[/\V<C-r>"<CR>cgn<C-a><Esc>]] },

  -- window
  {"gj", "<C-W>j"},
  {"gk", "<C-W>k"},
  {"gh", "<C-W>h"},
  {"gl", "<C-W>l"},

  -- Edit
  {"K",              ":call Show_documentation()<CR>"},
  {"<CR>",           "za"},
  {"gca",            ":call NERDComment('n', 'Toggle')<CR>"},

})


-- Leaderkey mapping --{{{1
set_keymap("n", { noremap = true, silent = true }, {
  { "<localleader>k", ":call Show_documentation()<CR>" },
  { "<localleader>j", ":call GotoFirstFloat()<CR>" },
})

-- Visual mode mapping --{{{1
set_keymap("v", { noremap = true, silent = true }, {
  {"9",         "c()<Esc>hp"},
  {"J",         ":move '<+1<CR>gv-gv"},
  {"K",         ":move '<-2<CR>gv-gv"},
  {"<C-Space>", ":lua require'nvim-treesitter.incremental_selection'.node_incremental()<CR>"},
  {"-",         ":lua require'nvim-treesitter.incremental_selection'.node_decremental()<CR>"},
})
-- }}}1

-- Buffer/Tab Switch {{{1
-- Map <Space>1..8 to buffers
-- Map [1..8 to tabs
for i = 1, 8, 1 do
  -- space_key_nmap[tostring(i)] = { "which_key_ignore", "<Plug>AirlineSelectTab" .. tostring(i) .. "<CR>" }
  space_key_nmap[tostring(i)] = { "which_key_ignore", "<Cmd>BufferLineGoToBuffer " .. tostring(i) .. "<CR>" }
  set_keymap("n", { noremap = true, silent = true }, {{ "[" ..tostring(i), tostring(i).."gt" } })
end

-- Impair keys and Bracket jump {{{1
-------------------------------------------------------------------------------
-- This is similaer to emacs transit mode

local impair_map_config = { --{{{2
  -- in pair

  -- Jump list
  { "[j", "<C-o>" },
  { "]j", "<C-i>" },

  -- Page down/up
  { "[d", "<PageUp>" },
  { "]d", "<PageDown>" },

  -- -- vim diff next hunk
  -- { "[c", "[c" },
  -- { "]c", "]c" },

  -- Git chagne Hunk
  { "[h", ":lua require'gitsigns'.prev_hunk({ preview = true})<CR>" },
  { "]h", ":lua require'gitsigns'.next_hunk({ preview = true})<CR>" },

  -- jump diagnostic
  { "[g", "<Cmd>lua vim.diagnostic.goto_prev({float = true})<CR>" },
  { "]g", "<Cmd>lua vim.diagnostic.goto_next({float = true})<CR>" },

  { "[e", "<CMD>Lspsaga diagnostic_jump_next<CR>" },
  { "]e", "<CMD>Lspsaga diagnostic_jump_prev<CR>" },


  { "[f", ":FloatermPrev<CR><C-Bslash><C-n>" },
  { "]f", ":FloatermNext<CR><C-Bslash><C-n>" },


  -- buffer
  { "[b", ":bprev<CR>" },
  { "]b", ":bnext<CR>" },
  { "[B", ":bfirst<CR>" },
  { "]B", ":blast<CR>" },

  -- t tabs
  { "[t", ":tabprev<CR>" },
  { "]t", ":tabnext<CR>" },
  { "[T", ":tabfirst<CR>" },
  { "]T", ":tablast<CR>" },

  -- quffer
  { "[q", ":qprev<CR>" },
  { "]q", ":qnext<CR>" },
  { "[Q", ":qfirst<CR>" },
  { "]Q", ":qlast<CR>" },

}


local default_impair_keys = { --{{{2
  -- Default unimar already supported, no mapping need but need to add them to shortcut
  "`", -- Mark
  "f", -- Next float term
  "c", -- vim diff next hunk
}


local brackets_keymap= { name = "+bracket jumps" } --{{{2

-- Add mapping to keymap
local impar_keys  = remove_dups(join(default_impair_keys, GetKeys(impair_map_config)))
for _, char in ipairs(impar_keys) do
  brackets_keymap[char] = {
    string.format("Set bracket jump (%s)", char),
    function()
      brj.set(char)
      brj.next()
    end,
  }
end
-- }}}2

-- Set impair map
set_keymap("n", { noremap = true, silent = true }, impair_map_config)
space_key_nmap.k = brackets_keymap
space_key_nmap.n = brackets_keymap

-- }}}1

-- Space Key mapping
-- Top {{{1
space_key_nmap['<space>'] = { 'FZF Command Search',         ':FFCommands<CR>'}
space_key_nmap['<tab>']   = { 'last buffer',                ':e#<CR>'}
space_key_nmap["\\"]      = { 'FZF Command History Search', ':FFHistory:<CR>'}
space_key_nmap["'"]       = { 'Toggles the neoterm',        ':above Ttoggle<CR>'}
space_key_nmap["`"]       = { 'Toggles the Float Terminal', ':FloatermToggle<CR>'}

space_key_nmap.b = { --{{{1
  name = '+buffer',

  b = {'List all buffers',                   ':FFBuffers<CR>'},
  d = {'Delete this buffer',                 ':call undoquit#SaveWindowQuitHistory()<CR>:Bclose<CR>'},
  D = {'Delete this buffer!',                ':bp<bar>sp<bar>bn<bar>bd!<CR>'},
  H = {'Startify Home',                      ':Startify<CR>'},
  h = {'Startify Home',                      ':call SaveCurrentSessions()<CR>:SClose<CR>'},
  j = {'Buffer line jump',                   ':BufferLinePick<CR>' },
  n = {'Next buffer',                        ':bnext<CR>'},
  o = {'Buffer only',                        ':Bdelete other<CR>'},
  c = {'Buffer clean unattached',            ':Bdelete hidden<CR>'},
  p = {'Previous buffer',                    ':bprev<CR>' },

  t = {'Buffer tabs',                        ':FzfLua tabs<CR>'},
  l = {'Buffer tabs list',                   ':FzfLua tabs<CR>'},

}

-- stylua: ignore
space_key_nmap.c = { --{{{1
  name = "+COC/Change",

  R = { "Help tag",     ":<C-u>CocList extensions<CR>" },
  e = { "Coc Explorer", ":CocCommand explorer<CR>" },
  c = { "Commands",     ":<C-u>CocList commands<CR>" },
  l = { "List",         ":CocList<CR>" },
  i = { "Info",         ":CocInfo<CR>" },
  E = { "extensions",   ":<C-u>CocList extensions<CR>" },
  -- s = { "symbols", ":<C-u>CocList -I symbols<CR>" },


  s = { "Change Schema",   ":FzfLua colorschemes<CR>" },
  f = { "Change FileType", ":FFFiletypes<CR>" },

}

-- stylua: ignore
space_key_nmap.d = { --{{{1
  name = "+Delete window/tab/buffer",

  -- Bclose: close buffer without close window
  h = { "Delete hidden unattached buffer", ":Bdelete hidden<CR>"},
  b = { "Delete this buffer",              ":call undoquit#SaveWindowQuitHistory()<CR>:Bclose<CR>"},
  B = { "Delete this buffer!",             ":call undoquit#SaveWindowQuitHistory()<CR>:bp<bar>sp<bar>bn<bar>bd!<CR>"},
  w = { "Delete this window",              ":call undoquit#SaveWindowQuitHistory()<CR>:close<CR>" },
  W = { "Delete this window and buffer",   ":call undoquit#SaveWindowQuitHistory()<CR>:bdelete!<CR>" },
  t = { "Delete this tab",                 ":tabclose<CR>"},
  T = { "Delete this tab and buffer",      ":Bclose<CR>:tabclose<CR>"},

}

-- stylua: ignore
space_key_nmap.e = { --{{{1
  name = "+EDIT/Explorer",
  e = { "Coc Explorer",               ":CocCommand explorer<CR>" },
  t = { "open current buffer in tab", ":tabedit %<CR>:tabprev<CR>:call undoquit#SaveWindowQuitHistory()<CR>:close<CR>:tabnext<CR>" },
}

-- stylua: ignore
space_key_nmap.f = { --{{{1
  name = "+File/Format",

  S = { "Save all files",                     ":wa<CR>!" },
  h = { "Open History files",                 ":FFHistory<CR>" },
  r = { "Open Recent files",                  ":FFHistory<CR>" },
  f = { "Open File under current directory",  ":ProjectFiles<CR>" },
  d = { "Directory (ranger)",                 ":FloatermNew --name=ranger --disposable ranger<CR>" },
  t = { "[format] Clean trailing space",      ":StripWhitespace<CR>" },
  s = { "Save current file",                  ":mkview<CR>:w<CR>" },
  o = { "Search File under cursor",           ":<C-U>execute ':MyFzfFiles' SafeFzfQuery(GetCurrentWord('n'))<CR>" },
  e = {
    name = "+Edit",

    a = { ":e! ~/.config/vim/lua/autocmd.lua<CR>"},
    c = { ":e! ~/.config/vim/lua/core.lua<CR>"},
    e = { ":mkview<CR>:e!<CR>:loadview<CR>"},
    f = { ":e! ~/.config/vim/after/300-filetypes.vim<CR>"},
    k = { ":e! ~/.config/vim/lua/keys.lua<CR>"},
    P = { ":e! ~/.config/vim/after/500-plugins-config.vim<CR>"},
    p = { ":e! ~/.config/vim/before/100-plugins.vim<CR>"},
    s = { ":e! ~/.config/vim/lua/settings.lua<CR>"},
    i = { ":e! ~/.config/vim/init.vim<CR>"},
    v = { ":e! ~/.config/vim/vimrc<CR>"},

  },
  w = {
    name = "+Write",
    q = { ":wq<CR>"},

  },
}

-- stylua: ignore
space_key_vmap.f = { --{{{1
  name = "+File/Format",
  o = {
    "FZF File under cursor", ":<C-U>execute ':MyFzfFiles' SafeFzfQuery(GetCurrentWord('v'))<CR>",
    mode = "v",
  },
}

-- stylua: ignore
space_key_nmap.g = { --{{{1
  name = "+Git/Go",

  a = { 'GitSign stage_hunk',   ':Gitsigns stage_hunk<CR>' },
  A = { 'Git add current file', ':Git add %<CR>' },
  b = { 'Git blame',            ':Git blame --date=relative<CR>' },
  c = { 'Git commit',           ':Git commit -v<CR>' },
  d = { 'Git diff',             ':Gdiffsplit<CR>' },
  D = { 'Git diff Close',       ':bwipeout! fugitive://*<CR>' },
  e = { 'Gitsign Edit mode',    ':Gitsigns toggle_linehl<CR>:Gitsigns toggle_deleted<CR>:Gitsigns toggle_numhl<CR>' },
  s = { 'Gitsign Edit mode',    ':Gitsigns toggle_linehl<CR>:Gitsigns toggle_deleted<CR>:Gitsigns toggle_numhl<CR>' },
  S = { 'Git status',           ':Git<CR>' },
  i = { 'Git line info',        ":lua require('gitsigns').blame_line({ full = true})<CR>" },
  l = { 'Git link open',        ':GBrowse<CR>' },
  p = { 'Git Gina Push',        ':Gina push<CR>' },
  y = { 'Git link yank',        ':lua require"gitlinker".get_buf_range_url("n")<CR>' },


  h = {
    name ='Git hunk',
    a = { 'GitSign stage_hunk',             ':Gitsigns stage_hunk<CR>' },
    A = { 'Git add current file',           ':Git add %<CR>' },
    u = { 'GitSign stage_hunk undo',        ':Gitsigns undo_stage_hunk<CR>' },
    R = { 'GitSign hunk restore',           ':Gitsigns reset_hunk<CR>' },
    r = { 'GitSign buffer reset',           ':Gitsigns reset_buffer_index<CR>' },
    p = { 'Git preview hunk',               ':Gitsigns preview_hunk<CR>'},

  },
  v = {'Git history view ', ':GV<CR>'},

  x = {'Search and open in browser ', '<Plug>(openbrowser-smart-search)'},
  f = {'Goto floating window',        ':call GotoFirstFloat()<CR>'},

}

-- stylua: ignore
space_key_vmap.g = { --{{{1
  name = "+Git/Go",

  a = { 'GitSign stage_hunk',    ':Gitsigns stage_hunk<CR>' },
  l = { 'Git link open',         ':GBrowse<CR>' },
  y = { 'Git link yank',         ':lua require"gitlinker".get_buf_range_url("v")<CR>' },
  h = {
    name ='Git hunk',
    a = { 'GitSign stage_hunk',      ':Gitsigns stage_hunk<CR>' },
    u = { 'GitSign stage_hunk undo', ':Gitsigns undo_stage_hunk<CR>' },
    R = { 'GitSign hunk restore',    ':Gitsigns reset_hunk<CR>' },

  },


  x = {'Search and open in browser ', '<Plug>(openbrowser-smart-search)'},
}

vim.cmd('command! -bar -bang IMaps  call fzf#vim#maps("i", <bang>0)')
vim.cmd('command! -bar -bang VMaps  call fzf#vim#maps("v", <bang>0)')

-- stylua: ignore
space_key_nmap.h = { --{{{1
  name = "+Help",

  t = {'Help tag', ':FzfLua help_tags<CR>'},
  m = {'Man Page', ':FzfLua man_pages<CR>'},

  k = {
    name = "+Keymap",

    v = {'[visual] Key Maps',               ':VMaps<CR>'},
    i = {'[insert] Key Maps',               ':IMaps<CR>'},
    m = {'[normal] Key Maps',               ':FFMaps<CR>'},
    d = {'Debug Key Maps',                  ':verbose map'},
    D = {'Debug Key Maps in new buffer',    ':execute "enew| pu=execute(\'verbos map\')"'},

    -- nnoremap <Space>hkD :execute "enew\| pu=execute('verbos map')"
  }

}

-- stylua: ignore
space_key_nmap.i = { --{{{1
  name = "+Inspect",

  f = {'Inspect file type',         ':call Show_buffer_info()<CR>'},
  p = {'Inspect installed plugins', ":enew|pu=execute('echo g:plugs')<CR>"},


}


-- stylua: ignore
space_key_nmap.j = { --{{{1
  name = "+Jump",

  i = {'Fzf Jump def',            ':FFLines (def<CR>'},
  I = {'Fzf Jump def in project', ':MyFzfAg (def[n]? <CR>'},
  -- t = {'Fzf Jump Tags',           ':FFTags<CR>'},
  t = {'Fzf BTags',               ':FFBTags<CR>'},
  -- w = {'Hop Jump Word',           ':HopWord<CR>'},
  w = {'Hop Jump Word',           ':HopWord<CR>'},
  c = {'Hop Jump Character',      ':HopChar2<CR>'},
  l = {'Hop Jump Line',           ':HopLineStart<CR>'},
  j = {'Fzf Jump jumps',          ':FzfLua jumps<CR>'},
  m = {'Fzf Jump marks',          ':FzfLua marks<CR>'},

}


-- stylua: ignore
space_key_nmap.L = { --{{{1
  name = "+LSP",

  S = {'LSP Stop',                 ':LspStop<CR>'},
  R = {'LSP Restart',              ':LspRestart<CR>'},
  G = {'LSP Start',                ':LspStart<CR>'},
  I = {'LSP Info',                 ':LspInfo<CR>'},

}

-- stylua: ignore
space_key_nmap.l = { --{{{1
  name = '+LSP',

  d = { 'lsp toggle diagnostics',       '<Cmd>lua vim.diagnostic.toggle()<CR>' },
  f = { 'lsp toggle diagnostics float', '<Cmd>lua vim.diagnostic.float_toggle()<CR>' },
  s = { 'LSP Stop',                     ':LspStop<CR>'},
  r = { 'LSP Restart',                  ':LspRestart<CR>'},
  g = { 'LSP Start',                    ':LspStart<CR>'},
  i = { 'LSP Info',                     ':LspInfo<CR>'},

}

-- stylua: ignore
space_key_nmap.o = { --{{{1
  name = "+Open",

  t = { "Open buffer in new Tab",                  ":tabedit %<CR>" },
  T = { "Open buffer in new Tab and close window", ":tabedit %<CR>:tabprev<CR>:call undoquit#SaveWindowQuitHistory()<CR>:close<CR>:tabnext<CR>" },
  l = { "Open Link",                               "<Plug>(openbrowser-smart-search)"},
  s = { "Open smart",                             "<Plug>(openbrowser-smart-search)"},
  g = {
    name = "Open Git",
    l = { "Open git link",                         ":GBrowse<CR>"},
    s = { "Search and open in Github ",            ":OpenBrowserSmartSearch -github <C-R><C-W><CR>"},
  },
  p = { "Open Plugin in Github",                   ":call OpenGithubPlugin()<CR>"},

}

-- stylua: ignore
space_key_vmap.o = { --{{{1
  name = "+Open",

  s = { 'Open search',                  '<Plug>(openbrowser-smart-search)'},
  g = {
    name = "Open Git",
    l = { "Open git link",              ":GBrowse<CR>"},
    s = { 'Search and open in Github ', 'y:OpenBrowserSmartSearch -github <C-R>0<CR>'},
  },

}

-- stylua: ignore
space_key_nmap.p = { --{{{1
  name = "+Project/Packages",

  f = {'Project files',                   ':FFFiles<CR>'},
  e = {'Project files',                   ':FzfLua files<CR>'},
  i = {'Plug Install',                    ':PlugInstall<CR>'},
  u = {'Plug Update',                     ':PlugUpdate<CR>'},
  C = {'Plug Clean',                      ':PlugClean<CR>'},
  S = {'Plug Status',                     ':PlugStatus<CR>'},
  o = {'Plugin main page open in Github', ':call OpenGithubPlugin()<CR>'},

}

-- stylua: ignore
space_key_nmap.q = { --{{{1
  name = "+Quit",

  A = {'Quit all (Force)', ':qa!CR<>'},
  a = {'Quit all',         ':qa<CR>'},
  f = {'Force Quit all',   ':qa!<CR>'},
  Q = {'Force Quit',       ':q!<CR>'},
  q = {'Quit',             ':q<CR>'},
  w = {'Quit',             ':wq<CR>'},

}


-- stylua: ignore
space_key_nmap.r = { --{{{1
  name = "+Run",

  r = {'Run Current file',            ':call RunUsingCurrentFiletype()<CR>'},
  f = {'Run FzfLua',                  ':FzfLua<CR>'},
  l = {'Run Current line in neoterm', '<Plug>(neoterm-repl-send-line)<CR>'},
  n = {'Run in neoterm',              '<Plug>(neoterm-repl-send)<CR>'},
  t = {'Run test fn',                 '<Plug>(mytestFn)'},


-- call s:map_cmd_2('nmap',     'r', 'l', 'Run Current line in neoterm', '<Plug>(neoterm-repl-send-line)<CR>')
-- call s:map_cmd_2('nmap',     'r', 'n', 'Run in neoterm',              '<Plug>(neoterm-repl-send)<CR>')
-- call s:map_cmd_2('nmap',     'r', 't', 'Run test fn',                 '<Plug>(mytestFn)')
-- call s:map_cmd_2('vmap',     'r', 't', 'Run test fn',                 '<Plug>(mytestFn)')

}

-- stylua: ignore
space_key_vmap.r = { --{{{1
  name = "+Run",
  t = {'Run test fn',                 '<Plug>(mytestFn)'},
}

-- stylua: ignore
space_key_nmap.s = { --{{{1
  name = "+Search/Source",

  B = {'FZF Search All Opened Buffers',     ":<C-U>execute ':MyFzfLuaLines '.GetCurrentWord('n')<CR>"},
  G = {'FZF <Rg> Search Current Project',   ":<C-U>execute ':MyFzfLuaGrep '.GetCurrentWord('n')<CR>"},
  P = {'FZF <Ag> Search Current Project',   ":<C-U>execute ':MyFzfAg '.GetCurrentWord('n')<CR>"},
  R = {'FZF <Rg> Search Current Project',   ":<C-U>execute ':FFRg '.GetCurrentWord('n')<CR>"},
  S = {'FZF Search Lines in Current Files', ":<C-U>execute ':MyFzfLuaBlines '.GetCurrentWord('n')<CR>"},

  b = {'FZF Search All Opened Buffers',     ":MyFzfLuaLines<CR>"},
  g = {'FZF <Rg> Search Current Project',   ":MyFzfLuaGrep<CR>"},
  p = {'FZF <Ag> Search Current Project',   ":MyFzfAg<CR>"},
  r = {'FZF <Rg> Search Current Project',   ":FFRg<CR>"},
  s = {'FZF Search Lines in Current Files', ":MyFzfLuaBlines<CR>"},
  f = {'run fag',                           ":FloatermSend FZF_TP_OPTS=\"-p 95\\%\" fag<CR>"},
  F = {'run fag',                           ":<C-U>execute ':FloatermSend FZF_TP_OPTS=\"-p 95\\%\" fag '.GetCurrentWord('n')<CR>"},

  c = {'Search highlight clean',            ':nohlsearch<CR>'},
  h = {'Search history',                    ':FzfLua search_history<CR>'},

  e = {'Source current file!',              ':so %<CR>'},
  v = {'Source vimrc',                      ':so $XDG_CONFIG_HOME/nvim/init.vim<CR>'},
  l = {'Source lua file',                   ':lua R_FOLD("plugins")<CR>'},

}

-- stylua: ignore
space_key_vmap.s = { --{{{1
  name = "+Search/Source",
  s = {'FZF Search Lines in Current Files', ":<C-U>execute ':MyFzfLuaBlines '.GetCurrentWord('v')<CR>"},
  b = {'FZF Search All Opened Buffers',     ":<C-U>execute ':MyFzfLuaLines '.GetCurrentWord('v')<CR>"},
  g = {'FZF <Rg> Search Current Project',   ":<C-U>execute ':MyFzfLuaGrep '.GetCurrentWord('v')<CR>"},
  r = {'FZF <Rg> Search Current Project',   ":<C-U>execute ':FFRg '.GetCurrentWord('v')<CR>"},
  p = {'FZF <Ag> Search Current Project',   ":<C-U>execute ':MyFzfAg '.GetCurrentWord('v')<CR>"},
  f = {'run fag',                           ":<C-U>execute ':FloatermSend FZF_TP_OPTS=\"-p 95\\%\" fag '.GetCurrentWord('v')<CR>"},
}

-- stylua: ignore
space_key_nmap.S = { --{{{1
  name = "+SESSION",

  S = { "Session Save",   ":SSave!<CR>" },
  C = { "Session Close",  ":SClose<CR>" },
  L = { "Session Load",   ":SLoad" },
  D = { "Session delete", ":SDelete" },
  N = { "Session new",    ":silent execute 'SSave! ' . GetUniqueSessionName()" },

-- " autocmd VimLeavePre *             silent execute 'SSave! ' . GetUniqueSessionName()

  R = { "Session Remove", ":FloatermSend FZF_TP_OPTS=\"-p 95\\%\" cd $XDG_DATA_HOME/nvim/session && ls | fzf_tp | xargs rm -r && popd<CR>" },

}

-- stylua: ignore
space_key_nmap.t = { --{{{1
  name = "+Toggle",

  a  = {
    name = 'auto+',
    s  = {'Toggle Strip Whitespace On Save',       ':ToggleStripWhitespaceOnSave<CR>:echo "ToggleStripWhitespaceOnSave"<CR>'},
  },
  m  = {'Color: Dark/Light Mode',                ':ToggleColorschemeMode<CR>'},
  M  = {'Color: FZF Schema',                     ':FzfLua colorschemes<CR>'},
  S  = {'Toggle Trailling whitespace indicator', ':ToggleWhitespace<CR>'},
  w  = {'Toggle 80 text width',                  ':call ToggleTextWidthWithColor()<CR>'},
  p  = {'Toggle findroot',                       ':call ToggleFindRootScope()<CR>'},
  -- h  = {'Toggle left',                           ':CocCommand explorer<CR>'},
  h  = {'Toggle left',                           ':NvimTreeFindFileToggle<CR>'},
  l  = {'Toggle right',                          ':SymbolsOutline<CR><c-w>h'},

  d  = {
    name = 'vimDiff+',
    w  = {'Toggle vimdiff whitespace',           ':call VimDiffWhiteToggle()<CR>'},
  },

  o = {
    name = 'Options+',
    v  = {'Toggle vertical indent line',           ':IndentLinesToggle<CR>'},
  },

  f = {
    name = 'Fold+',
    l = {'[loop] foldmethod',            ':call LoopFoldMethod()<CR>:set foldmethod<CR>zv'},
    m = {'change foldmethod to marker',  ':set foldmethod=marker<CR>:set foldmethod<CR>zv'},
    e = {'change foldmethod to expr',    ':set foldmethod=expr<CR>:set foldmethod<CR>zv'},
    s = {'change foldmethod to syntax',  ':set foldmethod=syntax<CR>:set foldmethod<CR>zv'},
    i = {'change foldmethod to indent',  ':set foldmethod=indent<CR>:set foldmethod<CR>zv'},
    c = {'Toggle Fold Column',           ':call ToggleFoldColumn()<CR>'},
  }

}


-- stylua: ignore
local toggle_keymap = {
  l = {'cursorline',     'setlocal', 'Toggle line cursorline'},
  c = {'cursorcolumn',   'setlocal', 'Toggle line cursorcolumn'},
  h = {'hlsearch',       'set',      'Toggle highlight matches'},
  i = {'list',           'set',      'Toggle invisible char (set list)'},
  n = {'number',         'set',      'Toggle line number '},
  r = {'relativenumber', 'set',      'Toggle relative line number'},
  w = {'wrap',           'set',      'Toggle line wrap'},
  s = {'wrapscan',       'set',      'Toggle wrapscan'},
}

local settingToggle = function(keymap)
  for k, v in pairs(keymap) do
    space_key_nmap.t.o[k] = { v[3], ":" .. v[2] .. " " .. v[1] .. "!<CR>:" .. v[2] .. " " .. v[1] .. "?<CR>" }
  end
end
settingToggle(toggle_keymap)

vim.cmd("nnoremap \\s :call LoopFoldMethod()<CR>:set foldmethod<CR>zv")

-- stylua: ignore

space_key_nmap.T = { --{{{1
  name = "+Tabs",

  c  = {'Tab close',                   ':tabclose<CR>'},
  o  = {'Tab Only',                    ':tabonly<CR>'},

}
space_key_vmap.v = { --{{{1
  v = {  "Visual", ":lua require'nvim-treesitter.incremental_selection'.node_incremental()<CR>" },
}

vim.cmd("vnoremap v :lua require'nvim-treesitter.incremental_selection'.node_incremental()<CR>")

-- stylua: ignore
space_key_nmap.w = { --{{{1
  name = "+Window",

  d = { "Window split move down",        ":aboveleft sbuffer#<CR><C-w>w" },
  l = { "Window split move right",       ":vert sbuffer#<CR><C-w>w" },

  s = { "Window split move down",        ":aboveleft sbuffer#<CR><C-w>w" },
  v = { "Window split vertical",         ":vert sbuffer#<CR><C-w>w" },
  c = { "Window close",                  ":call undoquit#SaveWindowQuitHistory()<CR>:close<CR>" },
  C = { "Window close!",                 ":call undoquit#SaveWindowQuitHistory()<CR>:bdelete!<CR>" },

  w = { "VimWiki Index Page",            ":e ~/vimwiki/index.md<CR>" },
  r = { "Window resize",                 ":WinResizerStartResize<CR>" },
  m = { "Maximum Current window",        ":ZoomToggle<CR>"},

  u = { "Undoquit Window",               ":Undoquit<CR>" },
  o = { "Window Only",                   "<C-w><C-o>"},
  q = { "Write and quit",                ":wq<CR>" },


}

-- stylua: ignore
space_key_nmap.z = { --{{{1
  name = "+Mist",

  l = {'Limelight',                  ':Limelight!!<CR>'},
  t = {'Twilight',                   ':Twilight<CR>'},

  u = {'Undo tree',                  ':UndotreeToggle<CR>'},
  z = {'Goyo',                       ':Goyo<CR>'},
  n = {'Zen mode (Similar to Goyo)', ':ZenMode<CR>'},
  m = {'Maximum Current window',     ':ZoomToggle<CR>'},
}


-- stylua: ignore
space_key_nmap.X = { --{{{1
  name = "+Help",
}
-- stylua: ignore end

-- Register key map {{{1
--

reformate_key_map(space_key_nmap)
reformate_key_map(space_key_vmap)
wk.register(space_key_nmap, { prefix = "<Space>" })
wk.register(space_key_vmap, { prefix = "<Space>", mode = "v" })

------------------------------------------------------------------------------
space_key_nmap_Plan = {
  a = {
    name = "+Applications",
    u = { "Undo tree", ":UndotreeToggle<CR>" },
  },
  B = {
    name = "Global buffers",
  },
  C = {
    name = "+Colors/Check?",
  },

  -- c = {
  --   name = "+comments/copy?"
  -- },
  d = {
    name = "+Diagnostic/directory",
    d = { "XXX" },
  },
  e = { name = "+EDIT" },
  i = { name = "+Insert ? snipper, Inspect" },
  k = { name = "Lisp/kill" },
  m = { name = "Major" },
  n = { name = "number?" },
  r = { name = "Run/Resume/Register" },
  T = { name = "UI toggle/theme" },
  u = { name = "universal " },
  v = { name = "visual, or maping v to expand" },
  x = { name = "Text" },
  z = { name = "Zoom" },
}
reformate_key_map(space_key_nmap_Plan)
-- nvim_print(space_key_nmap_Plan)
-- wk.register(space_key_nmap_test, { prefix = "<Space>" })

-- }}}1
