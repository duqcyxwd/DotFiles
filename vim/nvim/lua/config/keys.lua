require("nvim_utils")

local u = require("funcs.utility")
local brj = require("funcs.bracket_jump")
local vim_u = require('funcs.nvim_utility')
local core = require("funcs.nvim_core")

local M = {}

vim.g.mapleader = "\\"
-- vim.g.maplocalleader = ","
vim.g.maplocalleader = "-"

-- init which-key {{{1
-- stylua: ignore start
local status, wk = pcall(require, "which-key")
if not status then
  vim.notify("couldn't load which-key, skipping mappings")
  return
end

wk.setup({
  layout = {
    height = { min = 15, max = 25 }, -- min and max height of the columns
    width = { min = 20, max = 50 },  -- min and max width of the columns
    spacing = 3,                     -- spacing between columns
    align = "left",                  -- align columns left, center or right
  }
})

local space_key_nmap = {}
local space_key_vmap = {}

-- Local Utility functions {{{1
local function set_keymap(mode, opts, keymaps)
  for _, keymap in ipairs(keymaps) do
    vim.keymap.set(mode, keymap[1], keymap[2], u.merge(opts, keymap[3]))
    -- https://neovim.io/doc/user/lua-guide.html#lua-guide-mappings-set
    -- vim.keymap.set('n', '<Leader>pl1', require('plugin').action)
    -- Note that this loads the plugin at the time the mapping is defined. If you
    -- want to defer the loading to the time when the mapping is executed (as for
    -- autoload functions), wrap it in function() end:
    -- vim.keymap.set('n', '<Leader>pl2', function() require('plugin').action() end)
    --
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
-- this is file: do 'ga,'

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
  { "Q",         "@q" },

  -- vim.keymap is supported in latest neovim
  -- vim.keymap.set("n", "<Esc>", clear, { desc = "clear" })
  -- vim.keymap.set("n", "<Right>", brj.next, { desc = "next (])" })
  -- vim.keymap.set("n", "<Left>", brj.prev, { desc = "prev ([)" })
  { "<Left>",    ':lua R("funcs.bracket_jump").prev()<CR>' },
  { "<Right>",   ':lua R("funcs.bracket_jump").next()<CR>' },
  { "<Esc>",     vim_u.clear },

  -- yank to end of line
  -- { "Y", "y$" },

  -- yank/paste clipboard
  { "gy",        '"+y' },
  { "gp",        '"+p' },
  { "gP",        '"+P' },
  { "gY",        '"+y$' },

  { "gf",        vim_u.goto_first_float },

  { "j",         'gj' },
  { "k",         'gk' },

  -- { 'gs',        ':Gitsigns stage_hunk<CR>' },
  { 'gV',        '<Plug>(VM-Reselect-Last)',               { noremap = false } },


  -- -- source config
  -- { "<C-s>", ':lua R("funcs.config").source()<CR>' },

  -- Resize split
  { "<S-Up>",    ":resize +2<CR>" },
  { "<S-Down>",  ":resize -2<CR>" },
  { "<S-Left>",  ":vertical resize +2<CR>" },
  { "<S-Right>", ":vertical resize -2<CR>" },

  -- Quickfix
  -- { "<Up>",   ":copen<CR>" },
  -- { "<Down>", ":cclose<CR>" },
  -- {'<Left>', ':cprevious<CR>'},
  -- {'<Right>', ':cnext<CR>'},

  { "cg*",       "*Ncgn" },
  { "g.",        [[/\V<C-r>"<CR>cgn<C-a><Esc>]] },

  -- window
  { "gj",        "<C-W>j" },
  { "gk",        "<C-W>k" },
  { "gh",        "<C-W>h" },
  { "gl",        "<C-W>l" },

  -- Edit
  { "K",         ":call Show_documentation()<CR>" },
  { "<CR>",      "za" },
  { "gV",        "<Plug>(VM-Reselect-Last)" },
  { "<C-p>",     ":FzfLua files<CR>" },
  { "<D-p>",     ":FzfLua files<CR>" },

})


-- Leaderkey mapping --{{{1
set_keymap("n", { noremap = true, silent = true }, {
  { "<localleader>k", ":call Show_documentation()<CR>" },
  { "<localleader>j", vim_u.goto_first_float },
})

-- Visual mode mapping --{{{1
set_keymap("v", { noremap = true, silent = true }, {
  { "9",         "c()<Esc>hp" },
  { "J",         ":move '<+1<CR>gv-gv" },
  { "K",         ":move '<-2<CR>gv-gv" },
  { "<C-Space>", ":lua require'nvim-treesitter.incremental_selection'.node_incremental()<CR>" },
  { "-",         ":lua require'nvim-treesitter.incremental_selection'.node_decremental()<CR>" },
  { "v",         ":lua require'nvim-treesitter.incremental_selection'.node_incremental()<CR>" },
})

-- }}}1

-- Buffer/Tab Switch {{{1
-- Map <Space>1..8 to buffers
-- Map [1..8 to tabs
for i = 1, 8, 1 do
  space_key_nmap[tostring(i)] = { "which_key_ignore", "<Cmd>BufferLineGoToBuffer " .. tostring(i) .. "<CR>" }
  set_keymap("n", { noremap = true, silent = true }, { { "[" .. tostring(i), tostring(i) .. "gt" } })
  set_keymap("n", { noremap = true, silent = true }, { { "]" .. tostring(i), tostring(i) .. "gt" } })
end

-- Impair keys and Bracket jump {{{1
-------------------------------------------------------------------------------
-- This is similaer to emacs transit mode

local impair_map_config = { --{{{2
  -- in pair

  -- Jump list
  { "[j", "<C-o>" },
  { "]j", "<C-i>" },


  -- -- vim diff next hunk
  -- { "[c", "[c" },
  -- { "]c", "]c" },

  -- Git chagne Hunk
  { "[h", ":lua require'gitsigns'.prev_hunk({ preview = true})<CR>" },
  { "]h", ":lua require'gitsigns'.next_hunk({ preview = true})<CR>" },

  -- Place holder for e[rror] d[iagnostic]
  { "[e", "" },
  { "]e", "" },
  { "[d", "" },
  { "]d", "" },


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


local retrieveKeys = function(maps)
  -- get keys from a impar map
  local res = {}
  for _, entry in pairs(maps) do
    local c = string.gsub(entry[1], "[%[%]]", "")
    table.insert(res, c)
  end
  return res
end

local impar_keys  = u.remove_dups(u.join(default_impair_keys, retrieveKeys(impair_map_config)))
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
-- space_key_nmap.k = brackets_keymap
space_key_nmap.n = brackets_keymap

-- }}}1

-- Space Key mapping
-- TOP {{{1
space_key_nmap['<space>'] = { 'FZF Command Search',         ':FzfLua commands<CR>' }
space_key_nmap['<tab>']   = { 'last buffer',                ':e#<CR>' }
space_key_nmap["\\"]      = { 'FZF Command History Search', ':FzfLua command_history<CR>' }
space_key_nmap["'"]       = { 'Toggles the neoterm',        ':above Ttoggle<CR>' }
space_key_nmap["`"]       = { 'Toggles the Float Terminal', ':FloatermToggle<CR>' }
space_key_nmap[";"]       = { 'BufferLinePick',             ':BufferLinePick<CR>' }
space_key_nmap["v"]       = { 'Flash Treesitter',           function() require("flash").treesitter() end }

space_key_nmap.a = { --{{{1 +append
  name = '+append',
  [","] = {'Toggle Append ,',              ':lua require("chartoggle").toggle(",")<CR>'},
  [";"] = {'Toggle Append ;',              ':lua require("chartoggle").toggle(";")<CR>'},

    -- M.map('n', options.leader .. key, ':lua require("chartoggle").toggle("' .. key .. '")<CR>', { noremap = true, silent = true })
}

-- stylua: ignore
space_key_nmap.b = { --{{{1 +buffer
  name = '+buffer',

  a = { 'List all buffers',        ':Telescope scope buffers<CR>' },
  b = { 'List all buffers',        ':FzfLua buffers<CR>' },
  f = { 'Buffer line Pick',        ':BufferLinePick<CR>' },
  d = { 'Delete this buffer',      vim_u.smart_buffer_close },
  D = { 'Delete this buffer!',     ':bp<bar>sp<bar>bn<bar>bd!<CR>' },
  h = { 'Nvim Home',               ':Alpha<CR>' },
  j = { 'Buffer line jump',        ':BufferLinePick<CR>' },
  o = { 'Buffer Only',             ':Bdelete other<CR>' },
  c = { 'Buffer Clean unattached', ':Bdelete hidden<CR>' },
  n = { 'Next buffer',             ':bnext<CR>' },
  p = { 'Previous buffer',         ':bprev<CR>' },

  l = { 'Buffer tabs list',        ':FzfLua tabs<CR>' },

}

-- stylua: ignore
space_key_nmap.c = { --{{{1 +COC/Change/CD
  name = "+COC/Change",

  R = { "Help tag",                ":<C-u>CocList extensions<CR>" },
  e = { "Coc Explorer",            ":CocCommand explorer<CR>" },
  c = { "Commands",                ":<C-u>CocList commands<CR>" },
  -- l = { "List",                 ":CocList<CR>" },
  i = { "Info",                    ":CocInfo<CR>" },
  E = { "extensions",              ":<C-u>CocList extensions<CR>" },
  -- s = { "symbols",              ":<C-u>CocList -I symbols<CR>" },


  s = { "Change Schema",           ":FzfLua colorschemes<CR>" },
  f = { "Change FileType",         ":FzfLua fileTypes<CR>" },
  p = { "CD root",                 ":lua require'funcs.toggle'.set_root()<CR>" },
  d = { "CD local dir",            ":tcd %:p:h<CR>" },
  t = { "CD local dir for Tab",    ":tcd %:p:h<CR>" },
  l = { "CD local dir for Buffer", ":lcd %:p:h<CR>" },

}

-- stylua: ignore
space_key_nmap.d = { --{{{1 +Delete window/tab/buffer
  name = "+Delete window/tab/buffer",

  h = { "Delete hidden unattached buffer", ":Bdelete hidden<CR>" },
  b = { "Delete this buffer",              vim_u.smart_buffer_close },
  w = { "Delete this window",              ":close<CR>" },
  t = { "Delete this tab",                 ":tabclose<CR>" },
  T = { "Delete this tab and buffer",      ":Bclose<CR>:tabclose<CR>" },
  c = { "Diff/Check with Saved",           ":require'funcs.nvim_utility'.diff_with_saved<CR>" },

}

-- stylua: ignore
space_key_nmap.e = { --{{{1 +EDIT/Explorer
  name = "+EDIT/Explorer",
  -- e = { "Coc Explorer",               ":CocCommand explorer<CR>" },
  t = { "open current buffer in tab", ":tabedit %<CR>:tabprev<CR>:call undoquit#SaveWindowQuitHistory()<CR>:lua require('funcs.nvim_utility').smart_buffer_close()<CR>:tabnext<CR>" },
}

-- stylua: ignore
space_key_nmap.f = { --{{{1 +File/Format
  name = "+File/Format",

  S = { "Save all files",                                    ":wa<CR>!" },
  r = { "Open Recent files",                                 ":FzfLua oldfiles<CR>" },
  f = { "Open File under current directory",                 ":lua require'fzf-lua'.files({ cwd=vim.fn.expand('%:p:h') })<CR>" },
  d = { "Directory (ranger)",                                ":FloatermNew --name=ranger --disposable ranger<CR>" },
  t = { "[format] Clean trailing space",                     ":Trim<CR>" },
  s = { "Save current file",                                 ":w<CR>" },
  c = { "Check/Diff with Saved",                             ":require'funcs.nvim_utility'.diff_with_saved<CR>" },
  o = { "Search File under cursor",                          ":<C-U>execute ':MyFzfFiles' SafeFzfQuery(GetCurrentWord('n'))<CR>" },
  e = {
    name = "+Edit",

    d = { "FzfLua Open Dotfile",                             ":lua require'fzf-lua'.files({ cwd='lua' })<CR>" },
    a = { ":e! ~/.config/vim/lua/config/autocmds.lua<CR>" },
    c = { ":e! ~/.config/vim/lua/config/color.lua<CR>" },
    e = { ":mkview<CR>:e!<CR>:loadview<CR>" },
    k = { ":e! ~/.config/vim/lua/config/keys.lua<CR>" },
    l = { ":e! ~/.config/vim/lua/config/lazy.lua<CR>" },
    s = { ":e! ~/.config/vim/lua/settings.lua<CR>" },
    i = { ":e! ~/.config/vim/init.lua<CR>" },

  },
  w = {
    name = "+Write",
    q = { ":wq<CR>" },

  },
}

-- stylua: ignore
space_key_vmap.f = { --{{{1 +File/Format
  name = "+File/Format",
  o = {
    "FZF File under cursor",
    ":<C-U>execute ':MyFzfFiles' SafeFzfQuery(GetCurrentWord('v'))<CR>",
    mode = "v",
  },
}

-- stylua: ignore
space_key_nmap.g = { --{{{1 +Git/Go
  name = "+Git/Go",

  a = { 'GitSign stage_hunk',                ':Gitsigns stage_hunk<CR>' },
  A = { 'Git add current file',              ':Git add %<CR>' },
  b = { 'Git blame',                         ':Git blame --date=relative<CR>' },
  c = { 'Git commit',                        ':Git commit -v<CR>' },
  d = { 'Git diff',                          ':Gdiffsplit<CR>' },
  D = { 'Git diff Close',                    ':bwipeout! fugitive://*<CR>' },
  e = { 'Gitsign Edit mode',                 ':Gitsigns toggle_linehl<CR>:Gitsigns toggle_deleted<CR>:Gitsigns toggle_numhl<CR>' },
  s = { 'Git status',                        ':FzfLua git_status<CR>' },
  S = { 'Git status',                        ':git<cr>' },

  i = { 'Git line info',                     ":lua require('gitsigns').blame_line({ full = true})<CR>" },
  H = { 'Git home page',                     ':lua require"gitlinker".get_repo_url({action_callback = require"gitlinker.actions".open_in_browser})<cr>' },
  l = { 'Git link open',                     ':lua require"gitlinker".get_buf_range_url("n", {action_callback = require"gitlinker.actions".open_in_browser})<cr>' },
  p = { 'Git Gina Push',                     ':Gina push<CR>' },
  y = { 'Git link yank',                     ':lua require"gitlinker".get_buf_range_url("n")<CR>' },


  h = {
    name = 'Git hunk',
    a = { 'GitSign stage_hunk',              ':Gitsigns stage_hunk<CR>' },
    A = { 'Git add current file',            ':Git add %<CR>' },
    u = { 'GitSign stage_hunk undo',         ':Gitsigns undo_stage_hunk<CR>' },
    R = { 'GitSign hunk restore',            ':Gitsigns reset_hunk<CR>' },
    r = { 'GitSign buffer reset',            ':Gitsigns reset_buffer_index<CR>' },
    p = { 'Git preview hunk',                ':Gitsigns preview_hunk<CR>' },

  },
  v = { 'Git history view ',                 ':GV<CR>' },

  x = { 'Goto: Search and open in browser ', '<Plug>(openbrowser-smart-search)' },
  f = { 'Goto: floating window',             vim_u.goto_first_float },

}

-- stylua: ignore
space_key_vmap.g = { --{{{1 +Git/Go
  name = "+Git/Go",

  a = { 'GitSign stage_hunk',                ':Gitsigns stage_hunk<CR>' },
  -- l = { 'Git link open',                  ':GBrowse<CR>' },
  l = { 'Git link open',                     ':lua require"gitlinker".get_buf_range_url("v", {action_callback = require"gitlinker.actions".open_in_browser})<cr>' },
  y = { 'Git link yank',                     ':lua require"gitlinker".get_buf_range_url("v")<CR>' },
  h = {
    name = 'Git hunk',
    a = { 'GitSign stage_hunk',              ':Gitsigns stage_hunk<CR>' },
    u = { 'GitSign stage_hunk undo',         ':Gitsigns undo_stage_hunk<CR>' },
    R = { 'GitSign hunk restore',            ':Gitsigns reset_hunk<CR>' },
  },
  x = { 'Goto: Search and open in browser ', '<Plug>(openbrowser-smart-search)' },
}

vim.cmd('command! -bar -bang IMaps  call fzf#vim#maps("i", <bang>0)')
vim.cmd('command! -bar -bang VMaps  call fzf#vim#maps("v", <bang>0)')

-- stylua: ignore
space_key_nmap.h = { --{{{1 +Help
  name = "+Help",

  t = { 'Help tag',                       ':FzfLua help_tags<CR>' },
  m = { 'Man Page',                       ':FzfLua man_pages<CR>' },

  k = {
    name = "+Keymap",

    -- v = { '[visual] Key Maps',            ':VMaps<CR>' },
    -- i = { '[insert] Key Maps',            ':IMaps<CR>' },
    a = { '[all] Key Maps',               ":FzfLua keymaps<CR>" },
    b = { '[Buffer] Key Maps',            ":FzfLua keymaps<CR>" },
    i = { '[insert] Key Maps',            ":lua require'fzf-lua'.keymaps({modes = {'i'}})<CR>" },
    v = { '[visual] Key Maps',            ":lua require'fzf-lua'.keymaps({modes = {'v'}})<CR>" },
    m = { '[normal] Key Maps',            ":lua require'fzf-lua'.keymaps({modes = {'n'}})<CR>" },
    -- m = { '[normal] Key Maps',            ':FzfLua keymaps<CR>' },
    d = { 'Debug Key Maps',               ':verbose map' },
    D = { 'Debug Key Maps in new buffer', ':execute "enew| pu=execute(\'verbos map\')"' },

    -- nnoremap <Space>hkD :execute "enew\| pu=execute('verbos map')"
  }

}

-- stylua: ignore
space_key_nmap.i = { --{{{1 +Inspect
  name = "+Inspect",

  f = { 'Inspect file type',         ":lua require('funcs.nvim_utility').show_buffer_info()<CR>" },
  p = { 'Inspect installed plugins', ":enew|pu=execute('echo g:plugs')<CR>" },
  r = { 'Fzf inspect registers',     ":FzfLua registers<CR>" },
  a = { 'Fzf inspect autocmds',      ":FzfLua autocmd<CR>" },

}


-- stylua: ignore
space_key_nmap.j = { --{{{1 +Jump
  name = "+Jump",

  i = { 'Fzf Jump def',            ':FFLines (def<CR>' },
  I = { 'Fzf Jump def in project', ':MyFzfAg (def[n]? <CR>' },
  t = { 'Fzf BTags',               ':FFBTags<CR>' },
  w = { 'Hop Jump Word',           ':HopChar2<CR>' },
  c = { 'Fzf Jump Changes',        ':FzfLua changes<CR>' },
  l = { 'Hop Jump Line',           ':HopVerticalMW<CR>' },
  j = { 'Fzf Jump jumps',          ':FzfLua jumps<CR>' },
  m = { 'Fzf Jump marks',          ':FzfLua marks<CR>' },

}


-- stylua: ignore
space_key_nmap.L = { --{{{1 +LSP
  name = "+LSP",

  S = { 'LSP Stop',    ':LspStop<CR>' },
  R = { 'LSP Restart', ':LspRestart<CR>' },
  G = { 'LSP Start',   ':LspStart<CR>' },
  I = { 'LSP Info',    ':LspInfo<CR>' },

}

-- stylua: ignore
space_key_nmap.l = { --{{{1 +LSP
  name = "+LSP",

  d = { 'lsp toggle diagnostics',       '<Cmd>lua vim.diagnostic.toggle()<CR>' },
  f = { 'lsp toggle diagnostics float', '<Cmd>lua vim.diagnostic.float_toggle()<CR>' },
  s = { 'LSP Stop',                     ':LspStop<CR>' },
  r = { 'LSP Restart',                  ':LspRestart<CR>' },
  g = { 'LSP Start',                    ':LspStart<CR>' },
  i = { 'LSP Info',                     ':LspInfo<CR>' },

}

-- stylua: ignore
space_key_nmap.m = { --{{{1 +Move
  name = '+Move',
  t = { "Move current buffer to tab", ":ScopeMoveBuf<CR>" },
  w = { "Move current window to tab", "<c-w>T" },
}

-- stylua: ignore
space_key_nmap.o = { --{{{1 +Open
  name = "+Open",

  T = { "Open buffer in new Tab and close window", ":tabedit %<CR>:tabprev<CR>:call undoquit#SaveWindowQuitHistory()<CR>:close<CR>:tabnext<CR>" },
  l = { "Open Link",                               "<Plug>(openbrowser-smart-search)" },
  o = { "Open Open",                               "<Plug>(openbrowser-smart-search)" },
  p = { "Open Plugin in Github",                   ":call OpenGithubPlugin()<CR>" },
  s = { "Open smart",                              "<Plug>(openbrowser-smart-search)" },
  t = { "Open buffer in new Tab",                  ":tabedit %<CR>" },

  g = {
    name = "Open Git",
    l = { "Open git link",                         ":GBrowse<CR>" },
    s = { "Search and open in Github ",            ":OpenBrowserSmartSearch -github <C-R><C-W><CR>" },
  },

}

-- stylua: ignore
space_key_vmap.o = { --{{{1 +Open
  name = "+Open",

  s = { 'Open search',                  '<Plug>(openbrowser-smart-search)' },
  o = { 'Open Open',                    '<Plug>(openbrowser-smart-search)' },
  g = {
    name = "Open Git",
    l = { "Open git link",              ":GBrowse<CR>" },
    s = { 'Search and open in Github ', 'y:OpenBrowserSmartSearch -github <C-R>0<CR>' },
  },

}

-- stylua: ignore
space_key_nmap.p = { --{{{1 +Project/Packages
  name = "+Project/Packages",


  f = { 'Project files',                   ':FzfLua files<CR>' },
  e = { 'Project files',                   ':FzfLua files<CR>' },
  l = { 'Project List',                    ':SearchSession<CR>' },

  a = { 'Plug all',                        require"funcs.nvim_utility".fzf_get_plugin },
  i = { 'Plug Install',                    ':Lazy install<CR>' },
  u = { 'Plug Update',                     ':Lazy update<CR>' },
  C = { 'Plug Clean',                      ':Lazy clean<CR>' },
  S = { 'Plug Status',                     ':Lazy check<CR>' },
  p = { 'Plug Profile',                    ':Lazy profile<CR>' },
  h = { 'Plug home',                       ':Lazy home<CR>' },

  o = { 'Plugin main page open in Github', vim_u.open_github_plugin },

}

-- stylua: ignore
space_key_nmap.q = { --{{{1 +Quit
  name = "+Quit",

  A = { 'Quit all (Force)',    ':qa!CR<>' },
  Q = { 'Force Quit',          ':q!<CR>' },
  a = { 'Quit all',            ':qa<CR>' },
  c = { 'Quit Current buffer', ':q<CR>' },
  f = { 'Force Quit all',      ':qa!<CR>' },
  w = { 'Quit',                ':wq<CR>' },
  q = {
    name = "+Contineau quit",
    q = { 'Quit all',          ':qa!<CR>' },
  },

}


-- stylua: ignore
space_key_nmap.r = { --{{{1 +Run
  name = "+Run",

  r = { 'Run Current file',            ':call RunUsingCurrentFiletype()<CR>' },
  f = { 'Run FzfLua',                  ':FzfLua<CR>' },
  l = { 'Run Current line in neoterm', '<Plug>(neoterm-repl-send-line)<CR>' },
  n = { 'Run in neoterm',              '<Plug>(neoterm-repl-send)<CR>' },
  t = { 'Run test fn',                 '<Plug>(mytestFn)' },


  -- call s:map_cmd_2('nmap',          'r', 'l', 'Run Current line in neoterm', '<Plug>(neoterm-repl-send-line)<CR>')
  -- call s:map_cmd_2('nmap',          'r', 'n', 'Run in neoterm',              '<Plug>(neoterm-repl-send)<CR>')
  -- call s:map_cmd_2('nmap',          'r', 't', 'Run test fn',                 '<Plug>(mytestFn)')
  -- call s:map_cmd_2('vmap',          'r', 't', 'Run test fn',                 '<Plug>(mytestFn)')

}

-- stylua: ignore
space_key_vmap.r = { --{{{1 +Run
  name = "+Run",
  t = { 'Run test fn', '<Plug>(mytestFn)' },
}

-- stylua: ignore
space_key_nmap.s = { --{{{1 +Search/Source
  name = "+Search/Source",

  a = { 'FZF Lua buildin',                          ":FzfLua builtin<CR>" },
  r = { 'FZF Lua resume',                           ":FzfLua resume<CR>" },

  b = { 'FZF Open Buffer Lines',                    ":FzfLua lines<CR>" },
  B = { 'FZF Open Buffer Lines With CWord',         ":lua require('fzf-lua').lines({ search = vim.fn.expand('<cword>'), current_buffer_only = false })<CR>" },

  p = { 'FZF Live Grep Current Project',            ":FzfLua live_grep<CR>" },
  P = { 'FZF Live Grep Current Project With CWord', ":FzfLua grep_cword<CR>" },

  s = { 'FZF Current Buffer Lines',                 ":FzfLua grep_curbuf<CR>" },
  S = { 'FZF Current Buffer Lines With CWord',      ":lua require('fzf-lua').lines({ search = vim.fn.expand('<cword>'), })<CR>" },


  f = { 'run fag',                                  ":FloatermSend FZF_TP_OPTS=\"-p 95\\%\" fag<CR>" },
  F = { 'run fag',                                  ":<C-U>execute ':FloatermSend FZF_TP_OPTS=\"-p 95\\%\" fag '.GetCurrentWord('n')<CR>" },

  c = { 'Search highlight clean',                   ':nohlsearch<CR>' },
  h = { 'Search history',                           ':FzfLua search_history<CR>' },

  e = { 'Source current file!',                     ':so %<CR>' },
  v = { 'Source vimrc',                             ':so $XDG_CONFIG_HOME/nvim/init.vim<CR>' },
  -- l = { 'Source lua file',                          ':luafile %<CR>' },

  l = { 'Source lua code',                          ":lua loadstring( require'funcs.nvim_utility'.get_current_line())()<CR>" },
  i = { 'Inspec lua code result',                   ":lua loadstring('nvim_print(' .. require'funcs.nvim_utility'.get_current_line() .. ')')()<CR>" },

}

-- stylua: ignore
space_key_vmap.s = { --{{{1 +Search/Source
  name = "+Search/Source",

  s = { 'FZF Current Buffer Lines',      "y:lua require('fzf-lua').live_grep({ search = vim.fn.getreg('+'), })<CR>" },
  b = { 'FZF Open Buffer Lines',         "y:lua require('fzf-lua').lines({ search = vim.fn.getreg('+'), current_buffer_only = false })<CR>" },
  p = { 'FZF Live Grep Current Project', "y:lua require('fzf-lua').live_grep({ search = vim.fn.getreg('+'), current_buffer_only = false })<CR>" },
  f = { 'run fag',                       ":<C-U>execute ':FloatermSend FZF_TP_OPTS=\"-p 95\\%\" fag '.GetCurrentWord('v')<CR>" },

  l = { 'Source selected lua code',      ":luado loadstring(line)()<CR>" },
  i = { 'Inpsect selected lua code',     ":luado loadstring('P(' .. line .. ')')()<CR>" },

}


-- stylua: ignore
space_key_nmap.S = { --{{{1 +SESSION
  name = "+SESSION",

  S = { "Session Save",    ":SessionSave<CR>:echom 'Session Save'<CR>" },
  R = { "Sjssion Restore", ":SessionRestore<CR>:echom 'Session Restore'<CR>" },
  L = { "Session List",    ":SearchSession<CR>:echom 'Session List'<CR><CR>" },
  D = { "Session Delete",  ":SessionDelete<CR>:echom 'Session Delete'<CR>" },

  C = { "Session Clean",   ":FloatermSend FZF_TP_OPTS=\"-p 95\\%\" cd $XDG_STATE_HOME/nvim/sessions && ls | fzf_tp | xargs rm -r && popd<CR>" },

}

-- stylua: ignore
space_key_nmap.t = { --{{{1 +Toggle
  name = "+Toggle",


  -- UI
  h = { 'Toggle left',                             '<CMD>NvimTreeFindFileToggle<CR>' },
  l = { 'Toggle right',                            ':SymbolsOutline<CR><c-w>h' },
  c = { 'Toggle ChatGPT',                          ':ChatGPT<CR>' },

  a = {
    name = 'auto+',
    s    = { 'Toggle Strip Whitespace On Save',    ':TrimToggle<CR>' },
  },
  m = { 'Color: Dark/Light Mode',                  ":lua require'config.color'.toggle()<CR>" },
  M = { 'Color: FZF Schema',                       ':FzfLua colorschemes<CR>' },


  p = {
    name = 'project+',
    -- Rooter is not really useful and confuzing. Use <SPC>cd or <SPC>cp instead, <SPC>toa can autodir to current folder
    -- r = { 'Toggle findroot',                       ":lua require'funcs.toggle'.toggle_find_root()<CR>" },
    s = { 'Toggle root scope with .vimroot',       ":lua require'funcs.toggle'.toggle_set_root_scope()<CR>" },
  },

  i = {
    name = 'indicator+',
    o = { 'Toggle 80 text width',                  ":lua require'funcs.toggle'.highlight_over_80()<CR>" },
    s = { 'Toggle Trailling whitespace indicator', ':ToggleWhitespace<CR>' },
  },

  g = {
    name = 'Git+',
    s = { 'Gitsign Edit mode',                     ':Gitsigns toggle_linehl<CR>:Gitsigns toggle_deleted<CR>:Gitsigns toggle_numhl<CR>' },
  },


  d = {
    name = 'vimDiff+',
    w = { 'Toggle vimdiff whitespace',             ":lua require'funcs.toggle'.vim_diff_whitespace()<CR>" },
  },

  o = {
    name = 'Options+',
    -- v = { 'Toggle vertical indent line',           ':IndentBlanklineToggle<CR>' },
    v = { 'Toggle vertical indent line',           ':IndentLinesToggle<CR>' },
    r = { 'Toggle relative line number',           ":lua require'funcs.toggle'.toggle_relative_num()<CR>"},
  },

  f = {
    name = 'Fold+',
    l = { '[loop] foldmethod',                     ":lua require'funcs.toggle'.loop_fold_method()<CR>" },
    m = { 'change foldmethod to marker',           ':set foldmethod=marker<CR>:set foldmethod<CR>zv' },
    e = { 'change foldmethod to expr',             ':set foldmethod=expr<CR>:set foldmethod<CR>zv' },
    s = { 'change foldmethod to syntax',           ':set foldmethod=syntax<CR>:set foldmethod<CR>zv' },
    i = { 'change foldmethod to indent',           ':set foldmethod=indent<CR>:set foldmethod<CR>zv' },
  },
  s = { 'Toggle Flash Search',                     function() require("flash").toggle() end },
  -- w = { 'Toggle Word',                             function() return core.repeatableCall(require('nvim-toggler').toggle) end, expr = true },
  w = { 'Toggle Word',                             core.dotCall(require('nvim-toggler').toggle) , expr = true },

}


-- stylua: ignore
local toggle_keymap = {
  l = { 'cursorline',     'setlocal', 'Toggle line cursorline' },
  c = { 'cursorcolumn',   'setlocal', 'Toggle line cursorcolumn' },
  h = { 'hlsearch',       'set',      'Toggle highlight matches' },
  i = { 'list',           'set',      'Toggle invisible char (set list)' },
  n = { 'number',         'set',      'Toggle line number ' },
  w = { 'wrap',           'set',      'Toggle line wrap' },
  s = { 'wrapscan',       'set',      'Toggle wrapscan' },
  a = { 'autochdir',      'set',      'Toggle autochdir' },
}

local settingToggle = function(keymap)
  for k, v in pairs(keymap) do
    space_key_nmap.t.o[k] = { v[3], ":" .. v[2] .. " " .. v[1] .. "!<CR>:" .. v[2] .. " " .. v[1] .. "?<CR>" }
  end
end
settingToggle(toggle_keymap)


-- stylua: ignore
space_key_vmap.t = { --{{{1 +Toggle
  name = "+Toggle",
  w = { 'Toggle Word',                             core.dotCall(require('nvim-toggler').toggle) , expr = true },
}

-- stylua: ignore

space_key_nmap.T = { --{{{1 +Tabs
  name = "+Tabs",

  c = { 'Tab close', ':tabclose<CR>' },
  o = { 'Tab Only',  ':tabonly<CR>' },
  n = { 'Tab new',   ':tabnew %<CR>' },

}
space_key_nmap.u = { --{{{1 +UI
  name = "+UI",
  d = { "Dismiss all Notifications", ":lua require('notify').dismiss({ silent = true, pending = true })<CR>" },
  n = { "Viewing History",           ":lua require('telescope').extensions.notify.notify()<CR>" },
  u = { "Undo tree",                 ":UndotreeToggle<CR>" },
}

-- stylua: ignore
space_key_nmap.w = { --{{{1 +Window
  name = "+Window",

  d = { "Window split move down",  ":aboveleft sbuffer#<CR><C-w>w" },
  l = { "Window split move right", ":vert sbuffer#<CR><C-w>w" },

  s = { "Window split move down",  ":aboveleft sbuffer#<CR><C-w>w" },
  v = { "Window split vertical",   ":vert sbuffer#<CR><C-w>w" },
  c = { "Window close",            ":call undoquit#SaveWindowQuitHistory()<CR>:close<CR>" },
  C = { "Window close!",           ":call undoquit#SaveWindowQuitHistory()<CR>:bdelete!<CR>" },
  g = { "Window gone",             ":call undoquit#SaveWindowQuitHistory()<CR>:close<CR>" },

  r = { "Window resize",           ":WinResizerStartResize<CR>" },
  m = { "Maximum Current window",  ":ZoomToggle<CR>" },

  u = { "Undoquit Window",         ":Undoquit<CR>" },
  o = { "Window Only",             "<C-w><C-o>" },
  q = { "Write and quit",          ":wq<CR>" },

  t = { "Move current window to tab", "<c-w>T" },

}

-- stylua: ignore
space_key_nmap.z = { --{{{1 +Mist/Focus mode
  name = "+Mist/Focus mode",

  g = { 'Goyo',                       ':Goyo<CR>' },
  z = { 'Zen mode (Similar to Goyo)', ':ZenMode<CR>' },

  l = {
    name = "light",
    l = { 'Limelight',                ':Limelight!!<CR>' },
    t = { 'Twilight',                 ':Twilight<CR>' },
  },

  u = { 'Undo tree',                  ':UndotreeToggle<CR>' },
  m = { 'Maximum Current window',     ':ZoomToggle<CR>' },
}


-- stylua: ignore
space_key_nmap.X = { --{{{1 +XXX
  name = "+XXX",
}
-- stylua: ignore end

space_key_nmap.y = { --{{{1 +Yank/Copy
  name = "+Yank",
  p = { 'Yank file path',                       ':!cpath %<CR>' },
}
-- stylua: ignore end

-- Register key map {{{1
--

-- Reformat keys
reformate_key_map(space_key_nmap)
reformate_key_map(space_key_vmap)
-- Register reformated keys
wk.register(space_key_nmap, { prefix = "<Space>" })
wk.register(space_key_vmap, { prefix = "<Space>", mode = "v" })

-- }}}1

-- WIP
-- local chatgpt = R("chatgpt")
-- wk.register({
--   p = {
--     name = "ChatGPT",
--     e = {
--       function()
--         chatgpt.edit_with_instructions()
--       end,
--       "Edit with instructions",
--     },
--   },
-- }, {
--   prefix = "<Space>",
--   mode = "v",
-- })

M.space_key_nmap = space_key_nmap
M.space_key_vmap = space_key_vmap

return M;
