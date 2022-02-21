require("nvim_utils")
local brj = require("funcs.bracket_jump")

-- Utility functions {{{1
local function set_keymap(mode, opts, keymaps)
  for _, keymap in ipairs(keymaps) do
    vim.api.nvim_set_keymap(mode, keymap[1], keymap[2], opts)
  end
end

local function quick_swap(data)
  if type(data) == "table" and type(data[1]) == "string" then
    local temp = data[1]
    data[1] = data[2]
    data[2] = temp
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

local function clear()
  vim.cmd("nohlsearch")
  vim.lsp.buf.clear_references()
  IfHas("notify", function(notify)
    notify.dismiss()
  end)
end

-- normal mapping {{{1


vim.keymap.set("n", "<Esc>", clear, { desc = "clear" })
vim.keymap.set("n", "<Left>", brj.prev, { desc = "prev ([)" })
vim.keymap.set("n", "<Right>", brj.next, { desc = "next (])" })


-- Others {{{2

vim.cmd("vnoremap <silent> <C-Space> :lua require'nvim-treesitter.incremental_selection'.node_incremental()<CR>")
vim.cmd("vnoremap <silent> -         :lua require'nvim-treesitter.incremental_selection'.node_decremental()<CR>")

vim.cmd("vnoremap J :move '<+1<CR>gv-gv")
vim.cmd("vnoremap K :move '<-2<CR>gv-gv")

vim.cmd("nnoremap <silent> K :call Show_documentation()<CR>")
vim.cmd("nnoremap <silent> <localleader>K :call Show_documentation()<CR>")
vim.cmd("nnoremap <CR> za")

-- Bash like keys for the command line
vim.cmd("cnoremap <C-A> <Home>")
vim.cmd("cnoremap <C-E> <End>")
vim.cmd("cnoremap <C-K> <C-U>")
vim.cmd("cnoremap <C-P> <Up>")
vim.cmd("cnoremap <C-N> <Down>")
vim.cmd("nnoremap <Space>fee :mkview<CR>:e!<CR>:loadview<CR>")
vim.cmd("nnoremap <Space>fec :Defx ~/.config/vim/custom/<CR>")
vim.cmd("nnoremap <Space>fep :e!   ~/.config/vim/custom/100-plugins.vim<CR>")
vim.cmd("nnoremap <Space>fef :e!   ~/.config/vim/custom/300-filetypes.vim<CR>")
vim.cmd("nnoremap <Space>feP :e!   ~/.config/vim/custom/500-plugins-config.vim<CR>")
vim.cmd("nnoremap <Space>fem :e!   ~/.config/vim/custom/400-mappings.vim<CR>")
vim.cmd("nnoremap <Space>fer :e!   ~/.config/vim/custom/999-playground.vim<CR>")
vim.cmd("nnoremap <Space>fev :e!   ~/.config/vim/vimrc<CR>")
vim.cmd("nnoremap <Space>fet :e!   ~/vimwiki/TODO.md<CR>")


-- Start interactive EasyAlign in visual mode (e.g. vipga)
vim.cmd("xmap ga <Plug>(EasyAlign)")
-- Start interactive EasyAlign for a motion/text object (e.g. gaip)
vim.cmd("nmap ga <Plug>(EasyAlign)")

vim.cmd("nmap gV <Plug>(VM-Reselect-Last)")

-- " Fix paste
-- " p will not overwrite register
-- " https://vim.fandom.com/wiki/Replace_a_word_with_yanked_text
-- " xnoremap p "_dP
vim.cmd('xnoremap <silent> p p:let @+=@0<CR>:let @"=@0<CR>')

vim.cmd("nnoremap j gj")
vim.cmd("nnoremap k gk")

vim.cmd("vnoremap 9 c()<Esc>hp")

-- }}}

set_keymap("n", { noremap = true, silent = true }, {
  -- execute q macro
  { "Q", "@q" },

  -- yank to end of line
  { "Y", "y$" },

  -- yank/paste clipboard
  { "gy", '"+y' },
  { "gp", '"+p' },
  { "gP", '"+P' },
  { "gY", '"+y$' },

  -- source config
  { "<C-s>", ':lua R("funcs.config").source()<CR>' },

  -- Jump list
  { "[j", "<C-o>" },
  { "]j", "<C-i>" },

  -- Page down/up
  { "[d", "<PageUp>" },
  { "]d", "<PageDown>" },

  -- Smart way to move between tabs
  { "<A-h>", "gT" },
  { "<A-l>", "gt" },

  -- Resize split
  { "<S-Up>", ":resize +2<CR>" },
  { "<S-Down>", ":resize -2<CR>" },
  { "<S-Left>", ":vertical resize +2<CR>" },
  { "<S-Right>", ":vertical resize -2<CR>" },

  -- Quickfix
  { "<Up>", ":copen<CR>" },
  { "<Down>", ":cclose<CR>" },
  -- {'<Left>', ':cprevious<CR>'},
  -- {'<Right>', ':cnext<CR>'},
  { "[q", ":cprevious<CR>" },
  { "]g", ":cnext<CR>" },

  -- Navigate buffers
  { "]b", "<Cmd>bnext<CR>" },
  { "[b", "<Cmd>bprev<CR>" },
  { "]n", "<Cmd>next<CR>" },
  { "[n", "<Cmd>prev<CR>" },

  -- jump diagnostic
  { "[g", "<Cmd>lua vim.diagnostic.goto_prev({float = true})<CR>" },
  { "]g", "<Cmd>lua vim.diagnostic.goto_next({float = true})<CR>" },

  -- fix spelling with first suggestion
  { "gz", "z=1<CR><CR>" },

  { "cg*", "*Ncgn" },
  { "g.", [[/\V<C-r>"<CR>cgn<C-a><Esc>]] },

  -- window
  {"gj", "<C-W>j"},
  {"gk", "<C-W>k"},
  {"gh", "<C-W>h"},
  {"gl", "<C-W>l"},

  -- Edit

  {"<Space>fec", ":Defx ~/.config/vim/custom/<CR>"},
  {"<Space>fep", ":e!   ~/.config/vim/custom/100-plugins.vim<CR>"},
  {"<Space>fef", ":e!   ~/.config/vim/custom/300-filetypes.vim<CR>"},
  {"<Space>fek", ":e!   ~/.config/vim/lua/keys.lua<CR>"},
  {"<Space>feP", ":e!   ~/.config/vim/custom/500-plugins-config.vim<CR>"},
  {"<Space>fer", ":e!   ~/.config/vim/custom/999-playground.vim<CR>"},
  {"<Space>fev", ":e!   ~/.config/vim/vimrc<CR>"},

})

-- leader mapping {{{1
vim.g.mapleader = "\\"
vim.g.maplocalleader = ","

local status, wk = pcall(require, "which-key")
if not status then
  vim.notify("couldn't load which-key, skipping mappings")
  return
end

-- stylua: ignore
local space_key_map = {
  ['<space>'] = { 'FZF Command Search',          ':FFCommands<CR>'},
  ['<tab>']   = { 'last buffer',                 ':e#<CR>'},
  ["\\"]      = { 'FZF Command History Search',  ':FFHistory:<CR>'},
  ["'"]       = { 'Toggles the neoterm',         ':above Ttoggle<CR>'},
}
local space_key_vmap = {}

for i = 1, 10, 1 do
  space_key_map[tostring(i)] = { "which_key_ignore", "<Plug>AirlineSelectTab" .. tostring(i) .. "<CR>" }
end

-- Bracket jump {{{1
local brackets = {}
for _, char in ipairs({ "q", "g", "s", "n", "b", "j" }) do
  brackets[char] = {
    string.format("Set bracket jump (%s)", char),
    function()
      brj.set(char)
    end,
  }
end
brackets.name = "+bracket jumps"
space_key_map.o = brackets


-- stylua: ignore
space_key_map.b = { --{{{1
  name = '+buffer',

  D = {'Force delete this buffer', ':bp<bar>sp<bar>bn<bar>bd!<CR>'},
  b = {'List all buffers',         ':FFBuffers<CR>'},
  d = {'Delete this buffer',       ':call undoquit#SaveWindowQuitHistory()<CR>:Bclose<CR>'},
  h = {'Startify Home',            ':Startify<CR>'},
  n = {'Next buffer',              ':bnext<CR>'},
  o = {'Close other buffers',      ':BufOnly<CR>'},
  p = {'Previous buffer',          ':bprev<CR>' },

}

space_key_map.c = { --{{{1
  name = "+COC",

  R = {'Help tag',     ':<C-u>CocList extensions<CR>'},
  e = {'Coc Explorer', ':CocCommand explorer<CR>'},
  c = {"Commands",     ":<C-u>CocList commands<cr>"},
  l = {"List",         ":CocList<cr>"},
  i = {"Info",         ":CocInfo<cr>"},
  E = {"extensions",   ":<C-u>CocList extensions<cr>"},
  s = {"symbols",      ":<C-u>CocList -I symbols<cr>"},

}

space_key_map.e = { --{{{1
  name = "+EDIT",
}

-- stylua: ignore
space_key_map.f = { --{{{1
  name = "+File/Format",

  S = { "Save all files",                ":wa<CR>!" },
  h = { "Open History files",            ":FFHistory<CR>" },
  r = { "Open Recent files",             ":FFHistory<CR>" },
  t = { "[format] Clean trailing space", ":StripWhitespace<CR>" },
  s = { "Save current file",             ":mkview<CR>:w<CR>" },
  o = { "Search File under cursor",      ":<C-U>execute ':MyFzfFiles' SafeFzfQuery(GetCurrentWord('n'))<CR>" },

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
space_key_map.g = { --{{{1
  name = "+Git/Go",

  a = {'Git action',        ':FzfPreviewGitActions<CR>'},
  b = {'Git blame',         ':Git blame<CR>'},
  c = {'Git commit',        ':Git commit -v<CR>'},
  d = {'Git diff',          ':Gdiffsplit<CR>'},
  m = {'Git Magit',         ':MagitOnly<CR>'},
  s = {'Git status',        ':FzfPreviewGitStatus<CR>'},
  p = {'Git push',          ':Gina push<CR>'},
  l = {'Git link open',     ':GBrowse<CR>'},
  h = {'Git history view',  ':GV<CR>'},
  v = {'Git history view ', ':GV<CR>'},


  o = {'Search and open in browser ', '<Plug>(openbrowser-smart-search)'},
  x = {'Search and open in browser ', '<Plug>(openbrowser-smart-search)'},
  S = {'Search and open in Github ',  ':OpenBrowserSmartSearch -github <C-R><C-W>'},

}

-- stylua: ignore
space_key_vmap.g = { --{{{1
  name = "+Git/Go",

  o = {'Search and open in browser ', '<Plug>(openbrowser-smart-search)'},
  x = {'Search and open in browser ', '<Plug>(openbrowser-smart-search)'},
  S = {'Search and open in Github ',  'y:OpenBrowserSmartSearch -github <C-R>0'},

}

vim.cmd('command! -bar -bang IMaps  call fzf#vim#maps("i", <bang>0)')
vim.cmd('command! -bar -bang VMaps  call fzf#vim#maps("v", <bang>0)')

-- stylua: ignore
space_key_map.h = { --{{{1
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
space_key_map.j = { --{{{1
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
space_key_map.L = { --{{{1
  name = "+LSP",

  S = {'LSP Stop',                 ':LspStop'},
  R = {'LSP Restart',              ':LspRestart'},
  G = {'LSP Start',                ':LspStart'},
  I = {'LSP Info',                 ':LspInfo'},
  d = {'toggle diagnostics',       '<Cmd>lua vim.diagnostic.toggle()'},
  f = {'toggle diagnostics float', '<Cmd>lua vim.diagnostic.float_toggle()'},

}

-- stylua: ignore
space_key_map.p = { --{{{1
  name = "+Project/Packages",

  f = {'Project files',                   ':FFFiles<CR>'},
  i = {'Plug Install',                    ':PlugInstall<CR>'},
  u = {'Plug Update',                     ':PlugUpdate<CR>'},
  o = {'Plugin main page open in Github', ':call OpenGithubPlugin()<CR>'},

}

-- stylua: ignore
space_key_map.q = { --{{{1
  name = "+Quit",

  q = {'Quit',             ':q<CR>'},
  Q = {'Force Quit',       ':q<CR>'},
  a = {'Quit all',         ':qa<CR>'},
  A = {'Quit all (Force)', ':qa!CR<>'},

}


-- stylua: ignore
space_key_map.r = { --{{{1
  name = "+Run",

  r = {'Run Current file',            ':call RunUsingCurrentFiletype(CR)<>'},
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
space_key_map.s = { --{{{1
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

  c = {'Search highlight clean',            ':nohlsearch<CR>'},
  h = {'Search history',                    ':FzfLua search_history<CR>'},

  e = {'Source current file!',              ':so %<CR>'},
  v = {'Source vimrc',                      ':so $XDG_CONFIG_HOME/nvim/init.vim<CR>'},

-- TODO
-- " "call s:nmap_cmd_2('c', 's', 'FZF Schema',                        ':FzfLua colorschemes')


}

-- stylua: ignore
space_key_vmap.s = { --{{{1
  name = "+Search/Source",
  s = {'FZF Search Lines in Current Files', ":<C-U>execute ':MyFzfLuaBlines '.GetCurrentWord('v')<CR>"},
  b = {'FZF Search All Opened Buffers',     ":<C-U>execute ':MyFzfLuaLines '.GetCurrentWord('v')<CR>"},
  g = {'FZF <Rg> Search Current Project',   ":<C-U>execute ':MyFzfLuaGrep '.GetCurrentWord('v')<CR>"},
  r = {'FZF <Rg> Search Current Project',   ":<C-U>execute ':FFRg '.GetCurrentWord('v')<CR>"},
  P = {'FZF <Ag> Search Current Project',   ":<C-U>execute ':MyFzfAg '.GetCurrentWord('v')<CR>"},
}

-- stylua: ignore
space_key_map.t = { --{{{1
  name = "+Toggle",

  s  = {'Toggle Trailling whitespace indicator', ':ToggleWhitespace<CR>'},
  S  = {'Toggle Strip Whitespace On Save',       ':ToggleStripWhitespaceOnSave<CR>'},
  m  = {'Color: Dark/Light Mode',                ':ToggleColorschemeMode<CR>'},
  M  = {'Color: FZF Schema',                     ':FzfLua colorschemes<CR>'},
  t  = {'Toggle 80 text width',                  ':call ToggleTextWidth(CR)<>'},
  T  = {'TagbarToggle',                          ':TagbarToggle<CR>'},
  d  = {'toggle diagnostics',                    '<Cmd>lua vim.diagnostic.toggle()<CR>'},

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
  n = {'number',         'set',      'Toggle line number '},
  r = {'relativenumber', 'set',      'Toggle relative line number'},
  w = {'wrap',           'set',      'Toggle line wrap'},
  h = {'hlsearch',       'set',      'Toggle highlight matches'},
  i = {'list',           'set',      'Toggle invisible char (set list)'},
}

local settingToggle = function(keymap)
  for k, v in pairs(keymap) do
    space_key_map.t[k] = { v[3], ":" .. v[2] .. " " .. v[1] .. "!<CR>:" .. v[2] .. " " .. v[1] .. "?<CR>" }
  end
end
settingToggle(toggle_keymap)

vim.cmd("nnoremap \\s :call LoopFoldMethod()<CR>:set foldmethod<CR>zv")

-- stylua: ignore
space_key_map.w = { --{{{1
  name = "+Window",

  c = {'Window Close',            ':call undoquit#SaveWindowQuitHistory()<CR>:close<CR>'},
  h = {'Window Hide',             ':call undoquit#SaveWindowQuitHistory()<CR>:close<CR>'},
  d = {'Window Close and Delete', ':call undoquit#SaveWindowQuitHistory()<CR>:bd!<CR>'},
  u = {'Undoquit Window',         ':Undoquit<CR>'},
  q = {'Write and quit',          ':wq<CR>'},
  w = {'VimWiki Index Page',      ':e ~/vimwiki/index.md<CR>'},
  s = {'Window Swap',             ':call WindowSwap#EasyWindowSwap()<CR>'},
  n = {'Open Window in new tab',  ':tabedit %<CR>'},

}


-- stylua: ignore
space_key_map.z = { --{{{1
  name = "+Mist",

  z = {'Goyo', ':Goyo<CR>'},
  m = {'Zoom', ':ZoomToggle<CR>'},
  o = {'Zoom', ':ZoomToggle<CR>'},
  l = {'Limelight', ':Limelight!!<CR>'},
  u = {'Undo tree', ':UndotreeToggle<CR>'},

}


-- stylua: ignore
space_key_map.X = { --{{{1
  name = "+Help",
}


-- Register key map {{{1
--

reformate_key_map(space_key_map)
reformate_key_map(space_key_vmap)
wk.register(space_key_map, { prefix = "<Space>" })
wk.register(space_key_vmap, { prefix = "<Space>", mode = "v" })

-- -- WIP
-- wk.register({
--   f = {
--     z = { ":<C-U>execute ':MyFzfFiles' SafeFzfQuery(GetCurrentWord('n'))<CR>", "FZF File under cursor" },
--   },
-- }, { prefix = "<Space>" })

------------------------------------------------------------------------------
-- Misc {{{1

