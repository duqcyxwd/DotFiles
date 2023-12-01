require("lua.global")
local tableFn = require("lua.table")

local M = {}

M.on_very_lazy = function(fn) -- Call some method with this helper
  vim.api.nvim_create_autocmd("User", {
    pattern = "VeryLazy",
    callback = function()
      fn()
    end,
  })
end

M.lazy_run_if_has = function(plugin, callback)
  if M.has(plugin) then
    callback()
  end
end

M.has = function(plugin)
  return require("lazy.core.config").spec.plugins[plugin] ~= nil
end


M.get_all_plugins_config_table = function()

  local function load_lua_modules_and_merge(folder_path)
    local big_table = {}
    -- Use Vim's glob function to get all the .lua files recursively
    local files = vim.fn.glob(folder_path .. '/**/*.lua', true, true)
    for _, file in ipairs(files) do
      local module_path = file:sub(#folder_path, -5):gsub('/', '.')
      local loaded_module = require(module_path)
      if type(loaded_module) == "table" then
        big_table = tableFn.join(big_table, loaded_module)
      else
        print("Warning: Module " .. module_path .. " did not return a table")
      end
    end
    return big_table
  end

  local function convertToKeyedTable(tbl)
    -- Creates a new table using the first item of each sub-table as a key
    local result = {}
    for _, v in ipairs(tbl) do
      if type(v) == "string" then
        result[v] = { v }
      else
        local key = v[1]
        if type(key) == "string" then
          result[key] = v
        end
      end
    end
    return result
  end

  -- Example usage
  local merged_table = load_lua_modules_and_merge('$XDG_CONFIG_HOME/nvim/lua/plugins')
  return convertToKeyedTable(merged_table)
end

-- This will introduce 10 - 15ms delay
local plugins_config = {}
local is_inited = false
M.init = function()
  M.plugins_config = M.get_all_plugins_config_table()
  is_inited = true
end

M.get_plugin_config = function (plug)
  return plugins_config[plug]
end

M.enabled = function (plug)
  if not is_inited then
    return true
  end
  local config = plugins_config[plug]
  if config == nil then
    return false
  end
  if config.enabled == nil or config.enabled == false then
    return false
  end
  return true
end

M.get_all_plugins = function()
  return tableFn.keys(require("lazy.core.config").spec.plugins)
end

M.get_plugin = function (plug)
  local plug_info = require("lazy.core.config").spec.plugins[plug]
  vim.notify(vim.inspect(plug_info))
end

-- Editor

M.get_current_line = function()
  -- Example: require'funcs.nvim_utility'.get_listed_buffer()
  local current_line = vim.fn.line(".")

  return vim.fn.getline(current_line)
end

M.get_visual_select = function()


  local mode = vim.fn.mode()
  if mode == 'v' or mode == 'V' then
    -- In Visual model, '< and '> kept last visual position
    -- print("V mode")
    local a_orig = vim.fn.getreg('z')
    vim.cmd([[silent! normal! "zygv]])
    local text = vim.fn.getreg('z')
    vim.fn.setreg('z', a_orig)
    return text
  end

  local _, start_line, start_col, _ = unpack(vim.fn.getpos("'<"))
  local _, end_line, end_col, _ = unpack(vim.fn.getpos("'>"))
  local lines
  if end_col > 1000 then
    -- NVIM bug, when V mode is used, end_col is 2147483647
    lines = vim.api.nvim_buf_get_lines(0, start_line - 1, end_line, false)
  else
    lines = vim.api.nvim_buf_get_text(0, start_line - 1, start_col - 1, end_line - 1, end_col, {})
  end

  -- print(start_line .. " " .. start_col .. " " .. end_line .. " " .. end_col)
  local text = table.concat(lines, "\n")
  -- print(text)
  return text
end

M.escape_next_line = function(input_str)
    return string.gsub(input_str, "\n", "\\n")
end

M.restore_register = function()
  local last = vim.fn.getreg("0")
  vim.fn.setreg('+', last)
  vim.fn.setreg('"', last)
end

M.sync_copy_to_last = function()
  local last = vim.fn.getreg("+")
  vim.fn.setreg('0', last)
end

M.visual_to_search = function (opts)

  opts = tableFn.merge({ revert = false }, opts)
  local mode = vim.fn.mode()
  if mode == 'v' or mode == 'V' then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<ESC>", true, false, true), 'n', true)
  end

  local s = M.escape_next_line(M.get_visual_select())
  vim.fn.setreg('/', '\\V' .. s)
  if not opts.revert then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("/<CR>", true, false, true), 'n', true)
  else
    local start_pos = vim.fn.getpos("'<")
    vim.api.nvim_win_set_cursor(0, {start_pos[2], start_pos[3]-1})
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("?<CR>", true, false, true), 'n', true)
  end

end

M.visual_replace_and_search_next = function(clipboard)
  -- How it works
  -- Register update with option vim.opt.clipboard = "unnamedplus"
  --   1. yank -> 0 " + *
  --   2. copied -> * +
  -- p in normal mode will paste
  -- p in visual mode will
  --   if clipboard is true, update last register 0 to clipboard
  --   1. Set visual select to current search
  --   2. Exit to normal mode
  --   3. Change next match, 'cgn' with data in register 0
  -- p in visual mode limitation: I can't script to directly paste from clipboard register (register * or +),
  --   work around is add a flag in this function.
  --   To support dot, need to use register 0 (not clipboard register, because they are updated after replace).
  --   I can manually restore it orignal content but it is not working for dot
  -- Notes:
  --   Tried to use other register to keep data, not working because I can't update that register with dot
  --
  -- old implement   -- { "P",        '"zy:let @0=@+<CR>:let @/=@Z<CR>cgn<C-R>0<ESC>:let @+=@0<CR>:let @"=@0<CR>'},
  --   1. :let @0=@+<CR>               | Always update last yank content to clipboard
  --   2. :let @+=@0<CR>:let @"=@0<CR> | Restore clipboard from last yank after modification
  --
  -- Work flow limitation
  -- When copy from other place, use gp to paste and update the register
  clipboard = clipboard or false

  if clipboard then
    local last = vim.fn.getreg("+")
    vim.fn.setreg('0', last)
  end


  local s = M.escape_next_line(M.get_visual_select())
  vim.fn.setreg('/', '\\V' .. s)

  local mode = vim.fn.mode()
  if mode == 'v' or mode == 'V' then
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<ESC>", true, false, true), 'n', true)
  end

  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("cgn<C-R>0<ESC>", true, false, true), 'n', true)

end

M.visual_replace_and_select_next = function()
  -- nvgn select next match
  M.visual_replace_and_search_next()
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("nvgn", true, false, true), 'n', true)
end

M.show_all_buffers = function()
  local bufs = vim.api.nvim_list_bufs()
  for _, buf in ipairs(bufs) do
    vim.api.nvim_set_option_value("buflisted", true, { buf = buf })
  end
end

M.get_listed_buffer = function()
  local bufs = vim.fn.getbufinfo({ buflisted = 1 })
  local res = {}
  for _, buf in ipairs(bufs) do
    -- table.insert(res, buf.name)
    table.insert(res, buf.bufnr)
  end
  return res
end

M.get_next_list_buffer = function()
  local cbufnr = vim.api.nvim_get_current_buf()
  local bufs = vim.fn.getbufinfo({ buflisted = 1 })
  for _, buf in ipairs(bufs) do
    if cbufnr ~= buf.bufnr then
      return buf.bufnr
    end
  end
  return nil
end

M.toggle_char = function (char)
  local delimiters = { ',', ';', '.', '!', ':' }
  local line = vim.api.nvim_get_current_line()
  local last_char = line:sub(-1)

  if last_char == char then
    return vim.api.nvim_set_current_line(line:sub(1, #line - 1))
  elseif vim.tbl_contains(delimiters, last_char) then
    return vim.api.nvim_set_current_line(line:sub(1, #line - 1) .. char)
  else
    return vim.api.nvim_set_current_line(line .. char)
  end
end

-- Buffer/Windows

M.is_last_window = function()
  -- require'funcs.nvim_utility'.is_last_window()
  -- Get a list of all open windows
  local windows = vim.api.nvim_list_wins()

  -- Get the current window
  local current_window = vim.api.nvim_get_current_win()

  -- Get the last window ID from the list
  local last_window = windows[#windows]

  -- Check if the current window is the last open window
  local is_last_open_window = current_window == last_window

  -- Print the result
  return is_last_open_window
end

M.is_in_other_tab = function(bufnr)
  local tab_buffer_info = require 'scope.core'.cache
  local tabnr = vim.api.nvim_get_current_tabpage()
  for tab_id, buf_list in ipairs(tab_buffer_info) do
    if tab_id ~= tabnr then
      if tableFn.contain(buf_list, bufnr) then
        return true
      end
    end
  end
  return false
end

-- nvim_print(vim.fn.getbufinfo(vim.api.nvim_get_current_buf())[1])
-- nvim_print(vim.api.nvim_list_wins())
-- lua vim.api.nvim_win_set_buf(1198, 2)
-- nvim_print(vim.fn.getbufinfo({ buflisted = 1 }))

-- local bufs = vim.fn.getbufinfo({ buflisted = 1 })
-- local res = {}
-- for _, buf in ipairs(bufs) do
--   table.insert(res, buf.name)
-- end
-- nvim_print(res)

-- Step 1, eavl lua code
-- Visua/Normal: <SPC> sl
-- vim.api.nvim_set_option_value("buflisted", true, { buf = 1 })
-- vim.api.nvim_set_option_value("buflisted", false, { buf = 1 })
-- nvim_print(vim.api.nvim_list_bufs())
-- Step 2. Eval and print/inspect lua code
-- Visua/Normal: <SPC> si
-- vim.api.nvim_list_bufs()


-- Buffers nvim API
-- vim.api.nvim_list_bufs()
-- vim.api.nvim_buf_get_option(1)
--
-- vim.fn.getbufinfo({ buflisted = 1 })
-- vim.fn.getbufinfo({ bufnr = 7 })
-- vim.fn.nvim_list_tabpages()
-- vim.api.nvim_tabpage_is_valid(9)
-- vim.api.nvim_tabpage_list_wins(9)
-- vim.api.nvim_get_current_tabpage()
-- vim.api.nvim_tabpage_list_wins(vim.api.nvim_get_current_tabpage())
--
-- Window
-- nvim_win_get_tabpage({window})

M.smart_buffer_close = function() -- A safe method works for tab, buffer
  local opts = { close_window_fisrt = false }
  local buflisted = vim.api.nvim_buf_get_option(0, "buflisted")
  if not buflisted then
    -- Special case that current buffer is a unlisted buffer
    vim.api.nvim_command('bdelete')
    return
  end

  local bufnr = vim.api.nvim_get_current_buf()
  local nbufnr = M.get_next_list_buffer()
  local attached_windows = vim.fn.getbufinfo(bufnr)[1].windows

  local close_fn = function(buf_nr)
    if M.is_in_other_tab(bufnr) then
      vim.api.nvim_set_option_value("buflisted", false, { buf = buf_nr })
    else
      vim.api.nvim_buf_delete(buf_nr, { force = false })
    end
  end

  local update_attached_window_buffer = function(buf_nr)
    for _, win_id in ipairs(attached_windows) do
      vim.api.nvim_win_set_buf(win_id, buf_nr)
    end
  end

  if nbufnr ~= nil then
    -- When there is another buffer.
    -- Check if it is last window

    -- Optional, close window if it is not the last attached
    if #attached_windows > 1 and opts.close_window_fisrt then
      vim.api.nvim_win_close(0, false)
      return
    end

    update_attached_window_buffer(nbufnr)
    close_fn(bufnr)
  else
    -- When there is no another buffer.
    -- Check last window
    -- Check last tab

    -- Close window first if exists
    if #attached_windows > 1 and opts.close_window_fisrt then
      vim.api.nvim_win_close(0, false)
      return
    end

    -- Close current tab if it is not last one
    if #vim.api.nvim_list_tabpages() > 1 then
      vim.api.nvim_command('tabc')
      return
    end

    close_fn(bufnr)
  end

end

M.clear = function() -- Clear floating window and searches
  local function close_float()
    -- https://github.com/neovim/neovim/issues/11440#issuecomment-877693865
    local this_win = vim.fn.win_getid()
    -- close all floating windows that are relative to the current one
    for _, win in ipairs(vim.api.nvim_list_wins()) do
      local win_config = vim.api.nvim_win_get_config(win)
      -- If the mapping doesn't close enough windows, use the following line instead:
      -- if win_config.relative ~= "" then
      if win_config.relative == "win" and win_config.win == this_win then
        vim.api.nvim_win_close(win, false)
      end
    end
  end

  vim.cmd("nohlsearch")

  vim.lsp.buf.clear_references()
  M.lazy_run_if_has("nvim-notify", function() require('notify').dismiss() end)
  M.lazy_run_if_has("vim-exchange", function() vim.cmd [[ XchangeClear ]] end)

  if vim.api.nvim_get_namespaces().flash ~= nil then
    vim.api.nvim_buf_clear_namespace(0, vim.api.nvim_get_namespaces().flash, 0, -1)
  end
  -- TOWO check plugin before call

  close_float()
end

M.goto_first_float = function()
  for _, w in ipairs(vim.api.nvim_list_wins()) do
    local config = vim.api.nvim_win_get_config(w)
    if config.focusable and config.relative ~= "" then
      vim.api.nvim_set_current_win(w)
      vim.api.nvim_buf_set_keymap(0, 'n', 'q', ':q<CR>', { noremap = true })
      return
    end
  end
end

-- Others

M.diff_with_saved = function() -- | Diff current buffer with saved file
  -- Save the current filetype
  local filetype = vim.bo.filetype

  -- Mark the current buffer as "diffthis" to highlight differences
  vim.cmd('diffthis')

  -- Create a new vertical split and read the current buffer into it
  vim.cmd('vnew | r # | normal! 1Gdd')

  -- Mark the new buffer as "diffthis" to highlight differences
  vim.cmd('diffthis')

  -- Set the new buffer options
  vim.bo.buftype = 'nofile'
  vim.bo.bufhidden = 'wipe'
  vim.bo.buflisted = false
  vim.bo.readonly = true
  vim.bo.filetype = filetype
end

M.open_github_plugin = function()
  -- Get the plugin name and owner/repo from the current line
  local line = vim.fn.getline('.')
  local pattern = "[a-zA-Z][0-9a-zA-Z|%.%-%_]+[/][0-9a-zA-Z|%.%-%_]+"
  local plugin = line:match(pattern)


  if plugin and plugin ~= "" then
    local uri = "https://www.github.com/" .. plugin
    os.execute('open ' .. vim.fn.shellescape(uri))
    -- vim.cmd('redraw!')
  else
  end
end

M.show_buffer_info = function()
  -- Verbose output for syntax, filetype, foldmethod, and foldexpr settings
  vim.cmd('verbose set syntax filetype foldmethod foldexpr')
  print("---------------------------------------------------------------")

  -- Print buffer information
  -- print("Current session: " .. get_current_session())
  -- print("Latest session: " .. require 'auto-session'.get_latest_session())
  print("Project Path:    " .. vim.fn.getcwd())
  print("Current file:    " .. vim.fn.expand("%:p"))
end

return M
