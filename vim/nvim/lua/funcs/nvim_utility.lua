local M = {}

M.on_very_lazy = function(fn) -- Call some method with this helper
  vim.api.nvim_create_autocmd("User", {
    pattern = "VeryLazy",
    callback = function()
      fn()
    end,
  })
end

M.lazy_run_if_has = function (plugin, callback)
  if M.has(plugin) then
    callback()
  end
end

M.has = function(plugin)
  return require("lazy.core.config").spec.plugins[plugin] ~= nil
end

M.get_current_line = function()
  -- Example: require'funcs.nvim_utility'.get_listed_buffer()
  local current_line = vim.fn.line(".")

  return vim.fn.getline(current_line)
end

M.get_visual_select = function()
  -- This method can be replace with luado:
  -- e.g: :luado print(line)
  -- Get the starting and ending positions of the visual selection
  local start_pos = vim.fn.getpos("'<")
  local end_pos = vim.fn.getpos("'>")

  -- Extract the selected text
  local selected_text = vim.fn.getline(start_pos[2], end_pos[2])

  -- Print or use the selected text as needed
  return table.concat(selected_text, "\n")
end

M.show_all_buffers = function()
  local bufs = vim.api.nvim_list_bufs()
  for _, buf in ipairs(bufs) do
    vim.api.nvim_set_option_value("buflisted", true, { buf = buf })
  end
end

M.get_listed_buffer = function()
  -- vim.fn.getbufinfo({ buflisted = 1 })
  local bufs = vim.fn.getbufinfo({ buflisted = 1 })
  local res = {}
  for _, buf in ipairs(bufs) do
    -- table.insert(res, buf.name)
    table.insert(res, buf.bufnr)
  end
  return res
end

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

M.close_current_buffer = function() -- A safe method works for tab, buffer, window
  local buflisted = vim.api.nvim_buf_get_option(0, "buflisted")
  if not buflisted then
    return
  end

  local bufnr = vim.api.nvim_get_current_buf()

  if #M.get_listed_buffer() > 1 then
    vim.api.nvim_set_option_value("buflisted", false, { buf = bufnr })
    vim.cmd("bprevious")
    return
  end

  if #vim.api.nvim_list_tabpages() > 1 then
    print("Call tabc")
    vim.api.nvim_command('tabc')
    return
  end

  vim.api.nvim_command('bdelete')
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
  M.lazy_run_if_has("vim-exchange", function() vim.cmd[[ XchangeClear ]] end)

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
  local pattern = "[a-zA-Z][0-9a-zA-Z|%.%-]+[/][0-9a-zA-Z|%.%-]+"
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
  print("Latest session: " .. require 'auto-session'.get_latest_session())
  print("Project Path:    " .. vim.fn.getcwd())
  print("Current file:    " .. vim.fn.expand("%:p"))
end

return M
