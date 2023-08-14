local M = {}

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

M.clear = function()
  -- Clear floating window and searches
  vim.cmd("nohlsearch")
  vim.lsp.buf.clear_references()
  IfHas("notify", function(notify)
    notify.dismiss()
  end)
  close_float()
end

M.on_very_lazy = function(fn)
  vim.api.nvim_create_autocmd("User", {
    pattern = "VeryLazy",
    callback = function()
      fn()
    end,
  })
end

M.has = function(plugin)
  return require("lazy.core.config").spec.plugins[plugin] ~= nil
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


-- local line = "jackMort/ChatGPT.nvim"
-- local pattern = "[a-zA-Z][0-9a-zA-Z|%.%-]+[/][0-9a-zA-Z|%.%-]+"
-- print(string.match(line, pattern))
-- print(string.match("--- xx/xxsa12", pattern))
-- print(string.match("--- x-y", pattern))
-- print(string.match("--- xy", pattern))
-- print(string.match("--- xaaay/x|xa", pattern))
-- print(string.match("--- 0xaaay/x|xa", pattern))
-- print(string.match("--- -0xaaay/x|xa", pattern))
-- print(string.match("--- xx-0xaaay/x-xa.nvim", pattern))


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

function M.show_buffer_info()
  -- Verbose output for syntax, filetype, foldmethod, and foldexpr settings
  vim.cmd('verbose set syntax filetype foldmethod foldexpr')
  print("---------------------------------------------------------------")

  -- Print buffer information
  -- print("Current session: " .. get_current_session())
  print("Latest session: " ..require'auto-session'.get_latest_session())
  print("Project Path:    " .. vim.fn.getcwd())
  print("Current file:    " .. vim.fn.expand("%:p"))
end

return M
