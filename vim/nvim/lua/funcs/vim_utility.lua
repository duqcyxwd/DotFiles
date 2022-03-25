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

M.clear = function ()
  -- Clear floating window and searches
  vim.cmd("nohlsearch")
  vim.lsp.buf.clear_references()
  IfHas("notify", function(notify)
    notify.dismiss()
  end)
  close_float()
end


return M
