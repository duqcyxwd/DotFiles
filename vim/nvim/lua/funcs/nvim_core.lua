local M = {}

function M.nvim_create_augroups(definitions)
  for group_name, definition in pairs(definitions) do
    vim.api.nvim_command("augroup " .. group_name)
    vim.api.nvim_command("autocmd!")
    for _, def in ipairs(definition) do
      local command = table.concat(vim.tbl_flatten({ "autocmd", def }), " ")
      vim.api.nvim_command(command)
    end
    vim.api.nvim_command("augroup END")
  end
end

function M.autocmd(event, pattern, cmd_or_callback, group_id)
  local opts = {
    pattern = pattern,
    group = group_id,
  }

  if type(cmd_or_callback) == "function" then
    opts.callback = cmd_or_callback
  else
    opts.command = cmd_or_callback
  end

  vim.api.nvim_create_autocmd(event, opts)
end

function M.autogroup(definitions)
  for group_name, definition in pairs(definitions) do
    local group_id = vim.api.nvim_create_augroup(group_name, { clear = true })

    for _, def in ipairs(definition) do
      local event, pattern, cmd_or_callback = unpack(def)
      M.autocmd(event, pattern, cmd_or_callback, group_id)
    end
  end
end

function M.normalMap(binding, command, opts)
  opts = opts or { silent = true }
  vim.keymap.set("n", binding, command, opts)
end

function M.repeatableCall2(func)
  -- map unique Plug mapping using tostring of function
  local mapName = "<Plug>" .. tostring(func):gsub("function: ", "")
  -- mapping including vim-repeat magic
  local repeatMap = mapName .. [[:silent! call repeat#set("\]] .. mapName .. [[", v:count)<CR>]]
  print(mapName)
  -- vim repeat https://github.com/tpope/vim-repeat
  -- silent! call repeat#set("\<Plug>MyWonderfulMap", v:count)
  -- M.normalMap(mapName, func)
  -- M.normalMap(binding, repeatMap)
  print('execute "normal ' .. repeatMap .. '"')
  vim.cmd('execute "normal ' .. repeatMap .. '"')
  -- func()
end

-- Save last Fn reference so we can use dot to repeate it
-- Vim Repeat: https://github.com/tpope/vim-repeat
-- https://gist.github.com/kylechui/a5c1258cd2d86755f97b10fc921315c3
-- nvim_surround https://github.com/kylechui/nvim-surround/blob/7b8a295a27038715bc87c01277d82c294b690e6d/lua/nvim-surround/cache.lua#L12
M.lastFnRef = function () end
M.lastFnArgs = nil

function M.runLastFn()
  M.lastFnRef(M.lastFnArgs)
end

-- Exmaple mapping
--   ["."] = { 'Toggle Append .', core.dotCall(vim_u.toggle_char, '.'), expr = true },
-- func wrapper so that I can use dot to repeat it
function M.dotCall(func, args)
  return function()
    M.lastFnArgs = args
    M.lastFnRef = func
    vim.go.operatorfunc = "v:lua.require'funcs.nvim_core'.runLastFn"
    return "g@l"
  end
end

function M.jumpWrap(key)
  local count = vim.v.count

  -- If no count is provided, count is 1
  if count == 0 then
    count = 1
  end

  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("m'", true, false, true), 'n', true)
  vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("" .. count .. key, true, false, true), 'n', true)
end

return M
