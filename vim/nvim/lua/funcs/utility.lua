local M = {}

function nvim_print(...)
  if select("#", ...) == 1 then
    vim.api.nvim_out_write(vim.inspect((...)))
  else
    vim.api.nvim_out_write(vim.inspect {...})
  end
  vim.api.nvim_out_write("\n")
  -- # Notes: Otherway to show message
  -- vim.notify(vim.inspect(plug_info))
end



function _G.ReloadConfig() -- ReloadConfig {{{1
  local hls_status = vim.v.hlsearch
  for name, _ in pairs(package.loaded) do
    if name:match("^cnull") then
      package.loaded[name] = nil
    end
  end

  dofile(vim.env.MYVIMRC)
  if hls_status == 0 then
    vim.opt.hlsearch = false
  end
end

-- vim.api.nvim_set_keymap('n', '<space>vs', '<Cmd>lua ReloadConfig()<CR>', { silent = true, noremap = true })
vim.cmd("command! ReloadConfig lua ReloadConfig()")

-- Merge maps
function M.merge(t1, t2)
  if type(t2) ~= "table" then
    return t1
  end
  for k, v in pairs(t2) do
    if type(v) == "table" then
      if type(t1[k] or false) == "table" then
        M.merge(t1[k] or {}, t2[k] or {})
      else
        t1[k] = v
      end
    else
      t1[k] = v
    end
  end
  return t1
end

-- nvim_print(merge({ a = 10, b = 20 }, { b = 10 }))

function M.remove_dups(table)
  local hash = {}
  local res = {}

  for _, v in ipairs(table) do
    if not hash[v] then
      res[#res + 1] = v -- you could print here instead of saving to result table if you wanted
      hash[v] = true
    end
  end

  return res
end

-- local test = {1,2,4,2,3,4,2,3,4,"A", "B", "A"}
-- nvim_print(REMOVE_DUPS(test))

function M.join(...)
  local resultTable = {}

  -- Loop through each argument (table) in the list
  for _, tableToJoin in ipairs({ ... }) do
    -- Loop through the elements in the current table and add them to the result table
    for _, value in ipairs(tableToJoin) do
      table.insert(resultTable, value)
    end
  end

  return resultTable
end

-- local a = { 1, 2, 3 }
-- local b = { 1, 2, 3 }
-- nvim_print(M.join(a, b))
-- print("-----------")
-- nvim_print(a)
-- print("-----------")
-- nvim_print(b)

function IS_IN(s, list)
  for _, v in ipairs(list) do
    if string.match(s, v) then
      return true
    end
  end
  return false
end


function M.is_in(s, list)
  for _, i in ipairs(list) do
    if s == i then
      return true
    end
  end
  return false
end

-- local a = { 1, 2, 3 }
-- nvim_print(M.is_in (1, a))

return M
