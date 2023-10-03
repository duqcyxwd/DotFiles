local M = {}

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

vim.cmd("command! ReloadConfig lua ReloadConfig()")

-- Merge maps
M.merge = function(tbl1, tbl2)
  local is_dict1 = type(tbl1) == 'table' and (not vim.tbl_islist(tbl1) or vim.tbl_isempty(tbl1))
  local is_dict2 = type(tbl2) == 'table' and (not vim.tbl_islist(tbl2) or vim.tbl_isempty(tbl2))
  if is_dict1 and is_dict2 then
    local new_tbl = {}
    for k, v in pairs(tbl2) do
      if tbl1[k] ~= M.none then
        new_tbl[k] = M.merge(tbl1[k], v)
      end
    end
    for k, v in pairs(tbl1) do
      if tbl2[k] == nil then
        if v ~= vim.NIL then
          new_tbl[k] = M.merge(v, {})
        else
          new_tbl[k] = nil
        end
      end
    end
    return new_tbl
  end

  if tbl1 == M.none then
    return nil
  elseif tbl1 == nil then
    return M.merge(tbl2, {})
  else
    return tbl1
  end
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
