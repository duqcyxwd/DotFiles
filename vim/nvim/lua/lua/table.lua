local M = {}

function M.keys(t)
  local res = {}

  for key, _ in pairs(t) do
    -- print(key)
    table.insert(res, key)
  end
  return res
end

function M.vals(t)
  local res = {}

  for _, value in pairs(t) do
    table.insert(res, value)
  end
  return res
end

function M.contain(table, o)
  for _, v in ipairs(table) do
    if o == v then
      return true
    end
  end
  return false
end
local contain_test = function ()
  -- local a = { 1, 2, 3 }
  -- P(M.contain (a, 1))
  -- local a = { "A", "B", "C" }
  -- P(M.contain (a, "B"))
end



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
local remove_dups_test = function ()
  -- local test = {1,2,4,2,3,4,2,3,4,"A", "B", "A"}
  -- nvim_print(M.remove_dups(test))
end

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
local join_test = function ()
  -- local b = { 1, 2, 3 }
  -- nvim_print(M.join(a, b))
  -- print("-----------")
  -- nvim_print(a)
  -- print("-----------")
  -- nvim_print(b)
end

-- Deep merge
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

return M
