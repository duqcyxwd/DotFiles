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
-- Merge t2 into t1
function M.merge(t1, t2)
  if not t1 and not t2 then return {} end
  if not t1 then return t2 end
  if not t2 then return t1 end
  for k, v in pairs(t2) do
    if type(v) == "table" and type(t1[k]) == "table" then
      M.merge(t1[k], v)
    else
      t1[k] = v
    end
  end
  return t1
end

-- nvim_print(M.merge({ a = 10, b = 20 }, { b = 10 }))
-- nvim_print(M.merge({ a = 10, b = 20 }, nil))
-- nvim_print(M.merge({ a = 10, b = 20 }, { b = 10, c = 101 }))

function M.deep_copy(orig)
  local orig_type = type(orig)
  local copy
  if orig_type == 'table' then
    copy = {}
    for orig_key, orig_value in next, orig, nil do
      copy[M.deep_copy(orig_key)] = M.deep_copy(orig_value)
    end
    setmetatable(copy, M.deep_copy(getmetatable(orig)))
  else   -- number, string, boolean, etc
    copy = orig
  end
  return copy
end

function M.deep_clone_merge(t1, t2)
  if not t1 and not t2 then return {} end
  if not t1 then return M.deep_copy(t2) end
  if not t2 then return M.deep_copy(t1) end

  local merged = M.deep_copy(t1)
  for k, v in pairs(t2) do
    if type(v) == "table" and type(merged[k]) == "table" then
      merged[k] = M.deep_clone_merge(merged[k], v)
    else
      merged[k] = M.deep_copy(v)
    end
  end
  return merged
end

return M
