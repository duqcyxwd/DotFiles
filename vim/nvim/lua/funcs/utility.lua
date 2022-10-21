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

function merge(t1, t2)
  if type(t2) ~= "table" then
    return t1
  end
  for k, v in pairs(t2) do
    if type(v) == "table" then
      if type(t1[k] or false) == "table" then
        merge(t1[k] or {}, t2[k] or {})
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

function remove_dups(table)
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
-- nvim_print(remove_dups(test))

function join(t1, t2)
  local res = {}
  table.foreach(t1, function(_, v)
    table.insert(res, v)
  end)
  table.foreach(t2, function(_, v)
    table.insert(res, v)
  end)
  return res
end

-- nvim_print(join({ 1, 2, 3 }, { 4, 5, 6 }))


function GetKeys(maps)
  -- get keys from a impar map
  local res = {}
  for _, entry in pairs(maps) do
    local c = string.gsub(entry[1], "[%[%]]", "")
    table.insert(res, c)
  end
  return res
end

local tuple_obj = { --{{{2
  { "k1", "value_1" },
  { "k2", "value_1" },
  { "k3", "value_1" },
}
-- nvim_print(getKeys(tuple_obj))
