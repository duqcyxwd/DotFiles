local M = {}

local last_bracket_jump = 'b'  -- quickfix by default

local function bracket_jump(bracket)
		print(bracket .. last_bracket_jump)
    vim.api.nvim_feedkeys(bracket .. last_bracket_jump, 'm', true)
end

M.prev = function()
    bracket_jump('[')
end

M.next = function()
    bracket_jump(']')
end

M.set = function(char)
    last_bracket_jump = char
end

M.get = function()
    return last_bracket_jump
end



-- Add bracket jump mapping
-- Smart way to add next/prev but not needed
local addNextFamily = function(impair_map_table)  --{{{2
  local map_family_table = {
    -- keys will be map to e.g bnext bprev
    { "b", "b" },    -- Buffer
    -- { "c", "c" },    -- next error
    { "q", "q" },    -- quick fixt
    { "t", "tab" },  -- Tab
  }
  for _, entry in pairs(map_family_table) do
    table.insert(impair_map_table, { "]" .. entry[1], "<cmd>" .. entry[2] .. "next<CR>" })
    table.insert(impair_map_table, { "[" .. entry[1], "<cmd>" .. entry[2] .. "prev<CR>" })
    table.insert(impair_map_table, { "]" .. entry[1]:upper(), "<cmd>" .. entry[2] .. "last<CR>" })
    table.insert(impair_map_table, { "[" .. entry[1]:upper(), "<cmd>" .. entry[2] .. "first<CR>" })
  end
end

return M
