local M = {}

local last_bracket_jump = 'q'  -- quickfix by default

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


return M
