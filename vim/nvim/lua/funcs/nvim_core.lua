local M = {}

function M.nvim_create_augroups(definitions)
	for group_name, definition in pairs(definitions) do
		vim.api.nvim_command('augroup '..group_name)
		vim.api.nvim_command('autocmd!')
		for _, def in ipairs(definition) do
			local command = table.concat(vim.tbl_flatten{'autocmd', def}, ' ')
			vim.api.nvim_command(command)
		end
		vim.api.nvim_command('augroup END')
	end
end

function M.autocmd(event, pattern, cmd_or_callback, group_id)
  local opts = {
    pattern = pattern,
    group = group_id
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

return M
