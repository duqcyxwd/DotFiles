local M = {}

M.create_grouped_autocmds = function(groups)
  for group_name, events in pairs(groups) do
    local group = vim.api.nvim_create_augroup(group_name, {clear = true})
    for event, event_spec in pairs(events) do
      event_spec.group = group
      vim.api.nvim_create_autocmd(event, event_spec)
    end
  end
end

return M
