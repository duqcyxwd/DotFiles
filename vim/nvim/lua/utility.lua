
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


