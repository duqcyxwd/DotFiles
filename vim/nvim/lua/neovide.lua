local M = {}

-- Background Color {{{1

local font = [[JetBrainsMono\ Nerd\ Font]]
local transparency = 0.92
local fontsize = 15

vim.opt.linespace = 0

-- g:neovide_transparency should be 0 if you want to unify transparency of content and title bar.
vim.g.neovide_transparency = 0.0
vim.g.transparency = 0.8

-- Helper function for transparency formatting
local alpha = function()
  return string.format("%x", math.floor((255 * vim.g.transparency) or 0.8))
end
vim.g.neovide_background_color = "#0f1117" .. alpha()

-- Padding
vim.g.neovide_padding_top = 0
vim.g.neovide_padding_bottom = 0
vim.g.neovide_padding_right = 0
vim.g.neovide_padding_left = 0

vim.g.neovide_floating_blur_amount_x = 2.0 -- Floating Blur Amount

vim.g.neovide_refresh_rate = 60
vim.g.neovide_refresh_rate_idle = 5

vim.g.neovide_remember_window_size = false
vim.g.neovide_cursor_vfx_mode = "railgun"

-- Utility Fns {{{1

M.set_font_size = function(size)
  vim.cmd(string.format('set guifont=%s:h%d', font, size))
end

M.set_transparency = function(trans)
  vim.cmd(string.format('let g:neovide_transparency=%s', trans))
end

local current_fontsize = fontsize
M.change_font_size = function(amount)
  if current_fontsize + amount < 1 or current_fontsize + amount > 100 then
    return
  end
  current_fontsize = current_fontsize + amount
  M.set_font_size(current_fontsize)
end

local current_transparency = transparency
M.change_transparency = function(amount)
  if current_transparency + amount < 0.0 or current_transparency + amount > 1.0 then
    return
  end
  current_transparency = current_transparency + amount
  M.set_transparency(current_transparency)
end



-- Mapping {{{1


-- stylua: ignore

M.int_keymapping = function ()

  -- Formatter { let @a = "ga3ap*," }

  vim.keymap.set("n", "<D-v>", '"+p',                                           { desc = "Paste" })
  vim.keymap.set("v", "<D-v>", "<C-r>+",                                        { desc = "Paste" })
  vim.keymap.set("c", "<D-v>", "<C-r>+",                                        { desc = "Paste" })

  vim.keymap.set("n", "<D-=>", function() M.change_font_size(1) end,            { desc = "increase font size" })
  vim.keymap.set("n", "<D-->", function() M.change_font_size(-1) end,           { desc = "decrease font size" })
  vim.keymap.set("n", "<D-0>", function() M.set_font_size(fontsize) end,        { desc = "default font size" })

  vim.keymap.set("n", "<C-=>", function() M.change_transparency(0.1) end,       { desc = "increase transparency" })
  vim.keymap.set("n", "<C-->", function() M.change_transparency(-0.1) end,      { desc = "decrease transparency" })
  vim.keymap.set("n", "<C-0>", function() M.set_transparency(transparency) end, { desc = "default transparency" })

end



-- Init {{{1


M.set_font_size(fontsize)
M.set_transparency(transparency)
M.int_keymapping()

return M
