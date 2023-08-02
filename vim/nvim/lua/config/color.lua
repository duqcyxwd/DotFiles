local M = {}


-- Color Theme
M.dark = function()
  -- Hack: Call colorscheme twice to get correct color
  vim.cmd("colorscheme dracula")
  vim.cmd("colorscheme dracula")

  -- Color setting for better white space ExtraWhitespace defined in  [[better-whitespace]]
  vim.cmd [[highlight ExtraWhitespace ctermbg=LightYellow guibg=#6272a4 ]]
  vim.cmd [[highlight ExtraWhitespace ctermbg=LightYellow guibg=#6272a4 ]]
end
M.light = function()
  vim.cmd("colorscheme github_light")

  vim.cmd [[highlight ExtraWhitespace ctermbg=darkmagenta guibg=darkmagenta]]
end

-- VIM Multi color
M.vm_dark = function()
  -- Modified from https://github.com/mg979/vim-visual-multi/blob/master/autoload/vm/themes.vim
  vim.cmd("highlight VM_Extend ctermbg=24 guibg=#005f87")
  vim.cmd("highlight VM_Cursor ctermbg=31 ctermfg=237 guibg=#0087af guifg=#87dfff")
  vim.cmd("highlight VM_Insert ctermbg=239 guibg=#4c4e50")
  vim.cmd("highlight VM_Mono ctermbg=167 ctermfg=253 guibg=#df5f5f guifg=#dadada cterm=bold term=bold gui=bold")
end
M.vm_light = function()
  vim.cmd("highlight VM_Extend ctermbg=143 ctermfg=0 guibg=darkkhaki guifg=black")
  vim.cmd("highlight VM_Cursor ctermbg=64 ctermfg=186 guibg=olivedrab guifg=khaki")
  vim.cmd("highlight VM_Insert ctermbg=239 guibg=#4c4e50")
  vim.cmd("highlight VM_Mono ctermbg=131 ctermfg=235 guibg=#AF5F5F guifg=#262626")
end

-- VM Theme Toggle {{{1
local colorscheme_mode = "dark"
M.toggle = function()
  if colorscheme_mode == "dark" then
    colorscheme_mode = "light"
    M.light()
    M.vm_light()
  else
    colorscheme_mode = "dark"
    M.dark()
    M.vm_dark()
  end
end

-- One time setup {{{1
-- Start in dark mode
M.dark()
M.vm_dark()

-- but the color setting in that plugin is not working

return M
