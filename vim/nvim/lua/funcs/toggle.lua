-- " Setting toggle/Config Functions

local core = require("funcs.nvim_core")
local u = require('funcs.utility')

M = {}

M.vim_diff_whitespace = function() --| " [Functions] Vim diff whitespace toggle
  if vim.o.diffopt:find('iwhite', 1, true) then
    vim.o.diffopt = vim.o.diffopt:gsub(',iwhite', '')
  else
    vim.o.diffopt = vim.o.diffopt .. ',iwhite'
  end
end

M.highlight_over_80 = function()
  if vim.o.textwidth == 0 then
    vim.o.textwidth = 80
    vim.g.highlightCharacterOver80 = 1
    vim.cmd('highlight OverLength ctermbg=red ctermfg=white guibg=#592929')
    vim.cmd('match OverLength /\\%81v.\\+/')
    vim.cmd('syntax enable')
  else
    vim.o.textwidth = 0
    vim.g.highlightCharacterOver80 = 0
    vim.cmd('highlight OverLength ctermbg=red ctermfg=white guibg=#592929')
    vim.cmd('match OverLength /\\%999v.\\+/')
    vim.cmd('syntax enable')
  end
end

local foldMethodList = { 'indent', 'expr', 'marker', 'syntax' }
local foldMethodIndex = 1

M.loop_fold_method = function()
  print('set foldmethod=' .. foldMethodList[foldMethodIndex])
  vim.cmd('set foldmethod=' .. foldMethodList[foldMethodIndex])
  foldMethodIndex = foldMethodIndex % #foldMethodList + 1
end

local default_find_group_pattern = { '.git/' }
local local_find_group_pattern = { '.vimroot', '.svn/', '.hg/', '.bzr/', 'Rakefile', 'pom.xml', 'project.clj', '*.csproj', '*.sln', }

-- Set initial findroot patterns
local vim_root = true
vim.g.findroot_patterns = u.join(default_find_group_pattern, local_find_group_pattern)
vim.g.findroot_not_for_subdir = 1

local find_root_enable = true
function M.toggle_find_root()
  vim.opt.autochdir = false
  if find_root_enable then
    print("Disable find root")
    print("PWD: " .. vim.fn.getcwd())
    find_root_enable = false
    vim.cmd("CDC")
    local find_root_autocmd = {
      FindRoot = {
        { "BufEnter", '*', "CDC" }
      }
    }
    core.nvim_create_augroups(find_root_autocmd)
  else
    print("Enable find root")
    print("PWD: " .. vim.fn.getcwd())
    find_root_enable = true
    vim.cmd('FindRoot!')

    local find_root_autocmd = {
      FindRoot = {
        { "BufEnter", vim.g.findroot_mask or '*', ":call findroot#cd(0)" }
      }
    }
    core.nvim_create_augroups(find_root_autocmd)
  end
end

function M.toggle_find_root_scope()
  vim.opt.autochdir = false
  if not find_root_enable then
    print("vim-findroot is not enabled")
    return
  end
  if vim_root then
    print("[Parent] find root path " .. vim.fn.getcwd())
    vim_root = false

    vim.g.findroot_patterns = default_find_group_pattern
    nvim_print(vim.g.findroot_patterns)
    vim.cmd('FindRoot!')
  else
    print("[VimRoot] find root path " .. vim.fn.getcwd())
    vim_root = true

    vim.g.findroot_patterns = u.join(default_find_group_pattern, local_find_group_pattern)
    nvim_print(vim.g.findroot_patterns)
    vim.cmd('FindRoot!')
  end
end

return M
