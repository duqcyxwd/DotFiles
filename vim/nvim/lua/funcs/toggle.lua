-- " Setting toggle/Config Functions

local M = {}

M.vim_diff_whitespace = function() --| " [Functions] Vim diff whitespace toggle
  if vim.o.diffopt:find('iwhite', 1, true) then
    vim.o.diffopt = vim.o.diffopt:gsub(',iwhite', '')
  else
    vim.o.diffopt = vim.o.diffopt .. ',iwhite'
  end
end

M.highlight_over_80 = function()
  local fn = function()
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
  M.highlight_over_80 = fn
  fn()
end

M.loop_fold_method = function()
  local foldMethodList = { 'indent', 'expr', 'marker', 'syntax' }
  local foldMethodIndex = 1

  local fn = function()
    print('set foldmethod=' .. foldMethodList[foldMethodIndex])
    vim.cmd('set foldmethod=' .. foldMethodList[foldMethodIndex])
    foldMethodIndex = foldMethodIndex % #foldMethodList + 1
  end

  M.loop_fold_method = fn
  fn()
end

M.r_number = {
  state = true,

  toggle = function()
    -- Set init state
    M.r_number.state = vim.opt.relativenumber
    local fn = function()
      if M.r_number.state then
        M.r_number.state = false
        vim.opt.relativenumber = false
      else
        M.r_number.state = true
        vim.opt.relativenumber = true
      end
    end
    M.r_number.toggle = fn
    fn()
  end,

  enterHook = function()
    if M.r_number.state and vim.o.nu and vim.api.nvim_get_mode().mode ~= "i" then
      vim.opt.relativenumber = true
    end
  end,

  leaveHook = function()
    if vim.opt.number then
      vim.opt.relativenumber = false
      vim.cmd "redraw"
    end
  end,

}


-- Toggle Project Root
RootPattern = {
  default = { '.git/' },
  more = { '.git/', '.vimroot', '.svn/', '.hg/', '.bzr/', 'Rakefile', 'pom.xml', 'project.clj', '*.csproj', '*.sln', },
  current = {}
}

M.set_root = function()
  RootPattern.current = RootPattern.more
  -- Cache to use for speed up (at cost of possibly outdated results)
  local root_cache = {}
  local fn = function()
    -- Get directory path to start search from
    local path = vim.api.nvim_buf_get_name(0)
    if path == '' then return end
    path = vim.fs.dirname(path)

    -- -- TODO Add cache with different pattern
    -- -- Try cache and resort to searching upward for root directory
    -- local root = root_cache[path]
    -- if root == nil then
    --   local root_file = vim.fs.find(current, { path = path, upward = true })[1]
    --   if root_file == nil then return end
    --   root = vim.fs.dirname(root_file)
    --   root_cache[path] = root
    -- end

    -- Work without cache
    local root_file = vim.fs.find(RootPattern.current, { path = path, upward = true })[1]
    if root_file == nil then return end
    local root = vim.fs.dirname(root_file)

    -- Set current directory
    vim.fn.chdir(root)
    print("Set root path: " .. vim.fn.getcwd())
  end
  M.set_root = fn
  fn()
end

M.toggle_set_root_scope = function()
  local root_scope_flat = true
  local fn = function()
    vim.opt.autochdir = false
    if root_scope_flat then
      RootPattern.current = RootPattern.default
      root_scope_flat = false
    else
      root_scope_flat = true
      RootPattern.current = RootPattern.more
    end
    M.set_root()
  end
  print("Set Toogl FN")
  M.toggle_set_root_scope = fn
  fn()
end


return M
