require("funcs.utility")
local u = require("funcs.utility")

P = function(thing)
  print(vim.inspect(thing))
  return thing
end

local modules = {}

UnloadModules = function()
  for module, _ in pairs(modules) do
    package.loaded[module] = nil
  end
end

-- Require
R = function(module)
  modules[module] = true

  local status, error = pcall(require, module)
  if not status then
    print(error)
    vim.notify("couldn't load " .. module)
  end
  return require(module)
end

R_VIM = function(vimrc)
  vim.cmd.source(vimrc)
end

-- Reload Require
RE = function(module)
  package.loaded[module] = nil
  return require(module)
end

IfHasModule = function(plugin_name, callback, default)
  local has_plugin, plugin = pcall(require, plugin_name)
  if has_plugin then
    return callback(plugin)
  else
    if default ~= nil then
      return default()
    end
  end
end

local empty_module = {
  load = function(_) end,
  setup = function(_) end,
  init = function(_) end,
  config = function(_) end,
  default_capabilities = function(_) end,
}

-- Safe Require
SR = function(plugin_name)
  local has_plugin, plugin = pcall(require, plugin_name)
  if has_plugin then
    return plugin
  else
    return empty_module
  end
end

-- Disable Require
DR = function(_)
  return empty_module
end

-- Require all modules from folder
R_FOLD = function(dir, ignore)
  ignore = ignore or {}
  for filename in io.popen('ls -pUqAL "$XDG_CONFIG_HOME/nvim/lua/' .. dir .. '"'):lines() do
    filename = filename:match("^(.*)%.lua$")
    local is_filter = IS_IN(filename, ignore)
    if is_filter then
      -- print("ignore file: " .. filename)
    end
    if filename and not is_filter then
      RE(dir .. "." .. filename)
    end
  end
end

R_VIM_FOLD = function(dir, ignore)
  ignore = ignore or {}
  for path in io.popen('ls -pUqAL "$XDG_CONFIG_HOME/vim/' .. dir .. '"'):lines() do
    local filename = path:match("^(.*)%.vim$")
    local is_filter = IS_IN(filename, ignore)
    if filename and not is_filter then
      R_VIM("$XDG_CONFIG_HOME/vim/" .. dir .. "/" .. path)
    end
  end
end

LAZY_AUTO = function(dir)
  local configs = {}
  for path in io.popen('ls -pUqAL "$XDG_CONFIG_HOME/nvim/lua/' .. dir .. '"'):lines() do
    local filename = path:match("^(.*)%.lua$")
    if filename then
      configs = u.join(configs, RE(dir .. "." .. filename))
    end
  end
  return configs
end
