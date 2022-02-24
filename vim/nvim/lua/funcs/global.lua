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

  local status, _ = pcall(require, module)
  if not status then
    vim.notify("couldn't load " .. module)
  end
  return require(module)
end

-- Reload Require
RE = function(module)
  package.loaded[module] = nil
  return require(module)
end

IfHas = function(plugin_name, callback, default)
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
  config = function(_) end,
  update_capabilities = function(_) end,
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
R_FOLD = function(dir)
  for filename in io.popen('ls -pUqAL "$XDG_CONFIG_HOME/nvim/lua/' .. dir .. '"'):lines() do
    filename = filename:match("^(.*)%.lua$")
    if filename then
      RE(dir .. "." .. filename)
    end
  end
end
