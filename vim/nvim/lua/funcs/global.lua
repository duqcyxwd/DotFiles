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

R = function(module)
	modules[module] = true
	return require(module)
end

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

LoadAllPlug = function(dir)
	for filename in io.popen('ls -pUqAL "$XDG_CONFIG_HOME/nvim/lua/' .. dir .. '"'):lines() do
		filename = filename:match("^(.*)%.lua$")
		if filename then
			-- print("loading: " .. dir .. "." .. filename)
			-- require(dir .. "." .. filename)
			R(dir .. "." .. filename)
		end
	end
end
