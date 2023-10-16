-- A place hold module to hold all plugins functions
local M = {}

M.toggleExample = function()
  local m = {
    enabled = false,
    config = function(flag)
      if flag then
        print("enabled")
      else
        print("disabled")
      end
    end

  }

  m.toggle = function()
    m.enabled = not m.enabled
    m.config(m.enabled)
  end

  m.config(m.enabled)
  -- require("funcs.plug").plugName = m
end


return M
