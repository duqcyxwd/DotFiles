-- A place hold module to hold all plugins functions
local M = {}

M.toggleExample = function()
  local m
  m = {
    enabled = false,
    config = function(flag)
      if flag then
        print("enabled")
      else
        print("disabled")
      end
    end,
    toggle = function()
      m.enabled = not m.enabled
      m.config(m.enabled)
    end
  }
  m.config(m.enabled)
  -- require("funcs.plug").plugName = m
end

M.toggleObjExample = function ()
  local plugfn = require("funcs.plug")
  plugfn.lsp_lines = {
    enabled = false,
    config = function (self, flag)
      if flag or self.enabled then
        print("enabled")
      else
        print("disabled")
      end
    end,
    toggle = function(self)
      self.enabled = not self.enabled
      self:config()
    end,
    setup = function (self) self:config() end
  }

  plugfn.lsp_lines:setup()
end

return M
