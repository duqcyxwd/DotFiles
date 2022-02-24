require("funcs.global")

SR("hop").setup()
SR("lualine").setup({
  options = {
    icons_enabled = true,
    theme = "auto",
    component_separators = { left = "", right = "" },
    section_separators = { left = "", right = "" },
    disabled_filetypes = {},
    always_divide_middle = true,
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch", "diff", "diagnostics" },
    lualine_c = { "filename" },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress" },
    lualine_z = { "location" },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { "filename" },
    lualine_x = {},
    lualine_y = { "encoding", "fileformat", "filetype" },
    lualine_z = { "location" },
  },
  tabline = {},
  extensions = {},
})

SR("bufferline").setup({
  options = {
    numbers = "ordinal",
    diagnostics = "none",
    offsets = { { filetype = "coc-explorer" } },
    show_tab_indicators = true,
    enforce_regular_tabs = true,
    always_show_bufferline = true,
    separator_style = "slant",
    custom_filter = function(buf_number, buf_numbers)
      -- -- filter out filetypes you don't want to see
      -- if vim.bo[buf_number].filetype ~= "<i-dont-want-to-see-this>" then
      --   return true
      -- end
      -- -- filter out by buffer name
      -- if vim.fn.bufname(buf_number) ~= "<buffer-name-I-dont-want>" then
      --   return true
      -- end
      return true
    end,
  },
})

SR("twilight").setup({
  dimming = {
    alpha = 0.25, -- amount of dimming
    -- we try to get the foreground from the highlight groups or fallback color
    color = { "Normal", "#ffffff" },
    inactive = false, -- when true, other windows will be fully dimmed (unless they contain the same buffer)
  },
  context = 10, -- amount of lines we will try to show around the current line
  sitter = true, -- use treesitter when available for the filetype
  -- treesitter is used to automatically expand the visible text,
  -- but you can further control the types of nodes that should always be fully expanded
  expand = { -- for treesitter, we we always try to expand to the top-most ancestor with these types
    "function",
    "method",
    "table",
    "if_statement",
  },
  exclude = {}, -- exclude these filetypes
})

