require("funcs.global")
SR("lualine").setup({
  options = {
    icons_enabled = true,
    theme = "auto",
    component_separators = { left = "", right = "" },
    section_separators = { left = "", right = "" },
    disabled_filetypes = {},
    always_divide_middle = true,

    show_bufnr = true, -- this appends [bufnr] to buffer section,
    modified_icon = "+ ", -- change the default modified icon
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch", "diff", "diagnostics" },
    lualine_c = {  "lsp_progress", "filename",},
    lualine_x = {},
    lualine_y = { "encoding", "fileformat", "filetype" },
    lualine_z = { "progress", "location" },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = { },
    lualine_c = { "filename", "lsp_progress" },
    lualine_x = {},
    lualine_y = {  },
    lualine_z = { "progress" },
  },
  tabline = {},
  extensions = {},
})

SR("bufferline").setup({
  -- akinsho/bufferline.nvim
  options = {
    numbers = function(opts)
      return string.format('%s.', opts.ordinal)
    end,
    diagnostics = "none",
    offsets = { { filetype = "coc-explorer" } },
    show_tab_indicators = true,
    enforce_regular_tabs = true,
    always_show_bufferline = false,
    -- separator_style = "slant" | "thick" | "thin" | { 'any', 'any' },
    separator_style = "slant",
    max_prefix_length = 10,
    max_name_length = 30,
    tab_size = 25,
    show_buffer_close_icons = false,
    show_buffer_icons = false,

    middle_mouse_command = "bdelete! %d",
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
