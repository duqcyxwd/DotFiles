local has_plugin, cmp = pcall(require, "cmp")
if not has_plugin then
  return
end

-- WIP? NEED IT?
-- require("luasnip.loaders.from_vscode").load()
-- require'luasnip'.filetype_extend("ruby", {"rails"})
-- https://github.com/L3MON4D3/LuaSnip/blob/b5a72f1fbde545be101fcd10b70bcd51ea4367de/Examples/snippets.lua#L501

require("luasnip.loaders.from_vscode").load()                                                -- Load friendly-snippets
require("luasnip.loaders.from_vscode").load({ paths = { "~/duqcyxwd/DotFiles/snippets/" } }) -- Load snippets from my-snippets folder
require("luasnip.loaders.from_snipmate").load({ paths = { "~/github/vim-snippets/snippets/" }, include = { "c", "vim" }, })



-- Set configuration for specific filetype.
cmp.setup.filetype("gitcommit", {
  sources = cmp.config.sources({
    { name = "cmp_git" }, -- You can specify the `cmp_git` source if you were installed it.
  }, {
    { name = "buffer" },
  }),
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline("/", {
  sources = {
    { name = "buffer" },
  },
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(":", {
  sources = cmp.config.sources({
    { name = "path" },
  }, {
    { name = "cmdline" },
  }),
})
