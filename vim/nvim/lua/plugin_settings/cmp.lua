-- Setup nvim-cmp.
require("funcs.global")

local has_plugin, cmp = pcall(require, "cmp")
if not has_plugin then
  return
end

local has_luasnip, luasnip = pcall(require, "luasnip")
if not has_luasnip then
  return
end

local lspkind = require("lspkind")
local neogen = require("neogen")
local str = require("cmp.utils.str")
local types = require("cmp.types")
local fmt = string.format

local t = function(str)
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

-- WIP? NEED IT?
-- require("luasnip.loaders.from_vscode").load()
-- require'luasnip'.filetype_extend("ruby", {"rails"})
-- https://github.com/L3MON4D3/LuaSnip/blob/b5a72f1fbde545be101fcd10b70bcd51ea4367de/Examples/snippets.lua#L501

require("luasnip.loaders.from_vscode").load()                                                -- Load friendly-snippets
require("luasnip.loaders.from_vscode").load({ paths = { "~/duqcyxwd/DotFiles/snippets/" } }) -- Load snippets from my-snippets folder
require("luasnip.loaders.from_snipmate").load({ paths = { "~/github/vim-snippets/snippets/" }, include = { "c", "vim" }, })


-- local cmp = SR("cmp")
cmp.setup({
  autocomplete = false,
  experimental = {
    -- WIP
    native_menu = false,
    ghost_text = {
      hl_group = "Nontext",
    },
  },
  formatting = {
    format = lspkind.cmp_format({
      -- https://github.com/onsails/lspkind-nvim
      menu = {
        nvim_lua = "[Lua]",
        nvim_lsp = "[LSP]",
        path = "[Path]",
        luasnip = "[LuaSnip]",
        buffer = "[Buffer]",
        cmp_git = "[Git]",
      },
      -- options: 'text', 'text_symbol', 'symbol_text', 'symbol'
      mode = "symbol", -- show only symbol annotations
      maxwidth = 80, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)

      -- The function below will be called before any actual modifications from lspkind
      -- so that you can provide more controls on popup customization. (See [#30](https://github.com/onsails/lspkind-nvim/pull/30))
      preset = "codicons",
      before = function(entry, vim_item)
        return vim_item
      end,
      experimental = {
        ghost_text = true,
      },
      symbol_map = {
        Text = "",
        Method = "",
        Function = "",
        Constructor = "",
        Field = "ﰠ",
        Variable = "",
        Class = "ﴯ",
        Interface = "",
        Module = "",
        Property = "ﰠ",
        Unit = "塞",
        Value = "",
        Enum = "",
        Keyword = "",
        Snippet = "",
        Color = "",
        File = "",
        Reference = "",
        Folder = "",
        EnumMember = "",
        Constant = "",
        Struct = "פּ",
        Event = "",
        Operator = "",
        TypeParameter = "",
      },
    }),
  },
  completion = { border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" }, scrollbar = "║" },
  documentation = {
    border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
    scrollbar = "║",
  },

  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      require("luasnip").lsp_expand(args.body) -- For `luasnip` users.
      -- vim.fn["vsnip#anonymous"](args.body)        -- For `vsnip` users.
      -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
      -- vim.fn["UltiSnips#Anon"](args.body)         -- For `ultisnips` users.
    end,
  },
  mapping = {
    ["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
    ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),

    ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
    -- ["<C-y>"] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
    ["<C-e>"] = cmp.mapping({ i = cmp.mapping.abort(), c = cmp.mapping.close() }),
    ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    -- ["<Tab>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      elseif has_words_before() then
        cmp.complete()
      else
        fallback()
      end
    end, { "i", "s" }),

    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),

    ["<C-k>"] = cmp.mapping(function(_)
      luasnip.expand_or_jump()
    end, { "i", "s" }),
    -- ["<C-n>"] = cmp.mapping(function(_) cmp.select_next_item() end, { "i", "s" }),
    -- ["<C-p>"] = cmp.mapping(function(_) cmp.select_prev_item() end, { "i", "s" }),
    ["<C-p>"] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }), { "i", "s", "c" }),
    ["<C-n>"] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }), { "i", "s", "c" }),

    ["<C-l>"] = cmp.mapping(function(fallback)
      if luasnip.expand_or_jumpable() then
        vim.fn.feedkeys(t("<Plug>luasnip-expand-or-jump"), "")
      elseif neogen.jumpable() then
        vim.fn.feedkeys(t("<cmd>lua require('neogen').jump_next()<CR>"), "")
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<C-h>"] = cmp.mapping(function(fallback)
      if luasnip.jumpable(-1) then
        vim.fn.feedkeys(t("<Plug>luasnip-jump-prev"), "")
      else
        fallback()
      end
    end, { "i", "s", }),


  },
  sources = cmp.config.sources({

    -- { name = "vsnip" },     -- For vsnip users.
    -- { name = 'ultisnips' }, -- For ultisnips users.
    -- { name = 'snippy' },    -- For snippy users.

    { name = "luasnip" }, -- For luasnip users.
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "buffer", keyword_length = 5, max_item_count = 5 },
  }, {
    { name = "buffer" },
  }),
})

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
