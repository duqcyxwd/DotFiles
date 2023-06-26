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
local has_neogen, neogen = pcall(require, "neogen")

local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local has_words_before = function()
  local line, col = unpack(vim.api.nvim_win_get_cursor(0))
  return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end

local next_item = function(fallback)
  if cmp.visible() then
    cmp.select_next_item()
  elseif luasnip.expand_or_jumpable() then
    luasnip.expand_or_jump()
  elseif has_words_before() then
    cmp.complete()
  else
    fallback()
  end
end

local prev_item = function(fallback)
  if cmp.visible() then
    cmp.select_prev_item()
  elseif luasnip.jumpable(-1) then
    luasnip.jump(-1)
  else
    fallback()
  end
end


local next_parameter = function(fallback)
  if luasnip.expand_or_jumpable() then
    vim.fn.feedkeys(t("<Plug>luasnip-expand-or-jump"), "")
  elseif has_neogen and neogen.jumpable() then
    vim.fn.feedkeys(t("<cmd>lua require('neogen').jump_next()<CR>"), "")
  else
    fallback()
  end
end
local prev_parameter = function(fallback)
  if luasnip.jumpable(-1) then
    vim.fn.feedkeys(t("<Plug>luasnip-jump-prev"), "")
  else
    fallback()
  end
end


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
    ["<C-e>"] = cmp.mapping({ i = cmp.mapping.abort(), c = cmp.mapping.close() }),
    ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    ["<Tab>"] = cmp.mapping(next_item, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(prev_item, { "i", "s" }),
    ["<C-k>"] = cmp.mapping(function(_) luasnip.expand_or_jump() end, { "i", "s" }),
    ["<C-p>"] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }), { "i", "s", "c" }),
    ["<C-n>"] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }), { "i", "s", "c" }),

    ["<C-l>"] = cmp.mapping(next_parameter, { "i", "s" }),
    ["<C-h>"] = cmp.mapping(prev_parameter, { "i", "s", }),
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



-- documentation is replaced with window.documentation = cmp.config.window.bordered()
  -- documentation = {
  --   border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
  --   scrollbar = "║",
  -- },
