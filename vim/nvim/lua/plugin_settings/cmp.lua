-- Setup nvim-cmp.
local M = {}


M.config = function()
  local has_plugin, cmp = pcall(require, "cmp")
  if not has_plugin then
    return
  end

  local has_plugin, luasnip = pcall(require, "luasnip")
  if not has_plugin then
    return
  end

  local has_words_before = function()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
  end

  -- WIP? NEED IT?
  -- require("luasnip.loaders.from_vscode").load()
  -- require'luasnip'.filetype_extend("ruby", {"rails"})
  -- https://github.com/L3MON4D3/LuaSnip/blob/b5a72f1fbde545be101fcd10b70bcd51ea4367de/Examples/snippets.lua#L501
  -- require("luasnip.loaders.from_vscode").load({ paths = { "./my-snippets" } }) -- Load snippets from my-snippets folder

  require("luasnip.loaders.from_vscode").load()
  require("luasnip.loaders.from_vscode").load({ paths = { "~/duqcyxwd/DotFiles/snippets/" } }) -- Load snippets from my-snippets folder
  -- require("luasnip.loaders.from_snipmate").load({ paths = {"/Users/EYONDUU/github/ultisnips-snippets" }})
  require("luasnip.loaders.from_snipmate").load({
    -- paths = { "/Users/EYONDUU/github/ultisnips-snippets/vim.snippets" },
    paths = { "/Users/EYONDUU/github/vim-snippets/snippets/" },
    include = { "c", "vim" },
  })

  -- local cmp = SR("cmp")
  cmp.setup({
    autocomplete = false,
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
        require("luasnip").lsp_expand(args.body) -- For `luasnip` users.
        -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
        -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    mapping = {
      ["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
      ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),

      ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
      -- ["<C-y>"] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
      ["<C-e>"] = cmp.mapping({
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      }),
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
      ["<C-n>"] = cmp.mapping(function(_)
        cmp.select_next_item()
      end, { "i", "s" }),
      ["<C-p>"] = cmp.mapping(function(_)
        cmp.select_prev_item()
      end, { "i", "s" }),
    },
    sources = cmp.config.sources({
      -- { name = "vsnip" }, -- For vsnip users.
      { name = "luasnip" }, -- For luasnip users.
      -- { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
      { name = "nvim_lsp" },
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
end

M.config()
return M
