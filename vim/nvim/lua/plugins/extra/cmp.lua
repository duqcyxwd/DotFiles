require("funcs.global")

local build_cmp_mapping = function(cmp, luasnip, neogen, has_neogen)
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

  local map = {

    ["<C-k>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }), -- | scroll docs
    ["<C-j>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }), -- | scroll docs

    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-c>"] = cmp.mapping.abort(),
    ["<C-e>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.

    ["<Tab>"] = cmp.mapping(next_item, { "i", "s" }),
    ["<s-Tab>"] = cmp.mapping(prev_item, { "i", "s" }),

    ["<C-l>"] = cmp.mapping(next_parameter, { "i", "s" }),
    ["<C-h>"] = cmp.mapping(prev_parameter, { "i", "s" }),
    ["<C-n>"] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ["<C-p>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  }
  return map
end

return {
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    lazy = true,
    dependencies = {
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "onsails/lspkind-nvim", --        | Add icon for cmp

      --  For luasnip users.
      "L3MON4D3/LuaSnip", --             | Follows lsp protocol
      "saadparwaiz1/cmp_luasnip", --     |
      "rafamadriz/friendly-snippets", -- | Snippet collection
    },
    opts = function()
      vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })
      local cmp = require("cmp")
      local defaults = require("cmp.config.default")()
      local lspkind = require("lspkind")
      local types = require("cmp.types")
      local str = require("cmp.utils.str")

      return {
        sources = cmp.config.sources({
          { name = 'luasnip', option = { show_autosnippets = true } },
          { name = "nvim_lsp" },
          { name = "buffer" },
          { name = "path" },
        }),

        completion = {
          completeopt = "menu,menuone,noinsert",
          autocomplete = false,
        },
        snippet = {
          expand = function(args)
            require("luasnip").lsp_expand(args.body)
          end,
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
            -- before = function(entry, vim_item)
            --   -- Get the full snippet (and only keep first line)
            --   local word = entry:get_insert_text()
            --   if entry.completion_item.insertTextFormat == types.lsp.InsertTextFormat.Snippet then
            --     word = vim.lsp.util.parse_snippet(word)
            --   end
            --   word = str.oneline(word)

            --   -- concatenates the string
            --   -- local max = 50
            --   -- if string.len(word) >= max then
            --   -- 	local before = string.sub(word, 1, math.floor((max - 3) / 2))
            --   -- 	word = before .. "..."
            --   -- end

            --   if entry.completion_item.insertTextFormat == types.lsp.InsertTextFormat.Snippet
            --       and string.sub(vim_item.abbr, -1, -1) == "~"
            --   then
            --     word = word .. "~"
            --   end
            --   vim_item.abbr = word

            --   return vim_item
            -- end,
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
        sorting = defaults.sorting,
        experimental = {
          ghost_text = {
            hl_group = 'Nontext',
          },
        }
      }
    end,
    config = function(nvim_cmp, opts)
      local cmp = require("cmp")
      local luasnip = require("luasnip")
      local has_neogen, neogen = pcall(require, "neogen")

      opts.mapping = cmp.mapping.preset.insert(build_cmp_mapping(cmp, luasnip, neogen, has_neogen))

      cmp.setup(opts)

      require("luasnip.loaders.from_vscode").load() --                                                | Load friendly-snippets
      require("luasnip.loaders.from_vscode").load({ paths = { "~/duqcyxwd/DotFiles/snippets/" } }) -- | Load snippets from my-snippets folder
      require("luasnip.loaders.from_snipmate").load({
        paths = { "~/github/vim-snippets/snippets/" },
        include = { "c", "vim", "lua" },
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
    end,
  },
}