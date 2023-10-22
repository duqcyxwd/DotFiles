require("lua.global")
local core = require("funcs.nvim_core")
local lsp_util = require("config.lspconfig-util")

local diagnosticls = {
  filetypes = {
    "css",
    "javascript",
    "javascriptreact",
    -- "less",
    -- "markdown",
    "pandoc",
    "sh",
    "typescript",
    "typescriptreact",
    "vim",
    "xml",
    -- "lua",
  },
  init_options = {
    linters = {
      shellcheck = {
        command = "shellcheck",
        debounce = 100,
        args = { "--format", "json", "-" },
        sourceName = "shellcheck",
        parseJson = {
          line = "line",
          column = "column",
          endLine = "endLine",
          endColumn = "endColumn",
          message = "${message} [${code}]",
          security = "level",
        },
        securities = {
          error = "error",
          warning = "warning",
          info = "info",
          style = "hint",
        },
      },
      eslint = {
        command = "eslint_d",
        rootPatterns = { ".git" },
        debounce = 100,
        args = { "--stdin", "--stdin-filename", "%filepath", "--format", "json" },
        sourceName = "eslint_d",
        parseJson = {
          errorsRoot = "[0].messages",
          line = "line",
          column = "column",
          endLine = "endLine",
          endColumn = "endColumn",
          message = "[eslint] ${message} [${ruleId}]",
          security = "severity",
        },
        securities = {
          [2] = "error",
          [1] = "warning",
        },
      },
      vint = {
        command = "vint",
        debounce = 100,
        args = { "--enable-neovim", "-" },
        offsetLine = 0,
        offsetColumn = 0,
        sourceName = "vint",
        formatLines = 1,
        formatPattern = {
          "[^:]+:(\\d+):(\\d+):\\s*(.*)(\\r           | \\n)*$",
          { line = 1, column = 2, message = 3 },
        },
      },
      zsh = {
        command = "zsh",
        args = { "-n", "%file" },
        isStdout = false,
        isStderr = true,
        sourceName = "zsh",
        formatLines = 1,
        formatPattern = { "^[^:]+:(\\d+):\\s+(.*)$", { line = 1, message = 2 } },
      },
    },
    filetypes = {
      javascript = "eslint",
      javascriptreact = "eslint",
      typescript = "eslint",
      typescriptreact = "eslint",
      sh = "shellcheck",
      vim = "vint",
    },
    formatters = {
      eslint_d = {
        command = "eslint_d",
        args = { "--stdin", "--stdin-filename", "%filename", "--fix-to-stdout" },
        rootPatterns = { ".git" },
      },
      prettier = {
        command = "prettier",
        args = { "--stdin-filepath", "%filename" },
      },
      shfmt = {
        command = "shfmt",
        args = { "-i", "2", "-bn", "-ci", "-sr" },
      },
      xmllint = {
        command = "xmllint",
        isStdout = true,
        doesWriteToFile = false,
        args = { "--format", "%file" },
      },
      stylua = {
        command = "stylua",
        isStdout = false,
        doesWriteToFile = true,
        args = { "--search-parent-directories", "%file" },
      },
    },
    formatFiletypes = {
      sh = "shfmt",
      css = "prettier",
      javascript = "eslint_d",
      javascriptreact = "eslint_d",
      scss = "prettier",
      less = "prettier",
      typescript = "eslint_d",
      typescriptreact = "eslint_d",
      json = "prettier",
      markdown = "prettier",
      xml = "xmllint",
      lua = "stylua",
    },
  },
}

local lua_ls = {
  on_attach = function(client, bufnr)
    -- require("lsp-format").on_attach(client, bufnr) -- Enabled format on save

    -- local fn = function()
    --   vim.cmd([[mkview]])
    --   vim.cmd([[!stylua --search-parent-directories %]])
    --   vim.cmd([[loadview]])
    --   return;
    -- end
    -- vim.keymap.set("n", "<localleader>f", fn, { silent = true, buffer = true, desc = "Local customized formater"})
  end,
  settings = {
    Lua = {
      format = {
        enable = true,
        defaultConfig = {
          indent_style = "space",
          indent_size = "2",
          column_width = 200
        },
      },
      runtime = {
        version = "LuaJIT",
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        checkThirdParty = false,
        library = {
          vim.env.VIMRUNTIME,
          -- "${3rd}/luv/library"
          -- "${3rd}/busted/library",
        },
      },
      completion = {
        callSnippet = "Replace",
      },
    },
  }
}
local servers = {
  -- clangd = {},
  -- gopls = {},
  -- pyright = {},
  -- rust_analyzer = {},
  -- tsserver = {},
  -- html = { filetypes = { 'html', 'twig', 'hbs'} },

  diagnosticls = diagnosticls,
  lua_ls = lua_ls,
  jsonls = {}

  -- efm = {
  --   init_options = {documentFormatting = true},
  --   settings = {
  --     rootMarkers = { ".git/", "stylua.toml" },
  --     languages = {
  --       lua = {
  --         -- { formatCommand = "lua-format -i", formatStdin = true }
  --         { formatCommand = "stylua --search-parent-directories ${INPUT}", formatStdin = false }
  --       }
  --     }
  --   }, -- You must populate this according to the EFM readme
  --   filetypes = { 'lua' }
  -- },
}

return {
  ------------------------------------------------------------------------- |
  {
    "folke/neodev.nvim", --                                                 | Automatically configures lua-language-server
    event = "VeryLazy",
    opts = {},
  },
  {
    "neovim/nvim-lspconfig", --                                             | Configs for the Nvim LSP client
    event = { "BufReadPre", "BufNewFile" },
    -- event = "VeryLazy",
    dependencies = {
      { "folke/neoconf.nvim", cmd = "Neoconf", config = false, dependencies = { "nvim-lspconfig" } },
      { "folke/neodev.nvim",  opts = {} },
      {
        "hrsh7th/cmp-nvim-lsp",
        cond = function()
          return require("funcs.nvim_utility").has("nvim-cmp")
        end,
      },
    },
    config = function()
      IfHasModule('neodev', function(neodev)
        neodev.setup()
      end)

      local lspconfig = SR("lspconfig")

      for lsp, setup in pairs(servers) do
        setup.capabilities = lsp_util.capabilitiesFn()
        setup.handlers = lsp_util.handlers
        lspconfig[lsp].setup(setup)
      end
    end,
  },
  {
    "hinell/lsp-timeout.nvim",
    event = 'LspAttach',
    config = function()
      vim.g["lsp-timeout-config"] = {
        stopTimeout  = 1000 * 60 * 5, -- wait ms before stopping all LSP servers
        startTimeout = 1000 * 10,     -- ms before restart
        silent       = false          -- true to suppress notifications
      }
    end,
    dependencies = { "neovim/nvim-lspconfig" }
  },

  -- "lukas-reineke/lsp-format.nvim", -- Provide format on save

  {
    "tami5/lspsaga.nvim", --                                                | LSP UI
    -- Nice wiki:                                                           | https://github.com/kkharji/lspsaga.nvim/wiki
    name = "old_lspsaga",
    enabled = false,
    init = function()
      core.nvim_create_augroups({
        lspsagaHover = {
          { "FileType", "LspsagaHover", "nmap <buffer> q :q<CR>" },
          { "FileType", "LspsagaHover", "nmap <buffer> <esc> :q<CR>" },
        },
      })
    end,
    config = function()
      R("lspsaga").setup({ -- defaults ...
        debug = false,
        use_saga_diagnostic_sign = true,
        -- diagnostic sign
        error_sign = "",
        warn_sign = "",
        hint_sign = "",
        infor_sign = "",
        diagnostic_header_icon = "   ",
        -- code action title icon
        -- code_action_icon = " ",
        code_action_prompt = {
          enable = true,
          sign = false,
          sign_priority = 40,
          virtual_text = true,
        },
        finder_definition_icon = "  ",
        finder_reference_icon = "  ",
        max_preview_lines = 10,
        finder_action_keys = {
          open = "o",
          vsplit = "s",
          split = "i",
          quit = "q",
          scroll_down = "<C-f>",
          scroll_up = "<C-b>",
        },
        code_action_keys = {
          quit = "q",
          exec = "<CR>",
        },
        rename_action_keys = {
          quit = "<C-c>",
          exec = "<CR>",
        },
        definition_preview_icon = "  ",
        border_style = "single",
        rename_prompt_prefix = "➤",
        rename_output_qflist = {
          enable = false,
          auto_open_qflist = false,
        },
        server_filetype_map = {},
        diagnostic_prefix_format = "%d. ",
        diagnostic_message_format = "%m %c",
        highlight_prefix = false,
      })
    end,
  },
  {
    'nvimdev/lspsaga.nvim',
    enabled = true,
    event = 'LspAttach',
    config = function()
      require('lspsaga').setup({
        ui = {
          code_action = ' ',
          kind = {
            Namespace = { ' ', 'Include' },
            Package = { ' ', 'Label' },
            Class = { ' ', 'Include' },
            Method = { ' ', 'Function' },
            Property = { ' ', '@property' },
            Field = { ' ', '@field' },
            Enum = { ' ', '@number' },
            Interface = { ' ', 'Type' },
            Function = { '󰡱 ', 'Function' },
            Variable = { ' ', '@variable' },
            Constant = { ' ', 'Constant' },
            String = { '󰅳 ', 'String' },
            Number = { '󰎠 ', 'Number' },
            Boolean = { ' ', 'Boolean' },
            Array = { '󰅨 ', 'Type' },
            Object = { ' ', 'Type' },
            Key = { ' ', 'Constant' },
            Null = { '󰟢 ', 'Constant' },
            EnumMember = { ' ', 'Number' },
            Struct = { ' ', 'Type' },
            Event = { ' ', 'Constant' },
            Operator = { ' ', 'Operator' },
            TypeParameter = { ' ', 'Type' },
          },
        },
        symbol_in_winbar = {
          enable = true,
          dely = 1000,
        },
        lightbulb = {
          enable = true,
          sign = false,
          virtual_text = true,
        },
        diagnostic = {
          virtual_text = true
        },
      })
    end,
    dependencies = {
      enabled = false,
      'nvim-treesitter/nvim-treesitter', -- optional
      'nvim-tree/nvim-web-devicons'      -- optional
    }
  },
  {
    'weilbith/nvim-code-action-menu',
    enabled = true,
    lazy = true,
    cmd = { 'CodeActionMenu' },
  },
  {
    'Wansmer/symbol-usage.nvim', --                                         | Show usage symbol
    enabled = true,
    event = 'LspAttach',
    config = function()
      require('symbol-usage').setup({
        vt_position = 'end_of_line',
        text_format = function(symbol)
          local fragments = {}

          -- Modified so only show usages if it is used
          -- Modified so that usages are only displayed if they are actually being used.

          if symbol.references > 0 then
            local usage = symbol.references <= 1 and 'usage' or 'usages'
            local num = symbol.references == 0 and 'no' or symbol.references
            table.insert(fragments, ('%s %s'):format(num, usage))
          end

          if symbol.definition then
            table.insert(fragments, symbol.definition .. ' defs')
          end

          if symbol.implementation then
            table.insert(fragments, symbol.implementation .. ' impls')
          end

          return '  ' .. table.concat(fragments, ', ')
        end,
      })
    end
  },
  {
    "Maan2003/lsp_lines.nvim", --                                           | renders diagnostics using virtual lines on top of the real line of code
    event = 'LspAttach',
    lazy = true,
    config = function()
      require("lsp_lines").setup()

      local m = {
        enabled = false,
        config = function (self, flag)
          if flag or self.enabled then
            vim.diagnostic.config({ virtual_lines = { only_current_line = true } })
          else
            vim.diagnostic.config({ virtual_lines = false })
          end
        end,
        toggle = function(self)
          self.enabled = not self.enabled
          self:config()
        end,
        setup = function (self) self:config() end
      }

      m.run(m.enabled)
      require("funcs.plug").lsp_lines = m
    end,
  },

}
