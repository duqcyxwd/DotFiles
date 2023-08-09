require("funcs.global")
local core = require("funcs.nvim_core")
local lsp_util = require("config.lspconfig-util")

return {
  --" https://medium.com/prodhacker/how-to-set-up-neovim-0-5-modern-plugins-lsp-treesitter-etc-542c3d9c9887
  { -- "neovim/nvim-lspconfig", | Configs for the Nvim LSP client
    "neovim/nvim-lspconfig",
    config = function()
      local lspconfig = SR("lspconfig")
      local servers = { "pyright", "tsserver", "clojure_lsp" }
      for _, lsp in pairs(servers) do
        local not_created = not lspconfig[lsp].commands_created
        if not_created then
          lspconfig[lsp].setup({
            on_attach = lsp_util.lsp_on_attach_power,
            handlers = lsp_util.handlers,
            capabilities = lsp_util.capabilitiesFn(),
          })
        end
      end

      lspconfig.lua_ls.setup({
        on_attach = lsp_util.lsp_on_attach_power,
        on_init = function(client)
          local path = client.workspace_folders[1].name
          if not vim.loop.fs_stat(path..'/.luarc.json') and not vim.loop.fs_stat(path..'/.luarc.jsonc') then
            client.config.settings = vim.tbl_deep_extend('force', client.config.settings.Lua, {
              runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = 'LuaJIT'
              },
              -- Make the server aware of Neovim runtime files
              workspace = {
                library = { vim.env.VIMRUNTIME }
                -- or pull in all of 'runtimepath'. NOTE: this is a lot slower
                -- library = vim.api.nvim_get_runtime_file("", true)
              }
            })

            client.notify("workspace/didChangeConfiguration", { settings = client.config.settings })
          end
          return true
        end,
      })

      lspconfig.diagnosticls.setup({
        on_attach = lsp_util.lsp_on_attach,
        capabilities = lsp_util.capabilitiesFn(),
        cmd = { "diagnostic-languageserver", "--stdio" },
        filetypes = {
          "css",
          "javascript",
          "javascriptreact",
          "less",
          "markdown",
          "pandoc",
          "sh",
          "typescript",
          "typescriptreact",
          "vim",
          -- "lua",
          "xml",
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
              formatPattern = { "[^:]+:(\\d+):(\\d+):\\s*(.*)(\\r|\\n)*$", { line = 1, column = 2, message = 3 } },
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
            -- Stylua is already configured
            -- stylua = {
            --   command = "stylua",
            --   isStdout = true,
            --   doesWriteToFile = false,
            --   -- args = {"--config-path $XDG_CONFIG_HOME/stylua/stylua.toml", "%file" },
            --   args = { "--search-parent-directories", "%file" },
            -- },
            xmllint = {
              command = "xmllint",
              isStdout = true,
              doesWriteToFile = false,
              args = { "--format", "%file" },
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
            lua = "stylua",
            xml = "xmllint",
          },
        },
      })
    end,
  },
  { -- "tami5/lspsaga.nvim",
    "tami5/lspsaga.nvim",
    init = function()
      core.nvim_create_augroups({
        lspsagaHover = {
          { "FileType", "LspsagaHover", "nmap <buffer> q :q<CR>" },
          { "FileType", "LspsagaHover", "nmap <buffer> <esc> :q<CR>" }
        }
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
}
