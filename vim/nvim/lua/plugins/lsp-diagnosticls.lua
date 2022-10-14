local mapping = require("plugins.buffer-keys")

-- Config {{{1
-- https://github.com/neovim/nvim-lspconfig
require("lspconfig").diagnosticls.setup({
  on_attach = mapping.lsp_on_attach,
  capabilities = SR("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities()),
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
    "lua",
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
      stylua = {
        command = "stylua",
        isStdout = true,
        doesWriteToFile = false,
        -- args = {"--config-path $XDG_CONFIG_HOME/stylua/stylua.toml", "%file" },
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
      lua = "stylua",
    },
  },
})
