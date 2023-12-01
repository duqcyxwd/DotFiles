-- local line = "\"goolord/alpha-nvim\", --                      | Home Page: a fast and fully programmable greeter"
-- line = line:gsub(", ", " ", 1)
-- print(line)

-- Define a class called "Person"
-- Toggle = {}

-- -- Constructor for the Person class
-- function Toggle:new(opts)
--     local obj = {}
--     setmetatable(obj, self)
--     self.__index = self
--     obj.name = opts.name
--     obj.age = opts.age
--     return obj
-- end

-- -- Method to greet a person
-- function Toggle:toggle()
--     print("Hello, my name is " .. self.name .. " and I am " .. self.age .. " years old.")
-- end

-- -- Create instances of the Person class
-- local person1 = Toggle:new({name = "Alice", age = 30})
-- local person2 = Toggle:new({name = "Alice", age = 25})

-- -- Call the greet method on the instances
-- person1:toggle()
-- person2:toggle()

return {

  "nvim-lua/plenary.nvim", --             | Lua fns
  "benmills/vimux",

  -- " Useage: ColorHighlight
  "chrisbra/Colorizer", --               | View log file :ColorToggle  TEST: r! exa --color=always --icons -l,

  {
    "m-demare/hlargs.nvim",
    config = true,
    event = "VeryLazy",
  },
  {
    "desdic/agrolens.nvim",
    enabled = false,
    event = "VeryLazy",
    -- :Telescope agrolens query=functions buffers=all
    dependencies = { "nvim-telescope/telescope.nvim" },
    config = function()
      require "telescope".load_extension("agrolens")
    end
  },

  -- { 'Bekaboo/dropbar.nvim' }   -- Wait for neovim 10

  "kevinhwang91/nvim-bqf",
  "kiyoon/tmuxsend.vim",
  {
    'yaocccc/nvim-foldsign',
    enabled = false,
    event = 'CursorHold',
    config = function ()
      require('nvim-foldsign').setup({
        offset = -2,
        foldsigns = {
          open = '',
          close = '+',
          seps = { '', '' },
        }
      })
    end
  },

  {
    "RRethy/nvim-treesitter-textsubjects",
    -- not a big fan
    enabled = false,
    event = "VeryLazy",
    config = function ()
      require('nvim-treesitter.configs').setup {
        textsubjects = {
          enable = true,
          prev_selection = ',', -- (Optional) keymap to select the previous selection
          keymaps = {
            ['V'] = 'textsubjects-smart',
            -- ['v'] = 'textsubjects-container-outer',
            -- ['i;'] = 'textsubjects-container-inner',
            ['i;'] = { 'textsubjects-container-inner', desc = "Select inside containers (classes, functions, etc.)" },
          },
        },
      }
    end
  },
  {
    "drybalka/tree-climber.nvim",
    enabled = false,
    event = "VeryLazy",
    config = function ()
      -- require'tree-climber'.setup()
      local keyopts = { noremap = true, silent = true }
      vim.keymap.set({'n', 'v', 'o'}, 'H', require('tree-climber').goto_parent, keyopts)
      vim.keymap.set({'n', 'v', 'o'}, 'L', require('tree-climber').goto_child, keyopts)
      vim.keymap.set({'n', 'v', 'o'}, 'J', require('tree-climber').goto_next, keyopts)
      vim.keymap.set({'n', 'v', 'o'}, 'K', require('tree-climber').goto_prev, keyopts)
      vim.keymap.set({'v', 'o'}, 'in', require('tree-climber').select_node, keyopts)
      vim.keymap.set('n', '<c-k>', require('tree-climber').swap_prev, keyopts)
      vim.keymap.set('n', '<c-j>', require('tree-climber').swap_next, keyopts)
      vim.keymap.set('n', '<c-h>', require('tree-climber').highlight_node, keyopts)
    end

  },

}
