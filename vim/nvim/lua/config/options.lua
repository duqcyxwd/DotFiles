
-- stylua: ignore
-- EasyAlign /--/
local setup = function()

  vim.cmd("filetype plugin on")                                 -- | Enable filetype plugins
  vim.cmd("syntax enable")
  vim.cmd("syntax on")

  vim.opt.termguicolors  = true

  vim.wo.number          = true                                 -- | Set number column
  vim.wo.signcolumn      = 'yes'                                -- | Set sign column
  vim.wo.relativenumber  = true                                 -- | Set relative number
  vim.wo.cursorline      = true                                 -- | Highlight the current line

  vim.opt.autoread       = true                                 -- | Set to auto-read when a file is changed from the outside
  vim.opt.clipboard      = "unnamedplus"                        -- | Use system clipboard
  vim.opt.cmdheight      = 1                                    -- | Give more space for displaying messages.
  vim.opt.encoding       = "utf-8"                              -- | Set utf8 as the standard encoding
  vim.opt.ffs            = "unix,dos,mac"                       -- | Use Unix as the standard file type
  vim.opt.hidden         = true                                 -- | A buffer becomes hidden when it is abandoned
  vim.opt.history        = 1000                                 -- | Sets how many lines of history VIM has to remember
  vim.opt.ignorecase     = true                                 -- | Ignore case when searching
  vim.opt.incsearch      = true                                 -- | Makes search act like search in modern browsers
  vim.opt.langmenu       = "en"                                 -- | Set language menu to English
  vim.opt.laststatus     = 2                                    -- | Always show the status line
  vim.opt.lazyredraw     = false                                -- | Disable lazyredraw for Lazy
  vim.opt.matchtime      = 2                                    -- | How many tenths of a second to blink when matching brackets
  vim.opt.mouse          = "a"                                  -- | Enable mouse support in all modes
  vim.opt.autochdir      = false                                -- | Don't change the directory so we can use project search
  vim.opt.errorbells     = false                                -- | No annoying sound on errors
  vim.opt.joinspaces     = false                                -- | No extra space when joining lines
  vim.opt.ruler          = true                                 -- | Always show the current position
  vim.opt.scroll         = 4                                    -- | Number of lines to scroll with ^U/^D
  vim.opt.scrolloff      = 15                                   -- | Keep cursor away from this many chars top/bottom
  vim.opt.shell          = "zsh"                                -- | Default shell to zsh
  vim.opt.showcmd        = true                                 -- | Show command being entered
  vim.opt.showmatch      = true                                 -- | Show matching brackets when the text indicator is over them
  vim.opt.smartcase      = true                                 -- | When searching, try to be smart about cases
  vim.opt.startofline    = true                                 -- | When "on", the commands listed below move the cursor to the first non-blank of the line.
  vim.opt.timeoutlen     = 500                                  -- | The default timeoutlen is 1000 ms.
  vim.opt.updatetime     = 250                                  -- | The time in milliseconds for updating swap files and the undo file
  vim.opt.visualbell     = true                                 -- | Visual bell instead of audio bell
  vim.opt.wildmenu       = true                                 -- | Turn on the wild menu

                                                                -- " Text Editor Related
  vim.opt.autoindent     = true                                 -- | Set autoindent
  vim.opt.linebreak      = false                                -- | Automatically break lines at 80 characters (disable line break)
  vim.opt.shiftwidth     = 2                                    -- | 1 tab == 2 spaces
  vim.opt.smarttab       = true                                 -- | Be smart when using tabs
  vim.opt.expandtab      = true                                 -- | Use spaces instead of tabs
  vim.opt.tabstop        = 2                                    -- | Set tabstop and softtabstop to 2vim.opt.softtabstop = 2
  vim.opt.textwidth      = 0                                    -- | Disable auto break line
  vim.opt.wrap           = true                                 -- | Wrap lines
  vim.opt.wrapscan       = true                                 -- | Do not wrap around when search reaches the end of the file
  vim.opt.foldcolumn     = "0"                                  -- | Disable fold column by default
  vim.opt.foldmethod     = 'marker'                             -- | Use braces for fold method by default
  vim.opt.foldlevel      = 2                                    -- | Set the fold level to 2, fold all by default
  vim.opt.foldlevelstart = 99                                   -- | Set the start fold level to 99, for no folds closed

  vim.opt.backspace      = 'eol,start,indent'                   -- | Configure backspace
  vim.opt.whichwrap:append('<,>,h,l')                           -- | Allow specified keys to move the cursor left/right to move to the previous/next line
  vim.opt.diffopt        = 'internal,filler,vertical'           -- | Configure diff options
                                                                -- Invisible characters settings
  vim.opt.listchars      = { eol = '¬',
    tab                  = '>·',
    trail                = '~',
    extends              = '>',
    precedes             = '<',
    space                = '␣',
  }

  vim.opt.list           = false                                -- | Disable invisible characters display by default
  vim.opt.sessionoptions = 'curdir,folds,tabpages,winpos'       -- | Update session settings
  vim.opt.viewoptions    = 'cursor,folds'
  vim.opt.completeopt    = 'menu,menuone,noselect'              -- | Required by nvim-cmp (code completion plugin)
  vim.opt.jumpoptions:append('stack')                           -- | Change jump behavior like browser style stack



                                                                -- Turn persistent undo on
  vim.opt.undofile       = true                                 -- | Save undos after the file closes
  vim.opt.undodir        = vim.fn.stdpath('data') .. '/undodir' -- | Set the undo directory to $XDG_DATA_HOME/nvim-undodir
  vim.opt.undolevels     = 2000                                 -- | Set the maximum number of undos to 2000
  vim.opt.undoreload     = 20000                                -- | Set the number of lines to save for undo to 20000

                                                                -- Set the wildignore option to ignore certain file patterns
  vim.opt.wildignore:append({
    '*.o', '*.obj', '*.pyc', '*.class',                         -- Object files and compiled files
    'node_modules/', 'bower_components/', 'vendor/',            -- Directories
    '*.so', '*.dll', '*.dylib',                                 -- Shared libraries
    '*.swp', '*.swo', '*.swn',                                  -- Swap files
    '.git/', '.hg/', '.svn/',                                   -- Version control directories
    '.DS_Store', 'Thumbs.db',                                   -- System-specific files
  })

end

setup()
