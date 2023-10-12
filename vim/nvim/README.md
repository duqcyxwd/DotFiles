# Code organization


## Lua

```shell
 ./
├──  config/               | Code config will be load manually
│  ├──  autocmds.lua       | cmd
│  ├──  diagnostic.lua     | Diagnostic
│  ├──  keys.lua           | keymap
│  ├──  lazy.lua           | original lazy style loading
│  ├──  lspconfig-util.lua | lsp related config
│  └──  options.lua        | nvim options
├──  funcs/                | Utility method, used by require
│  ├──  autocmds.lua
│  ├──  bracket_jump.lua
│  ├──  global.lua
│  ├──  utility.lua
│  ├──  nvim_core.lua
│  └──  nvim_utility.lua
│  └──  plug.lua           | A module shared by all plugins
├──  plugins/              | Plugins will be auto load by lazy
│  ├──  cmp.lua
│  ├──  editor.lua
│  ├──  filetype.lua
│  ├──  fzf.lua
│  ├──  git.lua
│  ├──  lsp.lua
│  ├──  mist.lua
│  ├──  plugins.lua
│  ├──  treesitter.lua
│  ├──  ui.lua
│  ├──  window.lua
│  └──  wip.lua
└──  stylua.toml
```

nvim stuff config. e.g diagnostic
- functions
    - lua-util: join/add/merge
    - Global Loder: Load file, load folder
    - toggles: nvim add-ons
    - Standalone Fn: Open a link/page, go to fist float
    - Plugin Enhancement : MyFzfLua search
    - Command: CMD CDC
    - My fucntions written, e.g: bracket_jump

Snippet: format select text with jq
File Type:
- Markdown: config
- Some of them require autogroup

" Visual mode pressing * or # searches for the current selection
Method to support buffer

Delete next Object

Mapping toggle

Plugins config
editorL turn ------------------------------------------------------------------------------
Highlight
