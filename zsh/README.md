# ZSH config

Script Directories

- *commands*
  - General scripts will be add to PATH
  - Standalone script should not impact zsh environment
  - TODO Rename it to scripts
- *functions* (ZSH functions)
  - ZSH scripts will be added in FPATH
  - those scripts will be evalated on use. (Lazy load)
  - Lazy load environment variables as well
  - 2022-03-18 I noticed zsh functions is faster than run a new script
  - I will try to move most of my scripts into functions
  - Functions can be used in other script once I add `autoload`
- *snippets*
  - Random snippets that can't be lazy load

TODO
- lib
  - Inlcude general functions/setup shared in plugins or functions
- plugins
  - standalone plugins


