" neovim config


" Seperate my neovim with vim
" set runtimepath^=~/.vim runtimepath+=~/.vim/after
" let &packpath = &runtimepath
" Install https://github.com/junegunn/vim-plug
"sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
"       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

let vimrootPath = $XDG_CONFIG_HOME.'/vim/custom/*.vim'

for f in split(glob(vimrootPath), '\n')
  exe 'source' f
endfor


let luarootPath = $XDG_CONFIG_HOME.'/vim/lua/*.lua'

for f in split(glob(luarootPath), '\n')
  exe 'luafile' f
endfor

" https://github.com/glepnir/nvim-lua-guide-zh
" lua require('mapping')


" When I open a terminal buffer, I want it to feel like I'm in the terminal. I
" don't want to still be in normal mode.
autocmd TermOpen * startinsert


" set verbosefile=~/temp/log/vim.log
" set verbose=1
