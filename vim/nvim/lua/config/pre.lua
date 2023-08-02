require("funcs.global")

-- Stuff needed to be load before others, sucn as highlight

-- terminal color / italics finagling, no need for Alacritty, need to check with other terminal
-- vim.cmd[[let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"]]
-- vim.cmd[[let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"]]

vim.cmd[[autocmd ColorScheme * highlight Comment cterm=italic gui=italic]]

