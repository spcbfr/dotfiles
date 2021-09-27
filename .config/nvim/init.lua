require('settings')
require('tree')
require('plugins')
require('keymaps')
require('autocmd')

vim.cmd('colorscheme night-owl')

-- fern configuration
vim.g.nvim_tree_side = "left"
vim.g.nvim_tree_width = 30
vim.g.nvim_tree_auto_open = 1
