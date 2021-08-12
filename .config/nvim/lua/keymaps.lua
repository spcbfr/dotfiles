local map = vim.api.nvim_set_keymap

-- set leader to space (virgin comma leader < chad space leader)
map('n', '<Space>', '', {})
vim.g.mapleader = ' ' 

-- map `Y` to copy to end of line
-- conistent with the behaviour of `C` and `D`
map('n', 'Y', 'y$',               { noremap = true })
map('v', 'Y', '<Esc>y$gv',        { noremap = true })

-- saved you a click when switching between windows
map('n', '<C-h>', '<C-w>h', { noremap = true })
map('n', '<C-j>', '<C-w>j', { noremap = true })
map('n', '<C-k>', '<C-w>k', { noremap = true })
map('n', '<C-l>', '<C-w>l', { noremap = true })

-- open NERDTree if closed, focus if open
map('n', '<leader>n', ':NERDTreeFocus<CR>', { noremap = true })

-- tpope is the GOAT
map('n', '<leader>c', ':Commentary<CR>', { noremap = true })
map('v', '<', '<gv', {noremap = true})
map('v', '>', '>gv', {noremap = true})
