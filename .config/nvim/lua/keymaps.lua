local map = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

-- set leader to space (virgin comma leader < chad space leader)
map('n', '<Space>', '', {})
vim.g.mapleader = ' ' 

-- map `Y` to copy to end of line
-- conistent with the behaviour of `C` and `D`
map('n', 'Y', 'y$',               opts)
map('v', 'Y', '<Esc>y$gv',        opts)

-- saved you a click when switching between windows
map('n', '<C-h>', '<C-w>h', opts)
map('n', '<C-j>', '<C-w>j', opts)
map('n', '<C-k>', '<C-w>k', opts)
map('n', '<C-l>', '<C-w>l', opts)

-- tpope is the GOAT
map('n', '<leader>c', ':Commentary<CR>', opts)
map('v', '<', '<gv', opts)
map('v', '>', '>gv', opts)

map('n', '<leader>f', ':NvimTreeToggle<CR>', opts)
map('n', '<leader>z', ':ZenMode<CR>', opts)


-- Barbar.nvim mappings
--
-- Move to previous/next
map('n', '<A-,>', ':BufferPrevious<CR>', opts)
map('n', '<A-.>', ':BufferNext<CR>', opts)
-- Close buffer
map('n', '<A-q>', ':BufferClose<CR>', opts)
