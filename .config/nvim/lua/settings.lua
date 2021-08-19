-- variables to avoid repetition
local o = vim.o
local wo = vim.wo
local bo = vim.bo

-- obviously.
o.encoding = 'utf-8'
o.linebreak = true

-- better colors and stuff
o.termguicolors = true

-- I hate when text wraps
o.wrap = false

o.mouse = "a"

-- makes moving across the file easier
o.relativenumber = true
o.number = true

-- lualine replaces this
o.showmode = false

-- I cant manage multiple clipboards at once
o.clipboard = 'unnamedplus'
o.compatible = false

-- I dont want to manage buffers
o.hidden = true

-- make search !== pain
o.hlsearch = false
o.smartcase = true
