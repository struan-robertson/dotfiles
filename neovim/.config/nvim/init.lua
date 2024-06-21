-- Basic options

-- Show line numbers
vim.o.number = true         
-- Relative line numbers
vim.o.relativenumber = true 
-- Use spaces instead of tabs
vim.o.expandtab = true
-- Number of spaces to use for each indentation
vim.o.shiftwidth = 4
-- Number of spaces that a <Tab> counts for
vim.o.tabstop = 4
-- Dont show current pressed command
vim.o.showcmd = false

require("plugins")

