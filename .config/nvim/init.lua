-- https://github.com/priime0/dotfiles

-- given a key and value, set nvim's global settings of key to value
local function opt(key, value)
    vim.o[key] = value
end

-- line numbers
opt("number", true)
opt("relativenumber", true)

-- tabs
opt("expandtab", true)
opt("tabstop", 4)
opt("shiftwidth", 4)
opt("smarttab", true)

-- indentation
opt("smartindent", true)
opt("autoindent", true)

-- misc
opt("termguicolors", true)
opt("scrolloff", 1)
opt("cursorline", true)
opt("showcmd", true)
opt("inccommand", "split")
opt("updatetime", 100)
opt("mouse", "a")

vim.g.mapleader = " "
vim.g.copilot_no_tab_map = true
vim.g.maplocalleader = ","

require("plugins")
require("mappings")
require("treesitter")
require("completion")
require("lsp")
require("git")
require("neovide")
