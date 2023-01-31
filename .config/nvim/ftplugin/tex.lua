--  map the key (sequence) to a command
local function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

require("nvim-autopairs").setup {}

map("i", "<C-c>", "<ESC>:w<CR>:!just<CR>")
map("n", "<C-c>", "<ESC>:w<CR>:!just<CR>")

vim.cmd("inoremap <C-f> <Esc>: silent exec '.!inkscape-figures create \"'.getline('.').'\" \"'.b:vimtex.root.'/figures/\"'<CR><CR>:w<CR>")
vim.cmd("nnoremap <C-f> : silent exec '!inkscape-figures edit \"'.b:vimtex.root.'/figures/\" > /dev/null 2>&1 &'<CR><CR>:redraw!<CR>")
