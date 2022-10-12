--  map the key (sequence) to a command
local function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- map("n", "<C-c>", "<ESC>:w<CR>:terminal<CR>racket -i -f " .. vim.fn.expand("%"))
map("n", "<C-c>", "<ESC>:w<CR>:ToggleTerm<CR>racket -i -f ")
