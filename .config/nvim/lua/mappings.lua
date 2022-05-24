--  map the key (sequence) to a command
local function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- open terminal
map("n", "<leader>ot", ":ToggleTerm<CR>")

-- exit terminal mode
map("t", "<esc>", "<C-\\><C-n>")

-- open tree
map("n", "<leader>op", ":NvimTreeToggle<CR>")

-- telescope
map("n", "<leader>to", ":Telescope<CR>")
map("n", "<leader>tf", ":Telescope find_files<CR>")
map("n", "<leader>tg", ":Telescope live_grep<CR>")
