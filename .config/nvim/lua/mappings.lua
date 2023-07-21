--  map the key (sequence) to a command
local function map(mode, lhs, rhs, opts)
    local options = { noremap = true }
    if opts then
        options = vim.tbl_extend("force", options, opts)
    end
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- format
map('n', '<leader>f', ':w<CR>:Format<CR>')

-- open terminal
map("n", "<leader>ott", ":ToggleTerm<CR>")
map("n", "<leader>otf", ":ToggleTerm direction=float<CR>")
map("n", "<leader>otv", ":ToggleTerm direction=vertical<CR>")

-- exit terminal mode
map("t", "<esc>", "<C-\\><C-n>")

-- open tree
map("n", "<leader>op", ":NvimTreeToggle<CR>")

-- telescope
map("n", "<leader>to", ":Telescope<CR>")
map("n", "<leader>tf", ":Telescope find_files<CR>")
map("n", "<leader>tg", ":Telescope live_grep<CR>")

-- copilot
map('i', '<C-a>', 'copilot#Accept("<CR>")', {expr=true, silent=true})

-- neogit / git related
map('n', '<leader>gg', ':Neogit<CR>')
map('n', '<leader>gb', ':Gitsigns blame_line<CR>')
map('n', '<leader>gs', ':Gitsigns stage_hunk<CR>')

-- symbols outline
map("n", "<leader>os", ":SymbolsOutline<CR>")

-- leap
map("n", "<C-s>", ":lua require('leap').leap {}<CR>")
map("n", "<C-z>", ":lua require('leap').leap {  backward = true }<CR>")

-- pollen
map("i", "<A-d>", "◊")
map("i", "<A-l>", "λ")

-- colorscheme toggle
-- map("n", "<A-j>", ":let ayucolor=\"dark\"<CR>:colorscheme ayu<CR>")
-- map("n", "<A-k>", ":let ayucolor=\"light\"<CR>:colorscheme ayu<CR>")
map("n", "<A-j>", ":colorscheme catppuccin-mocha<CR>")
map("n", "<A-k>", ":colorscheme catppuccin-latte<CR>")

-- horizontal scrolling
map("n", "<C-l>", "zl")
map("n", "<C-h>", "zh")

-- bufferline
map("n", "<C-k>", ":BufferLineCycleNext<CR>")
map("n", "<C-j>", ":BufferLineCyclePrev<CR>")
map("n", "<C-S-k>", ":BufferLineMoveNext<CR>")
map("n", "<C-S-j>", ":BufferLineMovePrev<CR>")
map("n", "<leader>bd", ":bd<CR>")

-- trouble
map("n", "<leader>od", ":TroubleToggle<CR>")

-- workspaces
map("n", "<leader>pp", ":Telescope workspaces theme=dropdown<CR>")
map("n", "<leader>pa", ":WorkspacesAdd ")

-- relative line numbers
map("n", "<C-f>", ":set relativenumber!<CR>")
