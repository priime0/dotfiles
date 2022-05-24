local packer = require("packer")
local use = packer.use

packer.startup(function()
    -- plugin/package manager
    use("wbthomason/packer.nvim")

    -- lib
    use("nvim-lua/plenary.nvim")

    -- language
    use("nvim-treesitter/nvim-treesitter")
    use("neovim/nvim-lspconfig")
    use("j-hui/fidget.nvim")
    use("onsails/lspkind-nvim")

    -- ui
    use("ayu-theme/ayu-vim")
    use("nvim-lua/popup.nvim")
    use("nvim-telescope/telescope.nvim")
    use("lewis6991/gitsigns.nvim")
    use("kyazdani42/nvim-web-devicons")
    use("kyazdani42/nvim-tree.lua")
    use("nvim-lualine/lualine.nvim")
    use("akinsho/toggleterm.nvim")

    -- completion
    use{"ms-jpq/coq_nvim",branch='coq'}
    
    -- misc
    use("davidgranstrom/nvim-markdown-preview")
    use("folke/todo-comments.nvim")

    if packer_bootstrap then
      require('packer').sync()
    end
end)

-- fidget
require("fidget").setup {}

-- ayu
vim.cmd([[let ayucolor="dark"]])
vim.cmd([[colorscheme ayu]])

-- telescope
require("telescope").setup {
    defaults = {
        mappings = {
            i = {
                ["<esc>"] = require("telescope.actions").close
            },
        },
        pickers = {
            hidden = true,
            file_ignore_patterns = { ".git/" }
        }
    }
}

-- git signs
require('gitsigns').setup {
    signs = {
        add          = {hl = 'GitSignsAdd'   , text = '│', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
        change       = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
        delete       = {hl = 'GitSignsDelete', text = '│', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
        topdelete    = {hl = 'GitSignsDelete', text = '│', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
        changedelete = {hl = 'GitSignsChange', text = '│', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    },
    signcolumn = true,
    numhl = false,
    linehl = false,
    word_diff = false,
    watch_gitdir = {
        interval = 1500,
        follow_files = true
    },
}

-- nvim-web-devicons
require('nvim-web-devicons').setup {}

-- nvim-tree
require('nvim-tree').setup {
    view = {
        width = 35,
        signcolumn = "no",
    },
    diagnostics = {
        enable = true,
    },
    git = {
        enable = true,
        ignore = true,
        timeout = 1500,
    }
}

-- lualine
require('lualine').setup {
    options = {
        theme = 'ayu_dark'
    },
}

-- toggleterm
require('toggleterm').setup {
    open_mapping = nil
}

-- markdown preview
vim.cmd[[
    let g:mkdp_browser = 'surf'
]]

-- todo-comments
require('todo-comments').setup {}
