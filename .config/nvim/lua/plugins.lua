local packer = require("packer")
local use = packer.use

packer.startup(function()
    -- plugin/package manager
    use("wbthomason/packer.nvim")

    -- lib
    use("nvim-lua/plenary.nvim")

    -- setup
    use("Olical/aniseed")

    -- language
    use("nvim-treesitter/nvim-treesitter")
    use("neovim/nvim-lspconfig")
    use("j-hui/fidget.nvim")
    use("onsails/lspkind-nvim")
    use("wlangstroth/vim-racket")
    use("Olical/conjure")
    use("yioneko/nvim-yati")

    -- ui
    use("ayu-theme/ayu-vim")
    use("nvim-lua/popup.nvim")
    use("nvim-telescope/telescope.nvim")
    use("lewis6991/gitsigns.nvim")
    use("kyazdani42/nvim-web-devicons")
    use("kyazdani42/nvim-tree.lua")
    use("nvim-lualine/lualine.nvim")
    use{"akinsho/toggleterm.nvim",branch="main"}
    use("simrat39/symbols-outline.nvim")
    use("lcheylus/overlength.nvim")
    use("akinsho/bufferline.nvim")
    use("folke/trouble.nvim")

    -- completion
    use("hrsh7th/cmp-nvim-lsp")
    use("hrsh7th/cmp-buffer")
    use("hrsh7th/cmp-path")
    use("hrsh7th/cmp-cmdline")
    use("hrsh7th/nvim-cmp")
    use{"glepnir/lspsaga.nvim",branch="main"}
    use("github/copilot.vim")
    use("SirVer/ultisnips")
    use("quangnguyen30192/cmp-nvim-ultisnips")

    -- misc
    use("davidgranstrom/nvim-markdown-preview")
    use("folke/todo-comments.nvim")
    use("lervag/vimtex")
    use("TimUntersberger/neogit")
    use("ggandor/leap.nvim")
    use("eraserhd/parinfer-rust")
    use("windwp/nvim-autopairs")
    use("mhartington/formatter.nvim")
    use{"kylechui/nvim-surround",branch="main"}
    use("numToStr/Comment.nvim")

    if packer_bootstrap then
      require('packer').sync()
    end
end)

-- vimtex
vim.cmd("let g:vimtex_compiler_progname = 'nvr'")
require("nvim-treesitter.configs").setup {
    ignore_install = { "latex" },
    highlight = {
        disable = {"latex"},
    }
}

-- fidget
require("fidget").setup {}

-- ayu
vim.cmd([[let ayucolor="light"]])
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

-- copilot
vim.g.copilot_filetypes = {
    ['*'] = false,
    ['java'] = true,
}

-- symbols-outline
require('symbols-outline').setup {}

-- leap
require('leap').setup {}

-- overlength
require('overlength').setup {}

-- formatter
require('formatter').setup {
    filetype = {
        rust = {
            -- Rustfmt
            function()
                return {
                    exe = "rustfmt",
                    args = {"--emit=stdout", "--edition=2021"},
                    stdin = true
                }
            end
        },
        c = {
            require('formatter.filetypes.c').clangformat
        },
        cpp = {
            require('formatter.filetypes.cpp').clangformat
        },
        ocaml = {
            require('formatter.filetypes.ocaml').ocamlformat
        },
        python = {
            require('formatter.filetypes.python').black
        },
        javascript = {
            require('formatter.filetypes.javascript').prettier
        },
        tex = {
            require('formatter.filetypes.latex').latexindent
        },
        -- apply to all filetypes
        ["*"] = {
            require('formatter.filetypes.any').remove_trailing_whitespace
        },
    }
}

-- bufferline
require('bufferline').setup {}

-- nvim surround
require('nvim-surround').setup {}

-- Comment
require('Comment').setup {}

-- autopairs
require("nvim-autopairs").setup {
    disable_filetype = { "TelescopePrompt", "racket" }
}

-- trouple
require("trouble").setup {}
