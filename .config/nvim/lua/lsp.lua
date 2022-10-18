local lspconfig = require("lspconfig")
local saga = require("lspsaga")
local cmp = require("cmp")
local cmp_ultisnips_mappings = require("cmp_nvim_ultisnips.mappings")

cmp.setup({
    snippet = {
        expand = function(args)
            vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
        end,
    },
    window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ["<Tab>"] = cmp.mapping(
            function(fallback)
                cmp_ultisnips_mappings.expand_or_jump_forwards(fallback)
            end
        ),
    }),
    sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'ultisnips' }, -- For ultisnips users.
    }, {
        { name = 'buffer' },
    })
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
        { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
        { name = 'buffer' },
    })
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
        { name = 'buffer' }
    }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
        { name = 'path' }
    }, {
        { name = 'cmdline' }
    })
})

local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

local on_attach = function(client, bufnr)
    local opts = { noremap = true, silent = true }

    local function key(mode, keys, cmd)
        vim.api.nvim_buf_set_keymap(bufnr, mode, keys, cmd, opts)
    end

    local function opt(...)
        vim.api.nvim_buf_set_option(bufnr, ...)
    end

    -- Enable completion
    opt('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings
    key('n', '<leader>cD', '<cmd>lua vim.lsp.buf.declaration()<CR>')
    key('n', '<leader>cd', '<cmd>lua vim.lsp.buf.definition()<CR>')
    key('n', '<leader>cp', '<cmd>Lspsaga preview_definition<CR>')
    key('n', '<leader>ch', '<cmd>Lspsaga hover_doc<CR>')
    key('n', '<leader>ci', '<cmd>lua vim.lsp.buf.implementation()<CR>')
    key('n', '<leader>sh', '<cmd>Lspsaga signature_help<CR>')
    key('n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>')
    key('n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>')
    key('n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>')
    key('n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>')
    key('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>')
    key('n', '<leader>ca', '<cmd>Lspsaga code_action<CR>')
    key('n', '<leader>cr', '<cmd>lua vim.lsp.buf.references()<CR>')
    key('n', '<space>f', '<cmd>lua vim.lsp.buf.format()<CR>')
    key('n', '<leader>cs', '<cmd>lua vim.diagnostic.open_float()<CR>')
end

lspconfig.ccls.setup {
    on_attach = on_attach,
    capabilities = capabilities,
    init_options = {
        cache = {
            directory = vim.fn.expand("$HOME/.cache/ccls")
        }
    }
}

lspconfig.rust_analyzer.setup {
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
        ["rust-analyzer"] = {
            checkOnSave = {
                command = "clippy"
            },
            diagnostics = {
                disabled = {
                    "mismatched-arg-count"
                }
            }
        }
    }
}

lspconfig.tsserver.setup {
    on_attach = on_attach,
    capabilities = capabilities,
}

lspconfig.java_language_server.setup {
    on_attach = on_attach,
    capabilities = capabilities,
    cmd = { "/usr/share/java/java-language-server/lang_server_linux.sh" },
    root_dir = lspconfig.util.root_pattern('*.iml', '.idea', 'build.xml', 'pom.xml', 'settings.gradle',
        'settings.gradle.kts')
}

--lspconfig.jdtls.setup {
--    on_attach = on_attach,
--    capabilities = capabilities,
--    root_dir = lspconfig.util.root_pattern(
--        'build.xml',
--        'pom.xml',
--        'settings.gradle',
--        'settings.gradle.kts',
--        '*.iml',
--        '.idea'
--    )
--}

lspconfig.racket_langserver.setup {
    on_attach = on_attach,
    capabilities = capabilities,
    root_dir = lspconfig.util.root_pattern(
        'main.rkt'
    )
}

lspconfig.ocamllsp.setup {
    on_attach = on_attach,
    capabilities = capabilities,
}

lspconfig.pylsp.setup {
    on_attach = on_attach,
    capabilities = capabilities,
}

lspconfig.sumneko_lua.setup {
    on_attach = on_attach,
    capabilities = capabilities,
}

saga.init_lsp_saga()
