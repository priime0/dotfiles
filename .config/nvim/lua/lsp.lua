local lspconfig = require('lspconfig')


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
    key('n', '<leader>ch', '<cmd>lua vim.lsp.buf.hover()<CR>')
    key('n', '<leader>ci', '<cmd>lua vim.lsp.buf.implementation()<CR>')
    key('n', '<leader>sh', '<cmd>lua vim.lsp.buf.signature_help()<CR>')
    key('n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>')
    key('n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>')
    key('n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>')
    key('n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>')
    key('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>')
    key('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>')
    key('n', '<leader>cr', '<cmd>lua vim.lsp.buf.references()<CR>')
    key('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>')
    key('n', '<leader>cs', '<cmd>lua vim.diagnostic.open_float()<CR>')
end

local coq = require('coq')

lspconfig.ccls.setup(coq.lsp_ensure_capabilities{
    on_attach = on_attach,
    capabilities = capabilities,
    init_options = {
        cache = {
            directory = vim.fn.expand("$HOME/.cache/ccls/")
        }
    }
})

lspconfig.rust_analyzer.setup(coq.lsp_ensure_capabilities{
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
})

lspconfig.tsserver.setup {
    on_attach = on_attach,
    capabilities = capabilities
}
lspconfig.tsserver.setup(coq.lsp_ensure_capabilities())

lspconfig.java_language_server.setup(coq.lsp_ensure_capabilities{
    on_attach = on_attach,
    capabilities = capabilities,
    cmd = { "/usr/share/java/java-language-server/lang_server_linux.sh" }
})

lspconfig.vuels.setup {
    on_attach = on_attach,
    capabilities = capabilities
}
lspconfig.vuels.setup(coq.lsp_ensure_capabilities())

lspconfig.racket_langserver.setup {}
