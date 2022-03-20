filetype plugin indent on
set tabstop=4
set shiftwidth=4
set scrolloff=1
set expandtab
set smartindent
set autoindent
set cursorline
set number
set rnu
set nowrap
set inccommand=split
set updatetime=100
set mouse+=a

" File settings
au BufReadPost,BufNewFile *.md setlocal tw=80 | PencilHard | nnoremap <C-c> :MarkdownPreview<CR>
au BufReadPost,BufNewFile *.tex nnoremap <C-c> :w<CR>:!pdflatex main.tex<CR>
au BufReadPost,BufNewFile *.md,*.tex setlocal spell | set spelllang=en_US | inoremap <C-l> <c-g>u<ESC>[s1z=`]a<c-g>u
au BufReadPost,BufNewFile *.js,*.ts set sw=2 | set ts=2

" nnoremap : ;
" nnoremap ; :
nnoremap <space> :
tnoremap <silent> <ESC> <C-\><C-n>
nmap <C-z>z :Goyo 80<CR>
nmap <C-z>q :Goyo!<CR>
nnoremap <silent> <C-p> :RainbowToggle<CR>
nnoremap <silent> <C-q> <C-w>s<C-w>j:resize 20<CR>:terminal<CR><S-i>

" Telescope
nnoremap <C-t> :Telescope find_files<CR>
nnoremap <C-h>d :Telescope live_grep<CR>
nnoremap <C-h>u :Telescope lsp_definitions<CR>
nnoremap <C-h>e :Telescope<CR>git_
nnoremap <C-h>h :Telescope<CR>

" lsp
nnoremap <C-x>s :lua vim.lsp.diagnostic.show_line_diagnostics()<CR>
nnoremap <C-x>h :lua vim.lsp.buf.hover()<CR>
nnoremap <C-x>c :lua vim.lsp.buf.code_action()<CR>
nnoremap <C-x>r :lua vim.lsp.buf.rename()<CR>

" Tree
nnoremap <C-s> :NvimTreeToggle<CR>

" Barbar
nnoremap <silent> J :BufferPrevious<CR>
nnoremap <silent> K :BufferNext<CR>
nnoremap <silent> <C-j> :BufferMovePrevious<CR>
nnoremap <silent> <C-k> :BufferMoveNext<CR>
nnoremap <silent> <C-b>q :BufferClose<CR>

" Git Signs
nnoremap <silent> <C-n>b :lua require"gitsigns".blame_line()<CR>

call plug#begin('~/.config/nvim/plugged')

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'ayu-theme/ayu-vim'
Plug 'lewis6991/gitsigns.nvim', {'branch': 'main'}
Plug 'vim-airline/vim-airline'
Plug 'mattn/emmet-vim'
Plug 'reedes/vim-pencil'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'camspiers/animate.vim'
Plug 'Yggdroot/indentLine'
Plug 'frazrepo/vim-rainbow'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'kyazdani42/nvim-tree.lua'
Plug 'romgrk/barbar.nvim'
Plug 'elkowar/yuck.vim'

Plug 'SirVer/ultisnips'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && yarn install' }

call plug#end()

" VimTex
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
set conceallevel=0

" Ultisnips
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'
let g:UltiSnipsSnippetDirectories=[$HOME.'/.config/nvim/UltiSnips']

" Markdown Preview
let g:mkdp_auto_close = 0
let g:mkdp_browser = 'surf'

" Compe
let g:compe = {}
let g:compe.enabled = v:true
let g:compe.autocomplete = v:true
let g:compe.debug = v:false
let g:compe.min_length = 1
let g:compe.preselect = 'enable'
let g:compe.throttle_time = 80
let g:compe.source_timeout = 200
let g:compe.incomplete_delay = 400
let g:compe.max_abbr_width = 100
let g:compe.max_kind_width = 100
let g:compe.max_menu_width = 100
let g:compe.documentation = v:true

let g:compe.source = {}
let g:compe.source.path = v:true
let g:compe.source.buffer = v:true
let g:compe.source.calc = v:true
let g:compe.source.nvim_lsp = v:true
let g:compe.source.nvim_lua = v:true
let g:compe.source.vsnip = v:true
let g:compe.source.ultisnips = v:true

" gitsigns
lua << EOF
require('gitsigns').setup {
    signs = {
        add          = {hl = 'GitSignsAdd',    text = '+', numhl='GitSignsAddNr', linehl='GitSignsAddLn' },
        change       = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn' },
        delete       = {hl = 'GitSignsDelete', text = '-', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn' },
        topdelete    = {hl = 'GitSignsDelete', text = '-', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn' },
        changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn' },
    }
}
EOF

" indentLine
let g:indentLine_char = '⎸'
let g:indentLine_setConceal = 0

" Tree
let g:nvim_tree_width = 35

lua << EOF
require'lspconfig'.rust_analyzer.setup{}
EOF

lua << EOF
require'lspconfig'.ccls.setup{
    init_options = {
        cache = {
            directory = "/home/priime/.cache/ccls/";
        }
    }
}
EOF

lua << EOF
require'lspconfig'.pyls.setup{}
EOF

lua << EOF
require'lspconfig'.tsserver.setup{}
EOF

lua << EOF
require'lspconfig'.java_language_server.setup{
    cmd = { "/usr/share/java/java-language-server/lang_server_linux.sh" }
}
EOF

lua << EOF
require 'nvim-treesitter.configs'.setup {
    highlight = {
        enable = true,
    },
}
EOF

lua << EOF
require'lspconfig'.vuels.setup{
  on_attach = function(client)
    client.resolved_capabilities.document_formatting = true
  end;
}
EOF

lua << EOF
require'lspconfig'.racket_langserver.setup{}
EOF

fun! Start()
    " Don't run if: we have commandline arguments, we don't have an empty
    " buffer, if we've not invoked as vim or gvim, or if we'e start in insert mode
    if argc() || line2byte('$') != -1 || v:progname !~? '^[-gmnq]\=vim\=x\=\%[\.exe]$' || &insertmode
        return
    endif

    " Start a new buffer ...
    enew

    " ... and set some options for it
    setlocal
        \ bufhidden=wipe
        \ buftype=nofile
        \ nobuflisted
        \ nocursorcolumn
        \ nocursorline
        \ nolist
        \ nonumber
        \ noswapfile
        \ norelativenumber

    " Now we can just write to the buffer, whatever you want.
    call append('$', "")
    for line in split(system('cat /home/priime/.config/nvim/startscreen.vimstart'), '\n')
        call append('$', '' . l:line)
    endfor

    " No modifications to this buffer
    setlocal nomodifiable nomodified

    " When we go to insert mode start a new buffer, and start insert
    nnoremap <buffer><silent> e :enew<CR>
    nnoremap <buffer><silent> i :enew <bar> startinsert<CR>
    nnoremap <buffer><silent> o :enew <bar> startinsert<CR>

    " simple
    " set syntax=erlang
    " red leaves
    " set syntax=html
    " blue leaves
    set syntax=yaml
endfun

" Run after "doing all the startup stuff"
autocmd VimEnter * call Start()


syntax on
set termguicolors
set t_Co=256
let ayucolor="dark"
colorscheme ayu
