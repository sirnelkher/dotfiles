vim.cmd("set nocompatible")
vim.cmd("set showmatch")
vim.cmd("set ignorecase")
vim.cmd("set mouse=v")
vim.cmd("set hlsearch")
vim.cmd("set incsearch")
vim.cmd("set tabstop=2")
vim.cmd("set softtabstop=2")
vim.cmd("set expandtab")
vim.cmd("set shiftwidth=2")
vim.cmd("set autoindent")
vim.cmd("set number")
vim.cmd("set wildmode=longest,list")
-- vim.cmd("set cc=80")
vim.cmd("filetype plugin indent on")
vim.cmd("syntax on")
vim.cmd("set mouse=a")
vim.cmd("set clipboard=unnamedplus")
vim.cmd("filetype plugin on")
vim.cmd("set cursorline")
vim.cmd("set ttyfast")
--  vim.cmd("set spell")
--  vim.cmd("set noswapfile")
vim.cmd("set backupdir=~/.cache/nvim")

-- Statusline

-- Always show status line
vim.cmd("set laststatus=2")

-- "current time
-- set statusline=%{strftime(\"%H:%M\")}
-- "
-- set statusline+=\ %{hostname()}
-- "FAIL or OK depending on last shell cmd
-- set statusline+=\ \(%{v:shell_error?\"FAIL:\":\"ok:\"}
-- "exit code of last shell command
-- set statusline+=\%{v:shell_error}\)
-- "modified flag
-- set statusline+=\%m
-- "last 22 chars of file name
-- set statusline+=\ \%-.22F
-- "file type
-- set statusline+=%-y
-- "left-right separator
-- set statusline+=%=
-- "character encoding
-- set statusline+=%{strlen(&fenc)?&fenc:&enc}
-- "last operator
-- set statusline+=\ \[%{v:operator}\]
-- "current line and column
-- set statusline+=\ %5.l,%-3.c
-- "ASCII code of current char
-- set statusline+=x%02.B
-- "scroll position percentage
-- "
-- set statusline+=\ %3.p%%


vim.cmd("let leader='\'")
vim.cmd("nnoremap <leader>tt  <Esc>:tabnew<CR>")
vim.cmd("nnoremap <leader>tw  <Esc>:tabclose<CR>")
vim.cmd("nnoremap <leader>1 1gt")
vim.cmd("nnoremap <leader>2 2gt")
vim.cmd("nnoremap <leader>3 3gt")
vim.cmd("nnoremap <leader>4 4gt")
vim.cmd("nnoremap <leader>5 5gt")
vim.cmd("nnoremap <leader>6 6gt")
vim.cmd("nnoremap <leader>7 7gt")
vim.cmd("nnoremap <leader>8 8gt")
vim.cmd("nnoremap <leader>9 9gt")
vim.cmd("nnoremap <leader>0 10gt")

vim.cmd("set pastetoggle=<leader>pt")

-- Make constants readable on projector as well
vim.cmd("highlight Constant ctermbg=black ctermfg=green")

-- edit vim config in a split
vim.cmd("nnoremap <leader>evf :e $MYVIMRC<CR>")
-- reload vim confiv
vim.cmd("nnoremap <leader>rvf :so $MYVIMRC<CR>")


-- Use ctrl-[hjkl] to select the active split!
vim.cmd("nmap <silent> <c-l> :wincmd l<CR>")
vim.cmd("nmap <silent> <c-k> :wincmd k<CR>")
vim.cmd("nmap <silent> <c-j> :wincmd j<CR>")
vim.cmd("nmap <silent> <c-h> :wincmd h<CR>")


-- Make frequent typos work.
vim.cmd("command! Q :q")
vim.cmd("command! Qall :qall")
vim.cmd("command! QAll :qall")
vim.cmd("command! W :w")
vim.cmd("command! Wq :wq")
vim.cmd("command! WQ :wq")
vim.cmd("command! Wqall :wqall")
vim.cmd("command! WQall :wqall")
vim.cmd("command! WQAll :wqall")


-- Install lazy.nvim plugin manager
require("config/lazy")

vim.cmd([[colorscheme monokai-pro-classic]])
local builtin = require("telescope.builtin")

local config = require("treesitter.configs")
config.setup({
  ensure_installed = { "lua", "javascript", "typescript" },
  highlight = { enable = true },
  indent = { enable = true},
})

--  Plug 'itchyny/lightline.vim'
--  Plug 'loctvl842/monokai-pro.nvim'
--  Plug 'nvim/nvim-lspconfig'
--  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
--  Plug 'nvim-lua/plenary.nvim'
--  Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.x' }
--  Plug 'nvim-tree/nvim-web-devicons'

-- Telescope config
-- Find files using Telescope command-line sugar.
-- nnoremap <leader>ff <cmd>Telescope find_files<cr>
-- nnoremap <leader>fg <cmd>Telescope live_grep<cr>
-- nnoremap <leader>fb <cmd>Telescope buffers<cr>
-- nnoremap <leader>fh <cmd>Telescope help_tags<cr>

-- Using Lua functions
-- "nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files()<cr>
-- "nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>
-- "nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>
-- "nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>
