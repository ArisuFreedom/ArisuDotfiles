-- =========================================
--  ENCODING
-- =========================================
vim.o.encoding = 'utf-8'
vim.o.fileencoding = 'utf-8'
vim.o.fileencodings = 'utf-8'

-- =========================================
--  UI
-- =========================================
vim.cmd('syntax on')
vim.o.number = true
vim.o.relativenumber = true
vim.o.cursorline = true
vim.o.title = true

vim.o.termguicolors = true
vim.opt.fillchars = { vert = '|', fold = '-', eob = ' ', lastline = '@' }
vim.opt.listchars = { tab = '>-', space = '.', trail = '~', eol = '$' }

-- =========================================
--  CLIPBOARD
-- =========================================
vim.o.clipboard = 'unnamedplus'

-- =========================================
--  INDENT
-- =========================================
vim.o.autoindent = true
vim.o.smartindent = true
vim.o.expandtab = true
vim.o.tabstop = 4
vim.o.softtabstop = 4
vim.o.shiftwidth = 4

-- =========================================
--  SEARCH
-- =========================================
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.incsearch = true
vim.o.hlsearch = false
vim.g['/'] = "" -- Limpa a busca

-- =========================================
--  COMPLETION / MENUS
-- =========================================
vim.opt.complete:append("kspell")
vim.opt.completeopt = { 'menuone', 'longest' }
vim.opt.wildmenu = true
vim.opt.wildmode = { 'longest', 'full' }
vim.opt.wildoptions = 'pum'
vim.opt.shortmess:append('c')

-- =========================================
--  SCROLL
-- =========================================
vim.o.scrolloff = 2

-- =========================================
--  BUFFERS / SPLITS
-- =========================================
vim.o.splitbelow = true
vim.o.splitright = true

-- =========================================
--  FILES / BACKUPS
-- =========================================
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.backspace = { 'indent', 'eol', 'start' }
vim.opt.path = { '.', '**' }

vim.o.undofile = true
vim.o.undodir = '/home/arch/.local/state/nvim/undo'

-- =========================================
--  RELATIVE NUMBERS IN INSERT MODE
-- =========================================
vim.api.nvim_create_augroup("relative_numbers", { clear = true })
vim.api.nvim_create_autocmd("InsertEnter", {
  group = "relative_numbers",
  command = "set norelativenumber",
})
vim.api.nvim_create_autocmd("InsertLeave", {
  group = "relative_numbers",
  command = "set relativenumber",
})

-- =========================================
--  RANDOM
-- =========================================
vim.o.errorbells = false
vim.o.visualbell = true
vim.cmd('filetype plugin indent on')

-- =========================================
--  REMEMBER LAST POSITION
-- =========================================
vim.api.nvim_create_augroup("last_position", { clear = true })
vim.api.nvim_create_autocmd("BufReadPost", {
  group = "last_position",
  command = "if line(\"'\\\"\") > 0 && line(\"'\\\"\") <= line(\"$\") | exe \"normal! g`\\\"\" | endif",
})

vim.api.nvim_create_autocmd("BufWritePre", {
  group = "last_position",
  command = "if &ft != 'markdown' | %s/\\s\\+$//e | endif",
})

-- =========================================
--  NEW FILES IN INSERT MODE
-- =========================================
vim.api.nvim_create_autocmd("BufNewFile", {
  command = "startinsert",
})

-- =========================================
--  INSERT MODE WITHOUT A FILE NAME
-- =========================================
vim.api.nvim_create_autocmd("VimEnter", {
  command = "if empty(expand('%')) | startinsert | endif",
})

-- =========================================
--  INSERT MODE START IN EMPTY FILES
-- =========================================
-- vim.api.nvim_create_autocmd("VimEnter", {
--   command = "if getfsize(expand('%')) == 0 | startinsert | endif",
-- })

-- =========================================
--  KEYBINDS
-- =========================================
-- Alias for easy mapping
local map = vim.keymap.set

-- Leader key
vim.g.mapleader = " "

-- Source Current file
map('n', '<leader>o', '<Cmd>update<CR> :source<CR>', { desc = "Save and source current file" })

-- Save current file
map('n', '<leader>w', '<Cmd>update<CR>', { desc = "Save current file" })

-- ; instead of :
map({'n', 'v', 'x' }, ';', ':', { noremap = true })
map({'n', 'v', 'x' }, ':', ';', { noremap = true })

-- Norm biding
map({ 'n', 'v' }, '<leader>n', ':norm')

-- System Clipboard
map({ 'n', 'v' }, '<leader>y', '"+y')
map({ 'n', 'v' }, '<leader>d', '"+d')

-- =========================================
--  PLUGINS
-- =========================================
-- Colorscheme
vim.cmd("colorscheme vague")
