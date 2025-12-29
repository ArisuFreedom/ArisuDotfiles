-- =========================================
--  ENCODING
-- =========================================
vim.opt.encoding = 'utf-8'
vim.opt.fileencoding = 'utf-8'
vim.opt.fileencodings = 'utf-8'

-- =========================================
--  UI
-- =========================================
vim.cmd('syntax on')
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.opt.title = true

-- Preenchendo caracteres para separar janelas e mostrar o fim do buffer
vim.opt.fillchars = { vert = '|', fold = '-', eob = ' ', lastline = '@' }
vim.opt.listchars = { tab = '>-', space = '.', trail = '~', eol = '$' }

-- =========================================
--  CLIPBOARD
-- =========================================
vim.opt.clipboard = 'unnamedplus'

-- =========================================
--  INDENT
-- =========================================
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.expandtab = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4

-- =========================================
--  SEARCH
-- =========================================
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.incsearch = true
vim.opt.hlsearch = false
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
vim.opt.scrolloff = 2

-- =========================================
--  BUFFERS / SPLITS
-- =========================================
vim.opt.splitbelow = true
vim.opt.splitright = true

-- =========================================
--  FILES / BACKUPS
-- =========================================
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.backspace = { 'indent', 'eol', 'start' }
vim.opt.path = { '.', '**' }

vim.opt.undofile = true
vim.opt.undodir = '~/.vim/undo'

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
vim.opt.errorbells = false
vim.opt.visualbell = true
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
vim.api.nvim_create_autocmd("VimEnter", {
  command = "if getfsize(expand('%')) == 0 | startinsert | endif",
})
