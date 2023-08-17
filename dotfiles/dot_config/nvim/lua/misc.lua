vim.opt.foldenable = false
vim.opt.number = true
vim.opt.cindent = true
vim.opt.cinoptions = { ':0', 'l1', 'g0', 't0', '(0' }
vim.opt.textwidth = 80
vim.opt.mouse:append('a')
vim.opt.splitbelow = true
vim.opt.hidden = true
vim.opt.list = true
vim.opt.listchars = { trail = '~', tab = '> ', nbsp = '␣' }
-- Give room for two-line messages instead of asking to press enter
vim.opt.cmdheight = 2
-- Use cursor provided by terminal
vim.opt.guicursor = ''

vim.cmd('syntax on')
vim.cmd('filetype plugin on')
vim.cmd('filetype indent on')
vim.cmd('colorscheme behelit')

-- Copy path to clipboard
function _G.copy_path()
    local path = vim.fn.expand('%:p')
    vim.fn.setreg('+', path)
    vim.fn.setreg('*', path)
end

-- Space helpers
function _G.enable_spaces()
    vim.opt.expandtab = true
    vim.opt.softtabstop = 4
    vim.opt.shiftwidth = 4
    vim.opt.tabstop = 4
end

function _G.enable_spaces2()
    vim.opt.expandtab = true
    vim.opt.softtabstop = 2
    vim.opt.shiftwidth = 2
    vim.opt.tabstop = 2
end

function _G.enable_tabs()
    vim.opt.expandtab = false
    vim.opt.softtabstop = 0
    vim.opt.shiftwidth = 8
    vim.opt.tabstop = 8
end

-- Enable spaces by default
_G.enable_spaces()

-- Commands
vim.cmd('command EnableSpaces call v:lua.enable_spaces()')
vim.cmd('command EnableSpaces2 call v:lua.enable_spaces2()')
vim.cmd('command EnableTabs call v:lua.enable_tabs()')
vim.cmd('command ShowPath echo expand(\'%:p\')')
vim.cmd('command CopyPath call v:lua.copy_path()')
