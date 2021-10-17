-- nvim_tree variables should be set before `require`
vim.g.nvim_tree_hide_dotfiles = 1

local nvim_tree = require('nvim-tree')
local nest = require('nest')

nvim_tree.setup()

nest.applyKeymaps {
    { '<leader>n', '<cmd>NvimTreeToggle<cr>' },
}
