local nvim_tree = require('nvim-tree')
local nest = require('nest')

nvim_tree.setup {
    filters = {
        dotfiles = true,
    }
}

nest.applyKeymaps {
    { '<leader>n', '<cmd>NvimTreeToggle<cr>' },
}
