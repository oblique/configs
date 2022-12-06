-- Useful keybindings (check `:help nvim-tree-default-mappings` for more)
--
--  g?      toggle help
--  <C-]>   cd
--  <Tab>   preview
--  I       toggle git ignore
--  H       toggle dotfiles
--  a       create a file at directory
--  d       rm a file
--  r       rename a file
--  -       dir up
--  m       mark/unmark

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
