-- Use `<C-w> w` to move between floating windows
-- Use `o` to open a file from lsp_finder

local lspsaga = require('lspsaga')
local nest = require('nest')

lspsaga.setup {
    diagnostic = {
        show_code_action = false,
    },
    lightbulb = {
        enable = false,
        virtual_text = false,
    }
}

nest.applyKeymaps {
    {
        '<leader>l', {
            { 'r', '<cmd>Lspsaga rename<cr>' },
        }
    }
}
