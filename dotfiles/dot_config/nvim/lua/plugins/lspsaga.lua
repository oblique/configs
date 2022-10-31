-- Use `<C-w> w` to move between floating windows
-- Use `o` to open a file from lsp_finder

local lspsaga = require('lspsaga')
local lspsaga_rename = require('lspsaga.rename').rename
local nest = require('nest')

lspsaga.init_lsp_saga {
    diagnostic_header = nil,
    code_action_lightbulb = {
        enable = false,
        sign = true,
        virtual_text = false,
    },
}

nest.applyKeymaps {
    {
        '<leader>l', {
            { 'r', '<cmd>Lspsaga rename<cr>' },
        }
    }
}
