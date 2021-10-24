-- Use `<C-w> w` to move between floating windows
-- Use `o` to open a file from lsp_finder

local lspsaga = require('lspsaga')
local lspsaga_rename = require('lspsaga.rename').rename
local nest = require('nest')

lspsaga.init_lsp_saga {
    use_saga_diagnostic_sign = false,
    code_action_prompt = {
        enable = false,
        sign = true,
        virtual_text = false,
    },
    rename_prompt_prefix = '>',
}

nest.applyKeymaps {
    {
        '<leader>l', {
            { 'r', lspsaga_rename },
        }
    }
}
