-- Use `<C-w> w` to move between floating windows
-- Use `o` to open a file from lsp_finder

local saga = require 'lspsaga'

saga.init_lsp_saga {
    use_saga_diagnostic_sign = false,
    code_action_prompt = {
        enable = true,
        sign = true,
        virtual_text = false,
    }
}

vim.cmd('command Rename Lspsaga rename')
