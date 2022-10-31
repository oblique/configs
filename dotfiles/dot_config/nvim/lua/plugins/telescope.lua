local telescope = require('telescope')
local telescope_builtin = require('telescope.builtin')
local telescope_actions = require('telescope.actions')
local nest = require('nest')

telescope.setup {
    defaults = {
        mappings = {
            i = {
                ['<C-Up>'] = telescope_actions.preview_scrolling_up,
                ['<C-Down>'] = telescope_actions.preview_scrolling_down,
            },
            n = {
                ['<C-Up>'] = telescope_actions.preview_scrolling_up,
                ['<C-Down>'] = telescope_actions.preview_scrolling_down,
            },
        }
    }
}

nest.applyKeymaps {
    {
        '<leader>t', {
            { 'm', telescope_builtin.builtin },
            { 'r', telescope_builtin.resume },
            { 'b', telescope_builtin.buffers },
            { 'f', telescope_builtin.find_files },
            { 'g', telescope_builtin.grep_string },
            { 'l', telescope_builtin.live_grep },
        }
    },
    {
        'g', {
            { 'd', telescope_builtin.lsp_definitions },
            { 't', telescope_builtin.lsp_type_definitions },
            { 'i', telescope_builtin.lsp_implementations },
            { 'r', telescope_builtin.lsp_references },
        }
    },
    { 'z=', telescope_builtin.spell_suggest },
}
