local telescope = require('telescope')
local telescope_builtin = require('telescope.builtin')
local nest = require('nest')

telescope.setup()

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

}
